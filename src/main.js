const { app, BrowserWindow, dialog, ipcMain, shell, Menu } = require('electron');
const { spawn, spawnSync } = require('node:child_process');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const readline = require('node:readline');

const APP_NAME = 'SawitHeight R';
let mainWindow = null;
let activeProcess = null;
let activeRun = null;
let heartbeatTimer = null;

function getResourcePath(...parts) {
  if (app.isPackaged) return path.join(process.resourcesPath, ...parts);
  return path.join(__dirname, '..', ...parts);
}
function getAppFile(...parts) {
  if (app.isPackaged) return path.join(app.getAppPath(), ...parts);
  return path.join(__dirname, '..', ...parts);
}
function getSettingsPath() { return path.join(app.getPath('userData'), 'settings.json'); }
function readSettings() { try { return JSON.parse(fs.readFileSync(getSettingsPath(), 'utf8')); } catch { return {}; } }
function writeSettings(settings) {
  fs.mkdirSync(path.dirname(getSettingsPath()), { recursive: true });
  fs.writeFileSync(getSettingsPath(), JSON.stringify(settings, null, 2), 'utf8');
}

function createWindow() {
  mainWindow = new BrowserWindow({
    width: 1540, height: 960, minWidth: 1180, minHeight: 760,
    show: false, backgroundColor: '#0b0f11', title: APP_NAME,
    icon: getAppFile('assets', 'icon.ico'), autoHideMenuBar: true,
    webPreferences: {
      preload: path.join(__dirname, 'preload.js'), contextIsolation: true,
      nodeIntegration: false, sandbox: true, devTools: !app.isPackaged
    }
  });
  mainWindow.loadFile(path.join(__dirname, 'renderer', 'index.html'));
  mainWindow.webContents.setWindowOpenHandler(({ url }) => {
    if (/^https?:\/\//i.test(url)) shell.openExternal(url);
    return { action: 'deny' };
  });
  mainWindow.once('ready-to-show', () => { mainWindow.show(); mainWindow.focus(); });
}

function emitToRenderer(event) {
  if (mainWindow && !mainWindow.isDestroyed()) mainWindow.webContents.send('analysis:event', event);
}
function normalizeExistingFile(filePath) {
  if (!filePath || typeof filePath !== 'string') return '';
  const resolved = path.resolve(filePath);
  return fs.existsSync(resolved) ? resolved : '';
}
function findRscriptCandidates() {
  const candidates = [];
  const bundled = getResourcePath('vendor', 'R', 'bin', 'Rscript.exe');
  if (fs.existsSync(bundled)) candidates.push(bundled);
  if (process.env.R_HOME) {
    candidates.push(path.join(process.env.R_HOME, 'bin', 'Rscript.exe'));
    candidates.push(path.join(process.env.R_HOME, 'bin', 'x64', 'Rscript.exe'));
  }
  const settings = readSettings();
  if (settings.rscriptPath) candidates.push(settings.rscriptPath);
  const roots = [
    process.env.ProgramFiles ? path.join(process.env.ProgramFiles, 'R') : null,
    process.env['ProgramFiles(x86)'] ? path.join(process.env['ProgramFiles(x86)'], 'R') : null,
    process.env.LOCALAPPDATA ? path.join(process.env.LOCALAPPDATA, 'Programs', 'R') : null
  ].filter(Boolean);
  for (const root of roots) {
    try {
      const dirs = fs.readdirSync(root, { withFileTypes: true })
        .filter((d) => d.isDirectory() && /^R-/i.test(d.name))
        .map((d) => d.name)
        .sort((a, b) => b.localeCompare(a, undefined, { numeric: true }));
      for (const dir of dirs) {
        candidates.push(path.join(root, dir, 'bin', 'Rscript.exe'));
        candidates.push(path.join(root, dir, 'bin', 'x64', 'Rscript.exe'));
      }
    } catch { /* optional */ }
  }
  try {
    const result = spawnSync('where', ['Rscript.exe'], { encoding: 'utf8', windowsHide: true });
    if (result.status === 0 && result.stdout) candidates.push(...result.stdout.split(/\r?\n/).filter(Boolean));
  } catch { /* optional */ }
  const seen = new Set();
  return candidates.map(normalizeExistingFile).filter((candidate) => {
    const key = candidate.toLowerCase();
    if (!candidate || seen.has(key)) return false;
    seen.add(key); return true;
  });
}
function getRscriptPath(requestedPath = '') {
  const requested = normalizeExistingFile(requestedPath);
  return requested || findRscriptCandidates()[0] || '';
}
function validPeriod(value) { return /^[A-Z0-9]{1,3}$/.test(String(value || '').trim().toUpperCase()); }
function fileExists(file) { return Boolean(file && typeof file === 'string' && fs.existsSync(file)); }
function positiveNumber(v) { return Number.isFinite(Number(v)) && Number(v) > 0; }

function basicPeriodValidation(config) {
  const errors = [];
  if (!config || typeof config !== 'object') return ['Konfigurasi periode tidak valid.'];
  const i = config.inputs || {}; const p = config.parameters || {};
  if (!fileExists(i.point_cloud) || !/\.(las|laz)$/i.test(i.point_cloud || '')) errors.push('Point cloud periode harus LAS/LAZ yang valid.');
  if (!fileExists(i.tree_points) || !/\.(shp|gpkg|geojson|json)$/i.test(i.tree_points || '')) errors.push('Titik pokok harus SHP/GPKG/GeoJSON yang valid.');
  if (!validPeriod(p.period_code)) errors.push('Kode periode wajib 1-3 karakter, contoh P5 atau D2.');
  if (!String(p.tree_id_field || '').trim()) errors.push('Field TREE_ID wajib diisi.');
  if (!p.xy_alignment_confirmed) errors.push('Konfirmasi bahwa TREE_ID dan point cloud sudah berada pada frame XY yang benar.');
  const terrain = String(p.terrain_mode || 'CSF_TIN').toUpperCase();
  if (!['CSF_TIN', 'EXTERNAL_DTM'].includes(terrain)) errors.push('Terrain mode harus CSF_TIN atau EXTERNAL_DTM.');
  if (terrain === 'EXTERNAL_DTM') {
    if (!fileExists(i.external_dtm) || !/\.(tif|tiff)$/i.test(i.external_dtm || '')) errors.push('External DTM harus GeoTIFF yang valid.');
  }
  if (!i.output_root) errors.push('Folder output belum dipilih.');
  const positive = [
    ['sor_k','SOR k'],['sor_m','SOR m'],['csf_cloth_resolution','CSF cloth resolution'],
    ['csf_class_threshold','CSF class threshold'],['dtm_resolution_m','Resolusi DTM'],
    ['buffer_radius_m','Radius buffer'],['min_veg_h_m','Minimum vegetasi'],['metrics_chunk_size','Chunk metrics'],
    ['chm_resolution_m','Resolusi nCHM'],['threads','Thread CPU']
  ];
  for (const [key,label] of positive) if (!positiveNumber(p[key])) errors.push(`${label} harus > 0.`);
  if (!Number.isFinite(Number(p.min_normalized_z_m))) errors.push('Minimum normalized Z harus angka.');
  if (![1,2,3].includes(Number(p.csf_rigidness))) errors.push('CSF rigidness hanya 1, 2, atau 3.');
  const frac = Number(p.holdout_train_frac);
  if (!Number.isFinite(frac) || frac <= 0 || frac >= 1) errors.push('Hold-out train fraction harus >0 dan <1.');
  const minGround = Number(p.holdout_min_ground);
  if (!Number.isInteger(minGround) || minGround < 20) errors.push('Minimum ground hold-out minimal 20.');
  const maxGround = Number(p.holdout_max_ground || 0);
  if (!Number.isFinite(maxGround) || maxGround < 0) errors.push('Maximum ground hold-out harus 0 atau angka positif.');
  const epsg = Number(p.fallback_epsg || 0);
  if (epsg && (!Number.isInteger(epsg) || epsg <= 0)) errors.push('EPSG fallback harus bilangan bulat positif.');
  return errors;
}


function basicRegistrationValidation(config) {
  const errors = [];
  if (!config || typeof config !== 'object') return ['Konfigurasi registrasi tidak valid.'];
  const i = config.inputs || {}; const p = config.parameters || {};
  const lasCheck = (value, label) => { if (!fileExists(value) || !/\.(las|laz)$/i.test(value || '')) errors.push(`${label} harus LAS/LAZ yang valid.`); };
  const controlCheck = (value, label) => { if (!fileExists(value) || !/\.(csv|shp|gpkg|geojson|json)$/i.test(value || '')) errors.push(`${label} harus CSV/SHP/GPKG/GeoJSON yang valid.`); };
  lasCheck(i.target_point_cloud, 'Target point cloud');
  lasCheck(i.master_point_cloud, 'Master point cloud');
  controlCheck(i.target_controls, 'Target control points');
  controlCheck(i.master_controls, 'Master control points');
  if (!i.output_root) errors.push('Folder output registrasi belum dipilih.');
  if (!validPeriod(p.target_period_code)) errors.push('Kode target period wajib 1-3 karakter.');
  if (!validPeriod(p.master_period_code)) errors.push('Kode master period wajib 1-3 karakter.');
  const model = String(p.transform_model || '').toUpperCase();
  if (!['TRANSLATION_3D','RIGID_3D','SIMILARITY_3D'].includes(model)) errors.push('Transform model tidak valid.');
  for (const [key,label] of [['control_id_field','Control ID field'],['control_x_field','Control X field'],['control_y_field','Control Y field'],['control_z_field','Control Z field']]) {
    if (!String(p[key] || '').trim()) errors.push(`${label} wajib diisi.`);
  }
  const chunk = Number(p.las_chunk_size);
  if (!Number.isInteger(chunk) || chunk < 10000) errors.push('LAS chunk size minimal 10.000 titik.');
  const gate = Number(p.max_rmse_gate_m || 0);
  if (!Number.isFinite(gate) || gate < 0) errors.push('Max RMSE gate harus 0 atau angka positif.');
  return errors;
}

function validationForTask(config, task) {
  if (task === 'registration') return basicRegistrationValidation(config);
  if (task === 'period') return basicPeriodValidation(config);
  return basicCompareValidation(config);
}
function scriptForTask(task) {
  if (task === 'registration') return 'registration_pipeline.R';
  if (task === 'period') return 'period_pipeline.R';
  return 'compare_pipeline.R';
}

function manifestPath(folder) { return path.join(folder || '', 'period_manifest.json'); }
function readManifest(folder) {
  const file = manifestPath(folder);
  if (!fileExists(file)) throw new Error(`period_manifest.json tidak ditemukan di: ${folder}`);
  return JSON.parse(fs.readFileSync(file, 'utf8'));
}
function basicCompareValidation(config) {
  const errors = [];
  if (!config || typeof config !== 'object') return ['Konfigurasi perbandingan tidak valid.'];
  const i = config.inputs || {};
  for (const [key,label] of [['prev_package','Package periode sebelumnya'],['curr_package','Package periode saat ini']]) {
    if (!i[key] || !fs.existsSync(i[key]) || !fs.statSync(i[key]).isDirectory()) errors.push(`${label} harus berupa folder hasil Process One Period.`);
    else if (!fileExists(manifestPath(i[key]))) errors.push(`${label} tidak memiliki period_manifest.json.`);
  }
  if (i.prev_package && i.curr_package && path.resolve(i.prev_package) === path.resolve(i.curr_package)) errors.push('Package PREV dan CURR tidak boleh sama.');
  if (!i.output_root) errors.push('Folder output perbandingan belum dipilih.');
  if (!errors.length) {
    try {
      const a = readManifest(i.prev_package); const b = readManifest(i.curr_package);
      if (!validPeriod(a.period_code) || !validPeriod(b.period_code)) errors.push('Kode periode pada manifest tidak valid.');
      if (String(a.period_code).toUpperCase() === String(b.period_code).toUpperCase()) errors.push('Kode periode PREV dan CURR pada manifest sama.');
    } catch (e) { errors.push(e.message); }
  }
  return errors;
}

function createTaskRunConfig(config, task) {
  const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
  const outputRoot = path.resolve(config.inputs.output_root);
  fs.mkdirSync(outputRoot, { recursive: true });
  let prefix;
  if (task === 'registration') {
    const target = String(config.parameters.target_period_code || 'TGT').trim().toUpperCase().replace(/[^A-Z0-9]/g,'').slice(0,3) || 'TGT';
    const master = String(config.parameters.master_period_code || 'MST').trim().toUpperCase().replace(/[^A-Z0-9]/g,'').slice(0,3) || 'MST';
    prefix = `registration_${target}_to_${master}`;
  } else if (task === 'period') {
    const code = String(config.parameters.period_code || 'PX').trim().toUpperCase().replace(/[^A-Z0-9]/g,'').slice(0,3) || 'PX';
    prefix = `period_${code}`;
  } else {
    let prev = 'PRE', curr = 'CUR';
    try { prev = String(readManifest(config.inputs.prev_package).period_code || prev).toUpperCase(); } catch {}
    try { curr = String(readManifest(config.inputs.curr_package).period_code || curr).toUpperCase(); } catch {}
    prefix = `compare_${prev}_${curr}`;
  }
  const runDir = path.join(outputRoot, `${prefix}_${timestamp}`);
  fs.mkdirSync(runDir, { recursive: true });
  const completeConfig = { ...config, task, app: { name: APP_NAME, version: app.getVersion(), created_at: new Date().toISOString(), run_dir: runDir } };
  const configPath = path.join(runDir, 'run_config.json');
  fs.writeFileSync(configPath, JSON.stringify(completeConfig, null, 2), 'utf8');
  return { runDir, configPath };
}

function parseEventLine(line) {
  const marker = 'APP_EVENT:'; const idx = line.indexOf(marker);
  if (idx < 0) return null;
  try { return JSON.parse(line.slice(idx + marker.length).trim()); }
  catch { return { type: 'log', level: 'warning', message: `Event R tidak dapat diparse: ${line}` }; }
}
function getBackendScriptPath(scriptName) {
  const candidates = app.isPackaged
    ? [path.join(process.resourcesPath, 'r', scriptName), path.join(process.resourcesPath, 'app.asar.unpacked', 'r', scriptName)]
    : [path.join(__dirname, '..', 'r', scriptName)];
  for (const candidate of candidates) if (fs.existsSync(candidate)) return candidate;
  return { missing: true, candidates };
}
function clearHeartbeat() { if (heartbeatTimer) clearInterval(heartbeatTimer); heartbeatTimer = null; }

function runRProcess({ rscriptPath, scriptName, args = [], modeName }) {
  return new Promise((resolve, reject) => {
    if (activeProcess) return reject(new Error('Masih ada proses R yang berjalan.'));
    const resolvedScript = getBackendScriptPath(scriptName);
    if (typeof resolvedScript !== 'string') return reject(new Error(`Script backend tidak ditemukan. Lokasi: ${resolvedScript.candidates.join(' | ')}`));
    const child = spawn(rscriptPath, [resolvedScript, ...args], {
      windowsHide: true, shell: false,
      env: { ...process.env, R_DEFAULT_PACKAGES: process.env.R_DEFAULT_PACKAGES || 'datasets,utils,grDevices,graphics,stats,methods' }
    });
    const startedAt = Date.now(); let lastOutputAt = startedAt; let lastStructuredError = ''; const rawStderr = [];
    activeProcess = child; activeRun = { modeName, pid: child.pid, startedAt };
    emitToRenderer({ type:'process', status:'started', mode:modeName, pid:child.pid, startedAt:new Date(startedAt).toISOString() });
    clearHeartbeat();
    heartbeatTimer = setInterval(() => {
      if (!activeProcess || activeProcess.pid !== child.pid) return;
      const now = Date.now();
      emitToRenderer({ type:'heartbeat', pid:child.pid, mode:modeName, elapsed_seconds:Math.floor((now-startedAt)/1000), idle_seconds:Math.floor((now-lastOutputAt)/1000), timestamp:new Date(now).toISOString() });
    }, 2000);
    const stdoutLines = readline.createInterface({ input: child.stdout });
    const stderrLines = readline.createInterface({ input: child.stderr });
    stdoutLines.on('line', (line) => {
      lastOutputAt = Date.now(); const event = parseEventLine(line);
      if (event?.type === 'fatal' && event.message) lastStructuredError = event.message;
      if (event) emitToRenderer(event); else if (line.trim()) emitToRenderer({ type:'log', level:'info', message:line, source:'stdout' });
    });
    stderrLines.on('line', (line) => {
      lastOutputAt = Date.now(); rawStderr.push(line);
      if (line.trim()) emitToRenderer({ type:'log', level:'error', message:line, source:'stderr' });
    });
    child.on('error', (error) => {
      clearHeartbeat(); activeProcess = null; activeRun = null;
      emitToRenderer({ type:'process', status:'error', mode:modeName, message:error.message }); reject(error);
    });
    child.on('close', (code, signal) => {
      clearHeartbeat(); activeProcess = null; activeRun = null;
      emitToRenderer({ type:'process', status:code===0?'completed':'failed', mode:modeName, code, signal, elapsed_seconds:Math.floor((Date.now()-startedAt)/1000), stderr:rawStderr.slice(-30).join('\n') });
      if (code === 0) resolve({ code, signal });
      else reject(new Error(lastStructuredError || rawStderr.slice(-10).join('\n') || `Rscript berhenti dengan kode ${code}.`));
    });
  });
}

async function selectFile(filters) {
  const result = await dialog.showOpenDialog(mainWindow, { properties:['openFile'], filters });
  return result.canceled ? '' : result.filePaths[0];
}
async function selectFolder() {
  const result = await dialog.showOpenDialog(mainWindow, { properties:['openDirectory','createDirectory'] });
  return result.canceled ? '' : result.filePaths[0];
}

ipcMain.handle('dialog:pointCloud', () => selectFile([{ name:'Point Cloud', extensions:['las','laz'] }]));
ipcMain.handle('dialog:treePoints', () => selectFile([{ name:'Titik spasial', extensions:['shp','gpkg','geojson','json'] }]));
ipcMain.handle('dialog:dtm', () => selectFile([{ name:'GeoTIFF DTM', extensions:['tif','tiff'] }]));
ipcMain.handle('dialog:controls', () => selectFile([{ name:'Control Points', extensions:['csv','shp','gpkg','geojson','json'] }]));
ipcMain.handle('dialog:rscript', () => selectFile([{ name:'Rscript', extensions:['exe'] }, { name:'Semua file', extensions:['*'] }]));
ipcMain.handle('dialog:folder', () => selectFolder());

ipcMain.handle('app:getState', () => ({ version:app.getVersion(), settings:readSettings(), rscriptCandidates:findRscriptCandidates(), packaged:app.isPackaged, activeRun }));
ipcMain.handle('app:saveSettings', (_event, settings) => { writeSettings(settings || {}); return { ok:true }; });
ipcMain.handle('environment:detect', (_event, requestedPath) => {
  const rscriptPath = getRscriptPath(requestedPath);
  return { found:Boolean(rscriptPath), rscriptPath, candidates:findRscriptCandidates(), bundled:rscriptPath ? rscriptPath.includes(path.join('vendor','R')) : false };
});
ipcMain.handle('environment:check', async (_event, requestedPath) => {
  const rscriptPath = getRscriptPath(requestedPath); if (!rscriptPath) throw new Error('Rscript.exe tidak ditemukan.');
  writeSettings({ ...readSettings(), rscriptPath });
  await runRProcess({ rscriptPath, scriptName:'check_environment.R', args:[], modeName:'environment-check' });
  return { ok:true, rscriptPath };
});
ipcMain.handle('environment:installPackages', async (_event, requestedPath) => {
  const rscriptPath = getRscriptPath(requestedPath); if (!rscriptPath) throw new Error('Rscript.exe tidak ditemukan.');
  await runRProcess({ rscriptPath, scriptName:'install_packages.R', args:[], modeName:'package-install' }); return { ok:true };
});

async function validateTask(payload, task) {
  const config = payload?.config; const errors = validationForTask(config, task);
  if (errors.length) return { ok:false, errors };
  const rscriptPath = getRscriptPath(payload?.rscriptPath); if (!rscriptPath) return { ok:false, errors:['Rscript.exe tidak ditemukan.'] };
  const tempDir = fs.mkdtempSync(path.join(os.tmpdir(), `sawitheight-${task}-validate-`));
  const tempConfig = { ...config, task, app:{ name:APP_NAME, version:app.getVersion(), run_dir:tempDir } };
  const configPath = path.join(tempDir, 'validate_config.json'); fs.writeFileSync(configPath, JSON.stringify(tempConfig,null,2),'utf8');
  try {
    await runRProcess({ rscriptPath, scriptName:scriptForTask(task), args:['validate', configPath], modeName:`${task}-validation` });
    return { ok:true, rscriptPath };
  } catch (error) { return { ok:false, errors:[error.message] }; }
  finally { fs.rmSync(tempDir, { recursive:true, force:true }); }
}
async function startTask(payload, task) {
  const config = payload?.config; const errors = validationForTask(config, task);
  if (errors.length) return { ok:false, errors };
  if (activeProcess) return { ok:false, errors:['Masih ada proses yang berjalan.'] };
  const rscriptPath = getRscriptPath(payload?.rscriptPath); if (!rscriptPath) return { ok:false, errors:['Rscript.exe tidak ditemukan.'] };
  writeSettings({ ...readSettings(), rscriptPath, [`last${task[0].toUpperCase()+task.slice(1)}Config`]:config });
  const run = createTaskRunConfig(config, task);
  emitToRenderer({ type:'run-created', task, runDir:run.runDir, configPath:run.configPath });
  runRProcess({ rscriptPath, scriptName:scriptForTask(task), args:['run', run.configPath], modeName:task })
    .catch((error) => emitToRenderer({ type:'fatal', task, message:error.message, runDir:run.runDir }));
  return { ok:true, runDir:run.runDir, configPath:run.configPath };
}

ipcMain.handle('registration:validate', (_event, payload) => validateTask(payload, 'registration'));
ipcMain.handle('registration:start', (_event, payload) => startTask(payload, 'registration'));
ipcMain.handle('period:validate', (_event, payload) => validateTask(payload, 'period'));
ipcMain.handle('period:start', (_event, payload) => startTask(payload, 'period'));
ipcMain.handle('compare:validate', (_event, payload) => validateTask(payload, 'compare'));
ipcMain.handle('compare:start', (_event, payload) => startTask(payload, 'compare'));
ipcMain.handle('analysis:cancel', async () => {
  if (!activeProcess) return { ok:false, message:'Tidak ada proses aktif.' };
  const pid = activeProcess.pid; clearHeartbeat();
  if (process.platform === 'win32') spawnSync('taskkill', ['/pid', String(pid), '/T', '/F'], { windowsHide:true });
  else activeProcess.kill('SIGTERM');
  emitToRenderer({ type:'process', status:'cancelled', pid }); return { ok:true };
});
ipcMain.handle('shell:openPath', async (_event, targetPath) => {
  if (!targetPath || !fs.existsSync(targetPath)) return 'Path tidak ditemukan.'; return shell.openPath(targetPath);
});
ipcMain.handle('shell:showItem', (_event, targetPath) => { if (targetPath && fs.existsSync(targetPath)) shell.showItemInFolder(targetPath); return { ok:true }; });
ipcMain.handle('help:openTutorial', async () => {
  const tutorial = app.isPackaged ? getResourcePath('docs','reference_tutorial.html') : path.join(__dirname,'..','docs','reference_tutorial.html');
  if (!fs.existsSync(tutorial)) throw new Error('File tutorial tidak ditemukan.'); return shell.openPath(tutorial);
});

app.setName(APP_NAME); app.setAppUserModelId('com.pmnp.sawitheight');
app.whenReady().then(() => { Menu.setApplicationMenu(null); createWindow(); app.on('activate', () => { if (BrowserWindow.getAllWindows().length === 0) createWindow(); }); });
app.on('before-quit', (event) => {
  if (!activeProcess) return; event.preventDefault(); const pid = activeProcess.pid; clearHeartbeat();
  if (process.platform === 'win32') spawnSync('taskkill', ['/pid', String(pid), '/T', '/F'], { windowsHide:true }); else activeProcess.kill('SIGTERM');
  activeProcess = null; app.exit(0);
});
app.on('window-all-closed', () => { if (process.platform !== 'darwin') app.quit(); });
