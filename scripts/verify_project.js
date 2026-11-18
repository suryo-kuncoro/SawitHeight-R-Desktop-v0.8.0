const fs = require('node:fs');
const path = require('node:path');
const root = path.resolve(__dirname, '..');
const required = [
  'package.json','src/main.js','src/preload.js','src/renderer/index.html','src/renderer/styles.css','src/renderer/app.js',
  'r/registration_pipeline.R','r/period_pipeline.R','r/compare_pipeline.R','r/check_environment.R','r/install_packages.R','assets/icon.ico',
  '.github/workflows/build-windows.yml','docs/reference_tutorial.html','docs/V0.8.0_MASTER_REGISTRATION.md','docs/TEST_PLAN.md'
];
let failed = false;
for (const item of required) {
  const full = path.join(root,item);
  if (!fs.existsSync(full) || fs.statSync(full).size === 0) { console.error(`MISSING: ${item}`); failed = true; }
  else console.log(`OK: ${item}`);
}
const assertions = [
  ['package.json','0.8.1'],['package.json','--publish never'],
  ['src/renderer/index.html','Registration QC / Master Alignment'],['src/renderer/index.html','Process One Period'],['src/renderer/index.html','Compare 2 Period Packages'],['src/renderer/index.html','LIVE PROCESS LOG'],
  ['src/preload.js','validateRegistration'],['src/main.js',"return 'registration_pipeline.R'"],['src/main.js',"type:'heartbeat'"],
  ['r/registration_pipeline.R','TRANSLATION_3D'],['r/registration_pipeline.R','RIGID_3D'],['r/registration_pipeline.R','SIMILARITY_3D'],['r/registration_pipeline.R','Leave-one-out'],['r/registration_pipeline.R','registered_'],['r/registration_pipeline.R','registration_manifest.json'],
  ['r/period_pipeline.R','period_manifest.json'],['r/period_pipeline.R','Classification!=18L'],['r/period_pipeline.R','terrain_holdout_qc'],['r/period_pipeline.R','presence_support'],
  ['r/compare_pipeline.R','T_TECH'],['r/compare_pipeline.R','NEGATIVE_HEIGHT_OUTLIER'],['r/compare_pipeline.R',"'NO_RECHECK'"],['r/compare_pipeline.R',"'RECHECK'"],['r/compare_pipeline.R','recheck_zone_'],['r/compare_pipeline.R','read_metrics_csv'],['r/compare_pipeline.R','normalize_tree_id'],['r/compare_pipeline.R','colClasses=cc'],
  ['docs/reference_tutorial.html','Rolling 2-Period'],['docs/V0.8.0_MASTER_REGISTRATION.md','Master Alignment']
];
for (const [file,token] of assertions) {
  const text = fs.readFileSync(path.join(root,file),'utf8');
  if (!text.includes(token)) { console.error(`ASSERT FAILED: ${file} -> ${token}`); failed = true; }
  else console.log(`ASSERT OK: ${file} -> ${token}`);
}
process.exit(failed ? 1 : 0);
