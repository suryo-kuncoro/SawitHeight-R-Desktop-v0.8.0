const { contextBridge, ipcRenderer } = require('electron');
contextBridge.exposeInMainWorld('sawitHeight', {
  selectPointCloud: () => ipcRenderer.invoke('dialog:pointCloud'),
  selectTreePoints: () => ipcRenderer.invoke('dialog:treePoints'),
  selectDtm: () => ipcRenderer.invoke('dialog:dtm'),
  selectControls: () => ipcRenderer.invoke('dialog:controls'),
  selectFolder: () => ipcRenderer.invoke('dialog:folder'),
  selectRscript: () => ipcRenderer.invoke('dialog:rscript'),
  getState: () => ipcRenderer.invoke('app:getState'),
  saveSettings: (settings) => ipcRenderer.invoke('app:saveSettings', settings),
  detectEnvironment: (requestedPath) => ipcRenderer.invoke('environment:detect', requestedPath),
  checkEnvironment: (requestedPath) => ipcRenderer.invoke('environment:check', requestedPath),
  installPackages: (requestedPath) => ipcRenderer.invoke('environment:installPackages', requestedPath),
  validateRegistration: (payload) => ipcRenderer.invoke('registration:validate', payload),
  startRegistration: (payload) => ipcRenderer.invoke('registration:start', payload),
  validatePeriod: (payload) => ipcRenderer.invoke('period:validate', payload),
  startPeriod: (payload) => ipcRenderer.invoke('period:start', payload),
  validateCompare: (payload) => ipcRenderer.invoke('compare:validate', payload),
  startCompare: (payload) => ipcRenderer.invoke('compare:start', payload),
  cancelAnalysis: () => ipcRenderer.invoke('analysis:cancel'),
  openPath: (targetPath) => ipcRenderer.invoke('shell:openPath', targetPath),
  showItem: (targetPath) => ipcRenderer.invoke('shell:showItem', targetPath),
  openTutorial: () => ipcRenderer.invoke('help:openTutorial'),
  onAnalysisEvent: (callback) => {
    const listener = (_event, payload) => callback(payload);
    ipcRenderer.on('analysis:event', listener);
    return () => ipcRenderer.removeListener('analysis:event', listener);
  }
});
