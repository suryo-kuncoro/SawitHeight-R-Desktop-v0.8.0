const fs = require('node:fs');
const path = require('node:path');
const root = path.resolve(__dirname,'..');
const resources = path.join(root,'dist','win-unpacked','resources');
const requiredScripts = ['registration_pipeline.R','period_pipeline.R','compare_pipeline.R','check_environment.R','install_packages.R'];
function ok(f){try{return fs.statSync(f).isFile()&&fs.statSync(f).size>0}catch{return false}}
let failed=false;
for(const name of requiredScripts){const a=path.join(resources,'r',name),b=path.join(resources,'app.asar.unpacked','r',name);console.log(`${name}: resources/r=${ok(a)?'OK':'MISSING'} | app.asar.unpacked/r=${ok(b)?'OK':'MISSING'}`);if(!ok(a)&&!ok(b))failed=true}
const tutorial=path.join(resources,'docs','reference_tutorial.html');console.log(`Tutorial: ${ok(tutorial)?'OK':'MISSING'}`);if(!ok(tutorial))failed=true;
const vendorR=path.join(resources,'vendor','R','bin','Rscript.exe');if(fs.existsSync(path.join(root,'vendor','R'))){console.log(`Bundled Rscript: ${ok(vendorR)?'OK':'MISSING'}`);if(!ok(vendorR))failed=true}else console.log('Bundled R: not requested for this build.');
if(failed){console.error('Packaged resources verification FAILED.');process.exit(1)}console.log('Packaged resources verification PASSED.');
