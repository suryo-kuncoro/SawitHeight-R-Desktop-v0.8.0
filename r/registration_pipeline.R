options(warn = 1)
suppressPackageStartupMessages({library(jsonlite); library(lidR); library(sf); library(dplyr)})
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop('Usage: Rscript registration_pipeline.R <validate|run> <config.json>')
mode <- args[[1]]; cfg <- jsonlite::fromJSON(args[[2]], simplifyVector = FALSE)
`%||%` <- function(a,b) if (is.null(a) || length(a)==0 || (length(a)==1 && is.na(a))) b else a
as_num <- function(x, default=NA_real_) {v<-suppressWarnings(as.numeric(x)); if(length(v)&&is.finite(v[1])) v[1] else default}
as_chr <- function(x, default='') {if(is.null(x)||!length(x)||is.na(x[1])) default else as.character(x[1])}
as_bool <- function(x, default=FALSE) {if(is.null(x)||!length(x)||is.na(x[1])) default else isTRUE(x[1])}
run_dir <- normalizePath(as_chr(cfg$app$run_dir, tempdir()), winslash='/', mustWork=FALSE); dir.create(run_dir,recursive=TRUE,showWarnings=FALSE)
log_path <- file.path(run_dir,'analysis.log'); current_stage <- 'validation'
emit <- function(type, ...) {x<-list(type=type,...); cat('APP_EVENT:',jsonlite::toJSON(x,auto_unbox=TRUE,null='null',na='null'),"\n",sep=''); flush.console()}
write_log <- function(level,msg){line<-sprintf('[%s] [%s] [%s] %s',format(Sys.time(),'%Y-%m-%d %H:%M:%S'),level,current_stage,msg);cat(line,'\n');cat(line,'\n',file=log_path,append=TRUE);flush.console();emit('log',level=tolower(level),message=msg,timestamp=format(Sys.time(),'%Y-%m-%dT%H:%M:%S'))}
set_stage <- function(stage,label,progress){current_stage<<-stage;emit('progress',stage=stage,label=label,progress=progress);write_log('INFO',label)}
stop_run <- function(msg) stop(msg,call.=FALSE)
period_clean <- function(x){x<-toupper(gsub('[^A-Z0-9]','',as_chr(x)));if(!nzchar(x)||nchar(x)>3)stop_run('Kode periode harus 1-3 karakter alfanumerik.');x}
file_ok <- function(p) nzchar(as_chr(p)) && file.exists(as_chr(p))

read_control_points <- function(path, id_field, x_field, y_field, z_field, label) {
  ext <- tolower(tools::file_ext(path))
  if (ext == 'csv') {
    d <- utils::read.csv(path, stringsAsFactors=FALSE, check.names=FALSE)
    req <- c(id_field,x_field,y_field,z_field)
    miss <- setdiff(req,names(d)); if(length(miss)) stop_run(paste(label,'control CSV tidak memiliki field:',paste(miss,collapse=', ')))
    out <- data.frame(id=as.character(d[[id_field]]), X=as.numeric(d[[x_field]]), Y=as.numeric(d[[y_field]]), Z=as.numeric(d[[z_field]]), stringsAsFactors=FALSE)
    crs <- sf::NA_crs_
  } else {
    s <- sf::st_read(path, quiet=TRUE)
    if (!(id_field %in% names(s))) stop_run(paste(label,'control vector tidak memiliki field ID:',id_field))
    if (!all(sf::st_geometry_type(s) %in% c('POINT','MULTIPOINT'))) stop_run(paste(label,'control vector harus POINT.'))
    if (any(sf::st_geometry_type(s)=='MULTIPOINT')) s <- suppressWarnings(sf::st_cast(s,'POINT'))
    co <- sf::st_coordinates(s)
    if (nrow(co) != nrow(s)) stop_run(paste(label,'control geometry tidak satu-ke-satu.'))
    if (ncol(co) >= 3 && all(is.finite(co[,3]))) z <- co[,3]
    else {if (!(z_field %in% names(s))) stop_run(paste(label,'control vector harus memiliki Z geometry atau field',z_field)); z <- as.numeric(s[[z_field]])}
    out <- data.frame(id=as.character(s[[id_field]]), X=as.numeric(co[,1]), Y=as.numeric(co[,2]), Z=z, stringsAsFactors=FALSE)
    crs <- sf::st_crs(s)
  }
  if(any(!nzchar(out$id)|is.na(out$id))) stop_run(paste(label,'control memiliki ID kosong.'))
  if(anyDuplicated(out$id)) stop_run(paste(label,'control ID tidak unik.'))
  if(any(!is.finite(out$X)|!is.finite(out$Y)|!is.finite(out$Z))) stop_run(paste(label,'control memiliki XYZ tidak valid.'))
  list(data=out, crs=crs)
}

fit_transform <- function(src, dst, model) {
  n <- nrow(src); if(n != nrow(dst)) stop('Jumlah source/target tidak sama.')
  if(model == 'TRANSLATION_3D') {
    if(n < 1) stop('Translation 3D membutuhkan minimal 1 control pair.')
    tr <- apply(dst-src,2,median,na.rm=TRUE); R <- diag(3); sc <- 1
  } else {
    if(n < 3) stop(paste(model,'membutuhkan minimal 3 control pair.'))
    mx <- colMeans(src); my <- colMeans(dst); X <- sweep(src,2,mx,'-'); Y <- sweep(dst,2,my,'-')
    if(qr(X)$rank < 2) stop('Control source kollinear/tidak cukup menyebar untuk rotasi 3D.')
    C <- crossprod(X,Y)/n; sv <- svd(C); S <- diag(3); if(det(sv$v %*% t(sv$u)) < 0) S[3,3] <- -1
    R <- sv$v %*% S %*% t(sv$u)
    if(model == 'SIMILARITY_3D') {
      varx <- sum(X^2)/n; if(!is.finite(varx)||varx<=0) stop('Varians control source tidak valid.')
      sc <- sum(sv$d * diag(S))/varx
    } else if(model == 'RIGID_3D') sc <- 1 else stop('Model transform tidak dikenal.')
    tr <- as.numeric(my - sc * (R %*% mx))
  }
  A <- sc*R; M <- diag(4); M[1:3,1:3] <- A; M[1:3,4] <- tr
  list(R=R,scale=sc,translation=tr,matrix=M)
}
apply_xyz <- function(xyz, fit){t(fit$scale * (fit$R %*% t(xyz)) + fit$translation)}
rmse3 <- function(res){if(!nrow(res))return(NA_real_);sqrt(mean(rowSums(res^2)))}
rotation_angles_deg <- function(R){
  # XYZ/Tait-Bryan diagnostic only
  sy <- sqrt(R[1,1]^2 + R[2,1]^2); singular <- sy < 1e-8
  if(!singular){x<-atan2(R[3,2],R[3,3]); y<-atan2(-R[3,1],sy); z<-atan2(R[2,1],R[1,1])}
  else{x<-atan2(-R[2,3],R[2,2]); y<-atan2(-R[3,1],sy); z<-0}
  c(rx=x,ry=y,rz=z)*180/pi
}

validate_config <- function(deep=TRUE) {
  i<-cfg$inputs %||% list(); p<-cfg$parameters %||% list()
  target<-as_chr(i$target_point_cloud); master<-as_chr(i$master_point_cloud); tc<-as_chr(i$target_controls); mc<-as_chr(i$master_controls); out<-as_chr(i$output_root)
  if(!file_ok(target)||!grepl('\\.(las|laz)$',target,ignore.case=TRUE))stop_run('Target point cloud harus LAS/LAZ valid.')
  if(!file_ok(master)||!grepl('\\.(las|laz)$',master,ignore.case=TRUE))stop_run('Master point cloud harus LAS/LAZ valid.')
  for(x in c(tc,mc))if(!file_ok(x)||!grepl('\\.(csv|shp|gpkg|geojson|json)$',x,ignore.case=TRUE))stop_run('Control point harus CSV/SHP/GPKG/GeoJSON yang valid.')
  if(!nzchar(out))stop_run('Folder output registrasi wajib dipilih.')
  period_clean(p$target_period_code); period_clean(p$master_period_code)
  model<-toupper(as_chr(p$transform_model,'TRANSLATION_3D'));if(!(model%in%c('TRANSLATION_3D','RIGID_3D','SIMILARITY_3D')))stop_run('Transform model tidak valid.')
  for(nm in c('control_id_field','control_x_field','control_y_field','control_z_field')) if(!nzchar(as_chr(p[[nm]]))) stop_run(paste(nm,'wajib diisi.'))
  cs<-as_num(p$las_chunk_size,1000000); if(!is.finite(cs)||cs<10000)stop_run('LAS chunk size minimal 10,000 titik.')
  gate<-as_num(p$max_rmse_gate_m,0);if(!is.finite(gate)||gate<0)stop_run('RMSE gate harus 0 (off) atau angka positif.')
  if(deep){
    set_stage('controls','Validasi matched control points',12)
    idf<-as_chr(p$control_id_field); xf<-as_chr(p$control_x_field); yf<-as_chr(p$control_y_field); zf<-as_chr(p$control_z_field)
    a<-read_control_points(tc,idf,xf,yf,zf,'TARGET'); b<-read_control_points(mc,idf,xf,yf,zf,'MASTER')
    j<-inner_join(a$data,b$data,by='id',suffix=c('_src','_dst')); min_n<-if(model=='TRANSLATION_3D')1 else 3
    if(nrow(j)<min_n)stop_run(paste('Matched control pair hanya',nrow(j),'; minimal',min_n,'untuk',model))
    if(!is.na(a$crs)&&!is.na(b$crs)&&!identical(a$crs$wkt,b$crs$wkt))stop_run('CRS control TARGET dan MASTER berbeda. Jangan reproject otomatis pada modul alignment.')
    h1<-tryCatch(readLASheader(target),error=function(e)NULL); h2<-tryCatch(readLASheader(master),error=function(e)NULL)
    if(!is.null(h1)&&!is.null(h2)){c1<-st_crs(h1);c2<-st_crs(h2);if(!is.na(c1)&&!is.na(c2)&&!identical(c1$wkt,c2$wkt))stop_run('CRS LAS TARGET dan MASTER berbeda. Samakan CRS/datum terlebih dahulu.')}
    fit_transform(as.matrix(j[,c('X_src','Y_src','Z_src')]),as.matrix(j[,c('X_dst','Y_dst','Z_dst')]),model)
  }
  invisible(TRUE)
}

make_report <- function(summary, residuals, path){
  rows<-paste0('<tr><td>',residuals$id,'</td><td>',sprintf('%.4f',residuals$before_m),'</td><td>',sprintf('%.4f',residuals$after_m),'</td><td>',ifelse(is.finite(residuals$loo_m),sprintf('%.4f',residuals$loo_m),'-'),'</td></tr>',collapse='')
  html<-paste0('<!doctype html><html><meta charset="utf-8"><style>body{font-family:Segoe UI,Arial;max-width:1100px;margin:40px auto;color:#222}table{border-collapse:collapse;width:100%}td,th{border:1px solid #ddd;padding:8px}th{background:#eee}</style><h1>Registration QC / Master Alignment</h1><p><b>',summary$target_period_code,' → master ',summary$master_period_code,'</b></p><p>Model: ',summary$transform_model,' | Controls: ',summary$matched_controls,' | RMSE before: ',sprintf('%.4f',summary$rmse_before_m),' m | RMSE after: ',sprintf('%.4f',summary$rmse_after_m),' m | LOO RMSE: ',ifelse(is.finite(summary$loo_rmse_m),sprintf('%.4f',summary$loo_rmse_m),'NA'),' m</p><p>Scale: ',sprintf('%.9f',summary$scale),' | Translation XYZ: ',paste(sprintf('%.4f',unlist(summary$translation_m)),collapse=', '),'</p><table><thead><tr><th>ID</th><th>Before (m)</th><th>After (m)</th><th>LOO (m)</th></tr></thead><tbody>',rows,'</tbody></table></html>')
  writeLines(html,path,useBytes=TRUE)
}

run_pipeline <- function(){
  validate_config(FALSE); i<-cfg$inputs; p<-cfg$parameters
  TARGET<-period_clean(p$target_period_code); MASTER<-period_clean(p$master_period_code); model<-toupper(as_chr(p$transform_model,'TRANSLATION_3D'))
  set_stage('controls','Load & match control points',8)
  idf<-as_chr(p$control_id_field); xf<-as_chr(p$control_x_field); yf<-as_chr(p$control_y_field); zf<-as_chr(p$control_z_field)
  ta<-read_control_points(as_chr(i$target_controls),idf,xf,yf,zf,'TARGET'); ma<-read_control_points(as_chr(i$master_controls),idf,xf,yf,zf,'MASTER')
  pair<-inner_join(ta$data,ma$data,by='id',suffix=c('_src','_dst')); min_n<-if(model=='TRANSLATION_3D')1 else 3
  if(nrow(pair)<min_n)stop_run(paste('Matched control pair hanya',nrow(pair),'; minimal',min_n))
  write_log('INFO',paste('Matched control pairs:',nrow(pair)))
  src<-as.matrix(pair[,c('X_src','Y_src','Z_src')]); dst<-as.matrix(pair[,c('X_dst','Y_dst','Z_dst')])
  set_stage('fit','Estimate target → master transform',24); fit<-fit_transform(src,dst,model); pred<-apply_xyz(src,fit); before_res<-src-dst; after_res<-pred-dst
  before_d<-sqrt(rowSums(before_res^2)); after_d<-sqrt(rowSums(after_res^2)); rmse_before<-rmse3(before_res); rmse_after<-rmse3(after_res)
  write_log('INFO',sprintf('Control RMSE before: %.4f m | after: %.4f m',rmse_before,rmse_after))
  ang<-rotation_angles_deg(fit$R); write_log('INFO',sprintf('Scale %.9f | translation [%.4f, %.4f, %.4f] m | rotation [%.6f, %.6f, %.6f] deg',fit$scale,fit$translation[1],fit$translation[2],fit$translation[3],ang[1],ang[2],ang[3]))
  set_stage('crosscheck','Leave-one-out registration QC',36); loo<-rep(NA_real_,nrow(pair)); min_fit<-if(model=='TRANSLATION_3D')1 else 3
  if(nrow(pair)>=min_fit+1){for(k in seq_len(nrow(pair))){idx<-setdiff(seq_len(nrow(pair)),k); loo[k]<-tryCatch({f<-fit_transform(src[idx,,drop=FALSE],dst[idx,,drop=FALSE],model); pr<-apply_xyz(src[k,,drop=FALSE],f);sqrt(sum((pr-dst[k,,drop=FALSE])^2))},error=function(e)NA_real_)}}
  loo_rmse<-if(any(is.finite(loo)))sqrt(mean(loo[is.finite(loo)]^2))else NA_real_; if(is.finite(loo_rmse))write_log('INFO',sprintf('Leave-one-out RMSE: %.4f m',loo_rmse))else write_log('WARNING','Leave-one-out RMSE tidak tersedia (control terlalu sedikit / geometry tidak stabil).')
  gate<-as_num(p$max_rmse_gate_m,0); gate_metric<-if(is.finite(loo_rmse))loo_rmse else rmse_after; if(gate>0&&is.finite(gate_metric)&&gate_metric>gate)stop_run(sprintf('Registration QC gagal: RMSE %.4f m melebihi gate %.4f m. Registered LAS tidak dibuat.',gate_metric,gate))
  set_stage('load','Load target LAS/LAZ',48); target_path<-as_chr(i$target_point_cloud); master_path<-as_chr(i$master_point_cloud); las<-readLAS(target_path); if(is.null(las)||npoints(las)==0)stop_run('Target point cloud kosong/gagal dibaca.'); write_log('INFO',paste('Target points:',format(npoints(las),big.mark=',')))
  hmaster<-tryCatch(readLASheader(master_path),error=function(e)NULL); if(!is.null(hmaster)){ct<-st_crs(las);cm<-st_crs(hmaster);if(!is.na(ct)&&!is.na(cm)&&!identical(ct$wkt,cm$wkt))stop_run('CRS target dan master LAS berbeda.')}
  set_stage('transform','Apply transform ke target point cloud',58); chunk<-as.integer(as_num(p$las_chunk_size,1000000)); n<-npoints(las); starts<-seq.int(1L,n,by=chunk); A<-fit$scale*fit$R; tr<-fit$translation
  for(ci in seq_along(starts)){s<-starts[ci];e<-min(n,s+chunk-1L);ix<-s:e;x<-las@data$X[ix];y<-las@data$Y[ix];z<-las@data$Z[ix];las@data$X[ix]<-A[1,1]*x+A[1,2]*y+A[1,3]*z+tr[1];las@data$Y[ix]<-A[2,1]*x+A[2,2]*y+A[2,3]*z+tr[2];las@data$Z[ix]<-A[3,1]*x+A[3,2]*y+A[3,3]*z+tr[3];if(ci==1||ci==length(starts)||ci%%5==0)write_log('INFO',sprintf('Transform chunk %d/%d (%d-%d points)',ci,length(starts),s,e));emit('progress',stage='transform',label=sprintf('Transform LAS chunk %d/%d',ci,length(starts)),progress=58+round(24*ci/length(starts)))}
  set_stage('las_qc','Quantize & update LAS header',83); try(las_quantize(las),silent=TRUE); las<-las_update(las); if(!is.null(hmaster)&&!is.na(st_crs(hmaster))) st_crs(las)<-st_crs(hmaster)
  set_stage('export','Export registered LAS & QC report',90); out_laz<-file.path(run_dir,paste0('registered_',TARGET,'_to_',MASTER,'.laz'));writeLAS(las,out_laz)
  residuals<-data.frame(id=pair$id,before_dx=before_res[,1],before_dy=before_res[,2],before_dz=before_res[,3],before_m=before_d,after_dx=after_res[,1],after_dy=after_res[,2],after_dz=after_res[,3],after_m=after_d,loo_m=loo,stringsAsFactors=FALSE);res_csv<-file.path(run_dir,'registration_control_residuals.csv');write.csv(residuals,res_csv,row.names=FALSE)
  mat_csv<-file.path(run_dir,'registration_transform_matrix.csv');write.csv(fit$matrix,mat_csv,row.names=FALSE)
  summary<-list(task='registration',app_version=as_chr(cfg$app$version,'0.8.0'),run_dir=run_dir,target_period_code=TARGET,master_period_code=MASTER,transform_model=model,matched_controls=nrow(pair),rmse_before_m=rmse_before,rmse_after_m=rmse_after,loo_rmse_m=loo_rmse,scale=fit$scale,translation_m=as.list(setNames(fit$translation,c('X','Y','Z'))),rotation_deg=as.list(setNames(as.numeric(ang),c('X','Y','Z'))),rmse_gate_m=if(gate>0)gate else NA_real_,registered_las=out_laz,master_reference=master_path,control_residual_csv=res_csv,transform_matrix_csv=mat_csv,preview=head(residuals[,c('id','before_m','after_m','loo_m')],25))
  summary_path<-file.path(run_dir,'registration_summary.json');jsonlite::write_json(summary,summary_path,pretty=TRUE,auto_unbox=TRUE,na='null')
  manifest<-list(schema_version='1.0',task='registration',target_period_code=TARGET,master_period_code=MASTER,transform_model=model,target_input=normalizePath(target_path,winslash='/',mustWork=FALSE),master_reference=normalizePath(master_path,winslash='/',mustWork=FALSE),registered_las=normalizePath(out_laz,winslash='/',mustWork=FALSE),matched_controls=nrow(pair),rmse_after_m=rmse_after,loo_rmse_m=loo_rmse,scale=fit$scale,translation_m=summary$translation_m,rotation_deg=summary$rotation_deg,created_at=format(Sys.time(),'%Y-%m-%dT%H:%M:%S%z'))
  manifest_path<-file.path(run_dir,'registration_manifest.json');jsonlite::write_json(manifest,manifest_path,pretty=TRUE,auto_unbox=TRUE,na='null')
  report_path<-file.path(run_dir,paste0('report_registration_',TARGET,'_to_',MASTER,'.html'));make_report(summary,residuals,report_path)
  outputs<-c(out_laz,res_csv,mat_csv,summary_path,manifest_path,report_path,log_path);output_manifest<-file.path(run_dir,'output_manifest.csv');write.csv(data.frame(file=basename(outputs),path=outputs),output_manifest,row.names=FALSE);summary$output_files<-c(outputs,output_manifest);jsonlite::write_json(summary,summary_path,pretty=TRUE,auto_unbox=TRUE,na='null')
  set_stage('complete',paste('Registration',TARGET,'→',MASTER,'selesai'),100);emit('result',task='registration',status='success',runDir=run_dir,summaryPath=summary_path,reportPath=report_path,summary=summary);write_log('INFO','Registration selesai. Registered LAS siap menjadi input Process One Period.')
}

status_code<-0L
withCallingHandlers(tryCatch({if(mode=='validate'){validate_config(TRUE);set_stage('complete','Validasi registration berhasil',100);emit('validation-result',status='success',message='Input master alignment valid.')}else run_pipeline()},error=function(e){status_code<<-10L;msg<-conditionMessage(e);try(cat(sprintf('[%s] [FATAL] [%s] %s\n',format(Sys.time(),'%Y-%m-%d %H:%M:%S'),current_stage,msg),file=log_path,append=TRUE),silent=TRUE);emit('fatal',stage=current_stage,message=msg,runDir=run_dir,logPath=log_path)}),warning=function(w){try(write_log('WARNING',conditionMessage(w)),silent=TRUE);invokeRestart('muffleWarning')})
quit(status=status_code)
