options(warn = 1)
args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) >= 1) args[[1]] else 'run'
config_path <- if (length(args) >= 2) args[[2]] else NA_character_

if (!requireNamespace('jsonlite', quietly = TRUE)) {
  cat('APP_EVENT:{"type":"fatal","stage":"bootstrap","message":"Package jsonlite tidak tersedia."}\n')
  quit(status = 10)
}
suppressPackageStartupMessages({ library(lidR); library(sf); library(terra); library(dplyr) })

`%||%` <- function(a,b) if (is.null(a) || length(a)==0 || all(is.na(a))) b else a
as_num <- function(x,d=NA_real_) { y <- suppressWarnings(as.numeric(x)); if(!length(y)||!is.finite(y[1])) d else y[1] }
as_int <- function(x,d=NA_integer_) { y <- suppressWarnings(as.integer(x)); if(!length(y)||is.na(y[1])) d else y[1] }
as_bool <- function(x,d=FALSE) { if(is.null(x)||!length(x)||is.na(x[1])) d else isTRUE(as.logical(x[1])) }
clean_period <- function(x) { x <- toupper(gsub('[^A-Z0-9]','',as.character(x %||% ''))); if(!nzchar(x)||nchar(x)>3) stop('Kode periode harus 1-3 karakter.',call.=FALSE); x }
emit <- function(type,...,.list=list()) { payload <- c(list(type=type,timestamp=format(Sys.time(),'%Y-%m-%dT%H:%M:%S')),list(...),.list); cat('APP_EVENT:',jsonlite::toJSON(payload,auto_unbox=TRUE,null='null',na='null'),'\n',sep=''); try(flush(stdout()),silent=TRUE); flush.console() }
if (is.na(config_path) || !file.exists(config_path)) { emit('fatal',stage='bootstrap',message='Config JSON tidak ditemukan.'); quit(status=10) }
cfg <- jsonlite::fromJSON(config_path,simplifyVector=FALSE); inputs <- cfg$inputs %||% list(); p <- cfg$parameters %||% list()
run_dir <- normalizePath(cfg$app$run_dir %||% dirname(config_path),winslash='/',mustWork=FALSE); dir.create(run_dir,recursive=TRUE,showWarnings=FALSE)
shp_dir <- file.path(run_dir,'shapefile'); qc_dir <- file.path(run_dir,'qc'); dir.create(shp_dir,recursive=TRUE,showWarnings=FALSE); dir.create(qc_dir,recursive=TRUE,showWarnings=FALSE)
log_path <- file.path(run_dir,'analysis.log'); if(file.exists(log_path)) file.remove(log_path); current_stage <- 'bootstrap'
write_log <- function(level='INFO',message) { stamp <- format(Sys.time(),'%Y-%m-%d %H:%M:%S'); cat(sprintf('[%s] [%s] [%s] %s\n',stamp,level,current_stage,message),file=log_path,append=TRUE); emit('log',level=tolower(level),stage=current_stage,message=message) }
set_stage <- function(stage,label,progress,detail=NULL) { current_stage <<- stage; write_log('INFO',paste0('Tahap: ',label)); emit('progress',stage=stage,label=label,progress=as.numeric(progress),detail=detail) }
progress_event <- function(stage,label,progress,detail=NULL) emit('progress',stage=stage,label=label,progress=as.numeric(progress),detail=detail)
stop_run <- function(...) stop(paste0(...),call.=FALSE)
file_ok <- function(x) is.character(x)&&length(x)&&nzchar(x[1])&&file.exists(x[1])
safe_write_sf <- function(x,path) { suppressWarnings(st_write(x,path,delete_layer=TRUE,quiet=TRUE)) }
low_fence <- function(x) { x <- x[is.finite(x)]; if(length(x)<4L) return(0); max(0,as.numeric(quantile(x,.25,names=FALSE))-1.5*IQR(x)) }
top_mean <- function(z,prop) { z <- sort(z[is.finite(z)],decreasing=TRUE); if(!length(z)) return(NA_real_); mean(z[seq_len(max(1L,ceiling(length(z)*prop)))],na.rm=TRUE) }
metrics_one_clip <- function(clip,min_veg_h) {
  if(is.null(clip)) return(data.frame(n_all=0L,n_veg=0L,veg_ratio=NA_real_,p95=NA_real_,p99=NA_real_,top10=NA_real_,top20=NA_real_,top30=NA_real_))
  ni <- tryCatch(npoints(clip),error=function(e)0L); if(!is.finite(ni)||ni<=0L) return(data.frame(n_all=0L,n_veg=0L,veg_ratio=NA_real_,p95=NA_real_,p99=NA_real_,top10=NA_real_,top20=NA_real_,top30=NA_real_))
  z_all <- clip$Z[is.finite(clip$Z)]; z_veg <- z_all[z_all>=min_veg_h]
  if(!length(z_all)) return(data.frame(n_all=0L,n_veg=0L,veg_ratio=NA_real_,p95=NA_real_,p99=NA_real_,top10=NA_real_,top20=NA_real_,top30=NA_real_))
  if(!length(z_veg)) return(data.frame(n_all=length(z_all),n_veg=0L,veg_ratio=0,p95=NA_real_,p99=NA_real_,top10=NA_real_,top20=NA_real_,top30=NA_real_))
  data.frame(n_all=length(z_all),n_veg=length(z_veg),veg_ratio=length(z_veg)/length(z_all),p95=as.numeric(quantile(z_veg,.95,names=FALSE)),p99=as.numeric(quantile(z_veg,.99,names=FALSE)),top10=top_mean(z_veg,.10),top20=top_mean(z_veg,.20),top30=top_mean(z_veg,.30))
}
metrics_all_trees <- function(las,coords,min_veg_h,radius,chunk_size,label,p0,p1) {
  n <- nrow(coords); out <- vector('list',n); starts <- seq.int(1L,n,by=max(1L,chunk_size))
  for(s in starts) { e <- min(n,s+chunk_size-1L); ix <- s:e
    clips <- tryCatch(clip_circle(las,xcenter=coords[ix,1],ycenter=coords[ix,2],radius=radius),error=function(err)NULL)
    if(!(is.list(clips)&&length(clips)==length(ix))) { clips <- vector('list',length(ix)); for(j in seq_along(ix)) { k<-ix[j]; clips[[j]]<-tryCatch(clip_circle(las,xcenter=coords[k,1],ycenter=coords[k,2],radius=radius),error=function(err)NULL) } }
    out[ix] <- lapply(clips,metrics_one_clip,min_veg_h=min_veg_h)
    frac <- e/n; detail <- sprintf('%s: %s / %s TREE_ID (%.1f%%)',label,format(e,big.mark=','),format(n,big.mark=','),frac*100); write_log('INFO',detail); progress_event('metrics',label,p0+(p1-p0)*frac,detail)
  }
  bind_rows(out)
}
presence_support <- function(m) { cuts <- list(n_all=low_fence(m$n_all),n_veg=low_fence(m$n_veg),ratio=low_fence(m$veg_ratio)); flags <- data.frame(data_ok=is.finite(m$n_all)&m$n_all>=cuts$n_all,canopy_support=is.finite(m$p95)&is.finite(m$n_veg)&is.finite(m$veg_ratio)&m$n_veg>=cuts$n_veg&m$veg_ratio>=cuts$ratio); list(flags=flags,cuts=cuts) }
sample_ground_df <- function(ground,max_points=0L,seed=42L) { n<-npoints(ground); if(n<=0L)return(data.frame()); idx<-seq_len(n); if(max_points>0L&&n>max_points){set.seed(seed);idx<-sort(sample(idx,max_points))}; data.frame(X=ground$X[idx],Y=ground$Y[idx],Z=ground$Z[idx]) }
terrain_holdout_qc <- function(las_grounded,dtm_res,train_frac,seed,min_ground,max_ground) {
  ground<-filter_ground(las_grounded); n_total<-npoints(ground); empty<-function(nu=n_total)data.frame(n_ground=n_total,n_used=nu,rmse=NA_real_,median=NA_real_,nmad=NA_real_,p95_abs=NA_real_,method='HOLDOUT_CSF')
  if(n_total<min_ground)return(empty()); gdf<-sample_ground_df(ground,max_ground,seed); if(nrow(gdf)<min_ground)return(empty(nrow(gdf)))
  set.seed(seed); n_train<-max(3L,floor(train_frac*nrow(gdf))); idx<-sample(seq_len(nrow(gdf)),size=n_train); tr<-gdf[idx,,drop=FALSE]; te<-gdf[-idx,,drop=FALSE]; if(nrow(te)<3L)return(empty(nrow(gdf)))
  train_las<-LAS(data.frame(X=tr$X,Y=tr$Y,Z=tr$Z,Classification=2L)); st_crs(train_las)<-st_crs(las_grounded); dtm_train<-rasterize_terrain(train_las,res=dtm_res,algorithm=tin())
  tv<-vect(te,geom=c('X','Y'),crs=crs(dtm_train)); zhat<-extract(dtm_train,tv)[,2]; r<-te$Z-zhat; r<-r[is.finite(r)]; if(length(r)<3L)return(empty(nrow(gdf)))
  med<-median(r); r0<-r-med; data.frame(n_ground=n_total,n_used=nrow(gdf),rmse=sqrt(mean(r0^2)),median=med,nmad=1.4826*median(abs(r0)),p95_abs=as.numeric(quantile(abs(r0),.95,names=FALSE)),method='HOLDOUT_CSF')
}
terrain_external_qc <- function(las_grounded,dtm,min_ground,max_ground,seed) {
  ground<-filter_ground(las_grounded); n_total<-npoints(ground); empty<-function(nu=n_total)data.frame(n_ground=n_total,n_used=nu,rmse=NA_real_,median=NA_real_,nmad=NA_real_,p95_abs=NA_real_,method='GROUND_VS_EXTERNAL')
  if(n_total<min_ground)return(empty()); gdf<-sample_ground_df(ground,max_ground,seed); v<-vect(gdf,geom=c('X','Y'),crs=crs(dtm)); zhat<-extract(dtm,v)[,2]; r<-gdf$Z-zhat; r<-r[is.finite(r)]; if(length(r)<3L)return(empty(nrow(gdf)))
  med<-median(r); r0<-r-med; data.frame(n_ground=n_total,n_used=nrow(gdf),rmse=sqrt(mean(r0^2)),median=med,nmad=1.4826*median(abs(r0)),p95_abs=as.numeric(quantile(abs(r0),.95,names=FALSE)),method='GROUND_VS_EXTERNAL')
}
ensure_dtm_crs <- function(dtm,las_crs) { dc<-st_crs(crs(dtm)); if(is.na(dc))stop_run('CRS external DTM kosong.'); if(!identical(las_crs$wkt,dc$wkt))stop_run('CRS external DTM berbeda dengan LAS. Samakan CRS terlebih dahulu.') }
make_report <- function(summary,preview,path) { esc<-function(x){x<-as.character(x);x<-gsub('&','&amp;',x,fixed=TRUE);x<-gsub('<','&lt;',x,fixed=TRUE);x<-gsub('>','&gt;',x,fixed=TRUE);x}; scalar<-vapply(summary,function(x)length(x)==1L&&!is.list(x),logical(1)); rows<-paste0('<tr><th>',esc(names(summary)[scalar]),'</th><td>',esc(vapply(summary[scalar],function(x)ifelse(is.null(x)||is.na(x),'',as.character(x)),character(1))),'</td></tr>',collapse=''); ph<-''; if(nrow(preview)){headrow<-paste0('<th>',esc(names(preview)),'</th>',collapse='');body<-apply(preview,1,function(r)paste0('<tr>',paste0('<td>',esc(r),'</td>',collapse=''),'</tr>'));ph<-paste0('<h2>Preview metrics</h2><table><tr>',headrow,'</tr>',paste(body,collapse=''),'</table>')}; writeLines(paste0('<!doctype html><html><head><meta charset="utf-8"><style>body{font-family:Segoe UI;background:#0b0f11;color:#e7edee;padding:28px}h1,h2{color:#e3a73f}table{border-collapse:collapse;width:100%}th,td{border:1px solid #263135;padding:7px;text-align:left}th{color:#e3a73f;background:#171f22}</style></head><body><h1>SawitHeight R — Period Package</h1><table>',rows,'</table>',ph,'</body></html>'),path,useBytes=TRUE) }

validate_config <- function(deep=FALSE) {
  set_stage('validation','Validasi input satu periode',2)
  if(!file_ok(inputs$point_cloud))stop_run('Point cloud tidak ditemukan.'); if(!file_ok(inputs$tree_points))stop_run('Titik pokok tidak ditemukan.'); if(!nzchar(as.character(inputs$output_root %||% '')))stop_run('Folder output belum ditentukan.')
  period<-clean_period(p$period_code); idf<-trimws(as.character(p$tree_id_field %||% '')); if(!nzchar(idf))stop_run('Field TREE_ID belum diisi.'); if(!as_bool(p$xy_alignment_confirmed,FALSE))stop_run('Konfirmasi XY alignment belum dicentang.')
  tm<-toupper(as.character(p$terrain_mode %||% 'CSF_TIN')); if(!(tm%in%c('CSF_TIN','EXTERNAL_DTM')))stop_run('Terrain mode tidak valid.'); if(tm=='EXTERNAL_DTM'&&!file_ok(inputs$external_dtm))stop_run('External DTM tidak ditemukan.')
  if(deep){ trees<-suppressWarnings(st_read(inputs$tree_points,quiet=TRUE)); if(!(idf%in%names(trees)))stop_run('Field ID pohon tidak ditemukan: ',idf); if(any(is.na(trees[[idf]]))||anyDuplicated(trees[[idf]])>0L)stop_run('TREE_ID harus non-NA dan unik.'); peer<-trimws(as.character(p$peer_field %||% '')); if(nzchar(peer)&&!(peer%in%names(trees)))stop_run('Peer field tidak ditemukan: ',peer); emit('validation-result',status='success',message=paste('Validasi periode',period,'berhasil;',nrow(trees),'TREE_ID.')) }
  invisible(TRUE)
}

run_pipeline <- function() {
  validate_config(FALSE); PERIOD<-clean_period(p$period_code); idf<-trimws(as.character(p$tree_id_field)); peer_field<-trimws(as.character(p$peer_field %||% '')); tm<-toupper(as.character(p$terrain_mode %||% 'CSF_TIN'))
  buffer_m<-as_num(p$buffer_radius_m,2); min_veg<-as_num(p$min_veg_h_m,.30); dtm_res<-as_num(p$dtm_resolution_m,.5); train_frac<-as_num(p$holdout_train_frac,.8); seed<-as_int(p$holdout_seed,42L); min_ground<-as_int(p$holdout_min_ground,100L); max_ground<-as_int(p$holdout_max_ground,200000L); chunk<-as_int(p$metrics_chunk_size,250L); try(set_lidr_threads(as_int(p$threads,4L)),silent=TRUE)
  set_stage('load',paste('Load point cloud',PERIOD),5); write_log('INFO',paste('Point cloud:',inputs$point_cloud)); las<-readLAS(inputs$point_cloud); if(is.null(las)||npoints(las)==0L)stop_run('Point cloud kosong/gagal dibaca.'); input_n<-npoints(las); write_log('INFO',paste('Jumlah titik awal:',format(input_n,big.mark=',')))
  epsg<-as_int(p$fallback_epsg,0L); if(is.na(st_crs(las))&&epsg>0L)st_crs(las)<-epsg; if(is.na(st_crs(las)))stop_run('CRS LAS kosong. Isi fallback EPSG.'); if(isTRUE(st_is_longlat(st_crs(las))))stop_run('CRS masih geographic/derajat. Gunakan CRS proyeksi meter.')
  trees<-suppressWarnings(st_read(inputs$tree_points,quiet=TRUE)); if(!(idf%in%names(trees)))stop_run('Field ID pohon tidak ditemukan: ',idf); if(any(is.na(trees[[idf]]))||anyDuplicated(trees[[idf]])>0L)stop_run('TREE_ID harus non-NA dan unik.'); if(any(as.character(st_geometry_type(trees))=='MULTIPOINT'))trees<-st_cast(trees,'POINT'); if(!all(as.character(st_geometry_type(trees))=='POINT'))stop_run('TREE_FILE harus POINT.'); trees<-st_transform(trees,st_crs(las)); trees$tree_id<-as.character(trees[[idf]]); coords<-st_coordinates(trees); if(nrow(coords)!=nrow(trees))stop_run('Koordinat TREE_ID tidak satu-ke-satu.'); write_log('INFO',paste('TREE_ID:',nrow(trees)))
  set_stage('qc','QC point cloud',12); if(as_bool(p$remove_duplicates,TRUE)){n0<-npoints(las);write_log('INFO','Hapus duplikat...');las<-filter_duplicates(las);write_log('INFO',paste('Duplikat dibuang:',n0-npoints(las)))}; if(as_bool(p$run_noise_filter,TRUE)){n0<-npoints(las);write_log('INFO','SOR noise filtering...');las<-tryCatch({tmp<-classify_noise(las,sor(k=as_int(p$sor_k,10L),m=as_num(p$sor_m,3)));filter_poi(tmp,Classification!=18L)},error=function(e){write_log('WARNING',paste('Noise filter dilewati:',conditionMessage(e)));las});write_log('INFO',paste('Noise points dibuang:',n0-npoints(las)))}
  dens<-rasterize_density(las,res=1); avg_density<-tryCatch(as.numeric(global(dens,'mean',na.rm=TRUE)[1,1]),error=function(e)NA_real_); write_log('INFO',paste('Average density:',round(avg_density,2),'points/m2'))
  set_stage('registration','Audit metadata positioning / registration',20); pos<-as.character(p$positioning_quality %||% 'UNKNOWN'); master<-trimws(as.character(p$master_period_code %||% '')); write_log('INFO',paste('Positioning quality:',pos)); if(nzchar(master))write_log('INFO',paste('Master registration period (metadata):',master)); write_log('INFO','Period backend tidak melakukan transformasi. Jika alignment belum lolos QC, jalankan modul Registration QC / Master Alignment terlebih dahulu.')
  set_stage('ground','Ground classification CSF',27); write_log('INFO','classify_ground CSF dimulai...'); g<-classify_ground(las,csf(cloth_resolution=as_num(p$csf_cloth_resolution,.5),class_threshold=as_num(p$csf_class_threshold,.3),rigidness=as_int(p$csf_rigidness,2L))); n_ground<-sum(g$Classification==2L,na.rm=TRUE); pct_ground<-n_ground/npoints(g)*100; write_log('INFO',sprintf('Ground Class 2: %s (%.2f%%)',format(n_ground,big.mark=','),pct_ground)); if(n_ground==0L)stop_run('Tidak ada ground hasil CSF.')
  set_stage('terrain','Build / load terrain',35); if(tm=='CSF_TIN'){write_log('INFO','rasterize_terrain TIN...');dtm<-rasterize_terrain(g,res=dtm_res,algorithm=tin());dtm_path<-file.path(run_dir,paste0('DTM_',PERIOD,'.tif'));writeRaster(dtm,dtm_path,overwrite=TRUE);dtm_generated<-TRUE}else{write_log('INFO',paste('Load external DTM:',inputs$external_dtm));dtm<-rast(inputs$external_dtm);ensure_dtm_crs(dtm,st_crs(g));dtm_path<-normalizePath(inputs$external_dtm,winslash='/',mustWork=FALSE);dtm_generated<-FALSE}
  set_stage('terrain_qc','Terrain hold-out / external ground QC',43); tq<-if(tm=='CSF_TIN')terrain_holdout_qc(g,dtm_res,train_frac,seed,min_ground,max_ground)else terrain_external_qc(g,dtm,min_ground,max_ground,seed); write_log('INFO',paste('Terrain QC:',paste(names(tq),unlist(tq[1,]),sep='=',collapse=' | '))); terrain_qc_csv<-file.path(qc_dir,paste0('terrain_qc_',PERIOD,'.csv'));write.csv(tq,terrain_qc_csv,row.names=FALSE)
  set_stage('normalize','Normalisasi tinggi',52); write_log('INFO','normalize_height dimulai...'); nlas<-normalize_height(g,dtm); nlas<-filter_poi(nlas,Z>=as_num(p$min_normalized_z_m,-.10)); write_log('INFO',paste('Normalized points:',format(npoints(nlas),big.mark=','))); norm_path<-''; if(as_bool(p$save_normalized_laz,TRUE)){norm_path<-file.path(run_dir,paste0(PERIOD,'_normalized.laz'));write_log('INFO','Simpan normalized LAZ...');writeLAS(nlas,norm_path)}
  chm_path<-''; if(as_bool(p$create_nchm,TRUE)){write_log('INFO','Generate nCHM untuk visual/QC...');chm<-rasterize_canopy(nlas,res=as_num(p$chm_resolution_m,.10),algorithm=p2r());chm_path<-file.path(run_dir,paste0('nCHM_',PERIOD,'.tif'));writeRaster(chm,chm_path,overwrite=TRUE)}
  set_stage('metrics','Ekstraksi P95 & presence support',62); met<-metrics_all_trees(nlas,coords,min_veg,buffer_m,chunk,paste('Metrics',PERIOD),62,88); if(nrow(met)!=nrow(trees))stop_run('Jumlah metrics tidak sama dengan TREE_ID.'); prs<-presence_support(met); metrics<-bind_cols(data.frame(tree_id=trees$tree_id,stringsAsFactors=FALSE),met,prs$flags); if(nzchar(peer_field))metrics$peer_group<-as.character(trees[[peer_field]])else metrics$peer_group<-NA_character_
  metrics_csv<-file.path(run_dir,paste0('metrics_',PERIOD,'.csv'));write.csv(metrics,metrics_csv,row.names=FALSE)
  shp_attr<-metrics%>%transmute(tree_id=tree_id,peer_grp=substr(peer_group,1,40),n_all=n_all,n_veg=n_veg,veg_ratio=veg_ratio,p95=p95,p99=p99,t10=top10,t20=top20,t30=top30,data_ok=data_ok,can_sup=canopy_support); metrics_sf<-trees%>%select(tree_id)%>%left_join(shp_attr,by='tree_id'); metrics_shp<-file.path(shp_dir,paste0('metrics_',PERIOD,'.shp'));safe_write_sf(metrics_sf,metrics_shp)
  set_stage('export','Simpan period package',92); support_csv<-file.path(qc_dir,paste0('presence_cuts_',PERIOD,'.csv'));write.csv(data.frame(n_all_cut=prs$cuts$n_all,n_veg_cut=prs$cuts$n_veg,veg_ratio_cut=prs$cuts$ratio),support_csv,row.names=FALSE)
  qc_plot_files<-character(0)
  if(as_bool(p$create_qc_plots,TRUE)){
    try({
      x<-metrics$p95[is.finite(metrics$p95)]
      if(length(x)>1L){f<-file.path(qc_dir,paste0('hist_p95_',PERIOD,'.png'));png(f,1600,1000,res=150);hist(x,breaks='FD',main=paste('Distribusi P95',PERIOD),xlab='P95 normalized height (m)');dev.off();qc_plot_files<-c(qc_plot_files,f)}
    },silent=TRUE)
    try({
      x<-metrics$n_all[is.finite(metrics$n_all)]
      if(length(x)>1L){f<-file.path(qc_dir,paste0('hist_point_support_',PERIOD,'.png'));png(f,1600,1000,res=150);hist(x,breaks='FD',main=paste('Point Support',PERIOD),xlab='n_all points per buffer');dev.off();qc_plot_files<-c(qc_plot_files,f)}
    },silent=TRUE)
  }
  summary<-list(task='period',app_version=as.character(cfg$app$version%||%'0.8.0'),run_dir=run_dir,period_code=PERIOD,tree_count=nrow(trees),input_points=input_n,density_m2=avg_density,ground_count=n_ground,ground_pct=pct_ground,terrain_mode=tm,terrain_nmad_m=as_num(tq$nmad),terrain_rmse_m=as_num(tq$rmse),terrain_p95_abs_m=as_num(tq$p95_abs),p95_median=if(any(is.finite(metrics$p95)))median(metrics$p95,na.rm=TRUE)else NA_real_,data_ok_count=sum(metrics$data_ok%in%TRUE),canopy_support_count=sum(metrics$canopy_support%in%TRUE),buffer_radius_m=buffer_m,min_veg_h_m=min_veg,positioning_quality=pos,master_period_code=if(nzchar(master))master else NA_character_)
  preview<-head(metrics,25); summary$preview<-preview
  summary_path<-file.path(run_dir,'period_summary.json'); jsonlite::write_json(summary,summary_path,pretty=TRUE,auto_unbox=TRUE,na='null')
  # Manifest memakai path relatif yang eksplisit agar package dapat dipindahkan.
  manifest<-list(schema_version='1.0',package_type='SAWITHEIGHT_PERIOD',app_version=summary$app_version,period_code=PERIOD,created_at=format(Sys.time(),'%Y-%m-%dT%H:%M:%S'),positioning_quality=pos,master_period_code=if(nzchar(master))master else NULL,xy_alignment_confirmed=TRUE,terrain_mode=tm,tree_id_field=idf,peer_field=if(nzchar(peer_field))peer_field else NULL,methodology=list(estimator='P95',buffer_radius_m=buffer_m,min_veg_h_m=min_veg,normalized_height=TRUE),terrain_qc=list(nmad_m=summary$terrain_nmad_m,rmse_m=summary$terrain_rmse_m,p95_abs_m=summary$terrain_p95_abs_m),support_cuts=list(n_all=prs$cuts$n_all,n_veg=prs$cuts$n_veg,veg_ratio=prs$cuts$ratio),files=list(metrics_csv=basename(metrics_csv),metrics_shp=file.path('shapefile',basename(metrics_shp)),period_summary=basename(summary_path),terrain_qc=file.path('qc',basename(terrain_qc_csv)),presence_cuts=file.path('qc',basename(support_csv)),dtm=if(dtm_generated)basename(dtm_path) else NULL,normalized_laz=if(nzchar(norm_path))basename(norm_path) else NULL,nchm=if(nzchar(chm_path))basename(chm_path) else NULL))
  manifest_path<-file.path(run_dir,'period_manifest.json');jsonlite::write_json(manifest,manifest_path,pretty=TRUE,auto_unbox=TRUE,na='null')
  report_path<-file.path(run_dir,paste0('report_period_',PERIOD,'.html'));make_report(summary,preview,report_path)
  outputs<-c(metrics_csv,metrics_shp,summary_path,manifest_path,terrain_qc_csv,support_csv,if(dtm_generated)dtm_path else '',norm_path,chm_path,qc_plot_files,report_path,log_path);outputs<-unique(outputs[nzchar(outputs)&file.exists(outputs)]);summary$output_files<-outputs;jsonlite::write_json(summary,summary_path,pretty=TRUE,auto_unbox=TRUE,na='null')
  manifest_csv<-file.path(run_dir,'output_manifest.csv');write.csv(data.frame(file=basename(outputs),path=outputs),manifest_csv,row.names=FALSE);summary$output_files<-unique(c(outputs,manifest_csv));jsonlite::write_json(summary,summary_path,pretty=TRUE,auto_unbox=TRUE,na='null')
  set_stage('complete',paste('Period package',PERIOD,'selesai'),100);emit('result',task='period',status='success',runDir=run_dir,summaryPath=summary_path,reportPath=report_path,summary=summary);write_log('INFO','Period processing selesai.')
}

status_code<-0L
withCallingHandlers(tryCatch({if(mode=='validate'){validate_config(TRUE);set_stage('complete','Validasi periode berhasil',100);emit('validation-result',status='success',message='Input satu periode valid.')}else run_pipeline()},error=function(e){status_code<<-10L;msg<-conditionMessage(e);try(cat(sprintf('[%s] [FATAL] [%s] %s\n',format(Sys.time(),'%Y-%m-%d %H:%M:%S'),current_stage,msg),file=log_path,append=TRUE),silent=TRUE);emit('fatal',stage=current_stage,message=msg,runDir=run_dir,logPath=log_path)}),warning=function(w){try(write_log('WARNING',conditionMessage(w)),silent=TRUE);invokeRestart('muffleWarning')})
quit(status=status_code)
