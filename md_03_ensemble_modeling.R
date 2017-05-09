##########################################################################################
## Purpose: Species distribution modelling of coffee robusta; binary outputs and means
## Author: Eliana Vallejo
##dpto Risaralda
##########################################################################################
list.of.packages <- c("raster","sp")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(raster);library(sp)
###params
bdir<-"//dapadfs/Workspace_cluster_9/USAID_Project/Product_6_resilient_coffee/04-suitability/results"
eval<-lapply(list.files(paste0(bdir,"/evaluacion de modelos"),pattern=glob2rx(("*eval*.csv*")),full.names=T),read.csv)
eval<-ldply(eval)
bin<-eval[eval$eval%in%"ROC"& eval$metrics%in%"Cutoff" & eval$run%in%"Full",]
rcp<-c("rcp26","rcp45","rcp60","rcp85")
futuro<-c(20,30)
ideam_models<-c("giss_e2_h","bcc_csm1_1", "ncar_ccsm4","csiro_mk3_6_0","fio_esm","gfdl_cm3","giss_e2_r","nimr_hadgem2_ao","ipsl_cm5a_lr", "ipsl_cm5a_mr",
                "miroc_esm","miroc_esm_chem","miroc_miroc5","mri_cgcm3")
###end params
########functions
fun_bin<-function(x){ r=model1[[x]]
if(sum(grep(bin$model[1],names(model1[[x]]),NA))){
  r[]<-ifelse(r[]>bin$value[1],1,0) 
}
if(sum(grep(bin$model[2],names(model1[[x]]),NA))){
  r[]<-ifelse(r[]>bin$value[2],1,0) 
}
if(sum(grep(bin$model[3],names(model1[[x]]),NA))){
  r[]<-ifelse(r[]>bin$value[3],1,0) 
}
if(sum(grep(bin$model[4],names(model1[[x]]),NA))){
  r[]<-ifelse(r[]>bin$value[4],1,0) 
}
if(sum(grep(bin$model[5],names(model1[[x]]),NA))){
  r[]<-ifelse(r[]>bin$value[5],1,0) 
}
return(r)}

## Medias Ajustadas
trimedia_tukey <- function(x){
  y1 <- quantile(x, probs = 0.25, na.rm = T)
  y2 <- quantile(x, probs = 0.5, na.rm = T)
  y3 <- quantile(x, probs = 0.75, na.rm = T)
  promedio <- 0.25*y1+0.5*y2+0.25*y3
  return(promedio)
}
###end functions
######################################################################################
###calcula binarios futuros y la media de cada escenario y modelo 
###current
idir_f<-paste0(bdir,"/current")
odir_f<-paste0(idir_f,"/binario")
odir_fe<-paste0(idir_f,"/ensemble")
if (!file.exists(odir_f)) {dir.create(odir_f,recursive=T)} else {cat("Folder exists")}
if (!file.exists(odir_fe)) {dir.create(odir_fe,recursive=T)} else {cat("Folder exists")}
model1<-stack(lapply(list.files(idir_f,full.names = T,pattern=".tif"),function(x)stack(x)))
binario<-lapply(1:length(names(model1)),fun_bin)
names(binario)<-names(model1)
binario<-stack(binario)
sum_binario<-(sum(binario)/length(names(model1)))*100
sum_binario[which(sum_binario[]>100)]=NA
names(sum_binario)<-paste0("p_suit_current")
##descriptivas
media<-calc(model1,trimedia_tukey)
names(media)<-paste0("media_ajustada_")
writeRaster(stack(binario),filename=paste0(odir_f,"/",names(binario)), bylayer=TRUE,format="GTiff",overwrite=TRUE )
writeRaster(sum_binario,filename=paste0(odir_fe,"/",names(sum_binario)),format="GTiff",overwrite=TRUE )
writeRaster(media,filename=paste0(odir_fe,"/",names(media)),format="GTiff",overwrite=TRUE )
########################
for(m in 1:length(ideam_models)){
  for(f in 1:length(rcp)){
    for(fut in 1:length(futuro)){
      idir_f<-bdir
      fut<-paste0("futuro_",futuro[fut])
      rcpv<-rcp[f]
      model=ideam_models[m]
      idir_f<-paste0(idir_f,"/",fut,"/",rcpv,"/",model)
      odir_f<-paste0(idir_f,"/binario")
      odir_fe<-paste0(idir_f,"/ensemble")
      if (!file.exists(odir_f)) {dir.create(odir_f,recursive=T)} else {cat("Folder exists")}
      if (!file.exists(odir_fe)) {dir.create(odir_fe,recursive=T)} else {cat("Folder exists")}
      model1<-stack(lapply(list.files(idir_f,full.names = T,pattern=".tif"),function(x)stack(x)))
      binario<-lapply(1:length(names(model1)),fun_bin)
      names(binario)<-names(model1)
      binario<-stack(binario)
      sum_binario<-(sum(binario)/length(names(model1)))*100
      sum_binario[which(sum_binario[]>100)]=NA
      names(sum_binario)<-paste0("p_suit_binario_",model)
      ##descriptivas
      media<-calc(model1,trimedia_tukey)
      names(media)<-paste0("media_ajustada_",model)
      writeRaster(stack(binario),filename=paste0(odir_f,"/",names(binario)), bylayer=TRUE,format="GTiff",overwrite=TRUE )
      writeRaster(sum_binario,filename=paste0(odir_fe,"/",names(sum_binario)),format="GTiff",overwrite=TRUE )
      writeRaster(media,filename=paste0(odir_fe,"/",names(media)),format="GTiff",overwrite=TRUE )
    }}}

###################################################################################################
###########################resume por escenario los resultados de cada modelo GCM###################
##read ensemble
for(f in 1:length(rcp)){
  for(fut in 1:length(futuro)){
    idir_f<-bdir
    fut<-paste0("futuro_",futuro[fut])
    rcpv<-rcp[f]
    idir_f<-paste0(idir_f,"/",fut,"/",rcpv,"/")
    # odir_fe<-paste0(idir_f,"/ensemble")
    dirs<- unlist(lapply(ideam_models,function(x)paste0(idir_f,x,"/ensemble")))
    bin<- stack(unlist(lapply(dirs,function(x)list.files(x,pattern=glob2rx("*p_suit*.tif*"),full.names=T))))
    bin_mean<-raster::calc(bin,function(x){trimedia_tukey(x)})
    names(bin_mean)<-"concenso"
    suit<- stack(unlist(lapply(dirs,function(x)list.files(x,pattern=glob2rx("*media_ajustada*.tif*"),full.names=T))))
    suit_mean<-raster::calc(suit,function(x){trimedia_tukey(x)/10})
    names(suit_mean)<-"suit_mean"
    # dv<- stack(unlist(lapply(dirs,function(x)list.files(x,pattern=glob2rx("*dv*.tif*"),full.names=T))))
    #dv_mean<-raster::calc(dv,function(x){trimedia_tukey(x)/10})
    writeRaster(suit_mean,paste0("//dapadfs/Workspace_cluster_9/USAID_Project/Product_6_resilient_coffee/04-suitability/results/ensemble_modeling/",names(suit_mean),"_" ,fut,"_",rcpv),format="GTiff",overwrite=TRUE)        
    writeRaster(bin_mean,paste0("//dapadfs/Workspace_cluster_9/USAID_Project/Product_6_resilient_coffee/04-suitability/results/ensemble_modeling/",names(bin_mean),"_" ,fut,"_",rcpv),format="GTiff",overwrite=TRUE)
  }}
########################resume los resultados de cada RCP por cada uno de los sdm ajustados
############################by model
##read ensemble
models<-c("GAM","GLM","RF","GBM", "MAXENT.Tsuruoka")
for(m in 1:length(models)){
  for(f in 1:length(rcp)){
    for(fut in 1:length(futuro)){
      #   f=1;fut=1;m=2
      idir_f<-bdir
      fut<-paste0("futuro_",futuro[fut])
      rcpv<-rcp[f]
      idir_f<-paste0(idir_f,"/",fut,"/",rcpv,"/")
      dirs<- unlist(lapply(ideam_models,function(x)paste0(idir_f,x,"/")))
      m_m<- stack(unlist(lapply(dirs,function(x)list.files(x,pattern=glob2rx(paste0("*",models[m],"*.tif*")),full.names=T))))
      media<-calc(m_m,trimedia_tukey)/10
      names(media)<-"suit_mean"
      #dv<-calc(m_m,sd)
      #dirs<- unlist(lapply(ideam_models,function(x)paste0(idir_f,x,"/binario")))
      #bin<-stack(unlist(lapply(dirs,function(x)list.files(x,pattern=glob2rx(paste0("*",models[m],"*.tif*")),full.names=T))))
      #bin_mean<-raster::calc(bin,function(x){mean(x)})
      #names(bin_mean)<-"concenso"
      writeRaster(media,paste0("//dapadfs/Workspace_cluster_9/USAID_Project/Product_6_resilient_coffee/04-suitability/results/ensemble_modeling/",names(media),"_" ,fut,"_",rcpv,"_",models[m]),format="GTiff",overwrite=TRUE)        
      #writeRaster(bin_mean,paste0("//dapadfs/Workspace_cluster_9/USAID_Project/Product_6_resilient_coffee/04-suitability/results/ensemble_modeling/",names(bin_mean),"_" ,fut,"_",rcpv,"_",models[m]),format="GTiff",overwrite=TRUE)
    }}}