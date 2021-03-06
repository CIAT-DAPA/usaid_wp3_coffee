############################################################################
#######################EVENTOS EXTREMOS RISARALDA
###OBJETIVO: Identificar valores atipicos en las variables agro-climaticas
require(raster)
require(rasterVis)
require(maptools)
require(rgdal)
library(sp)
######PARAMS
year_from<-2000
year_to<-2010 ###es el a�o final de la linea base
percentile<-c(.25,.75) ##percentiles para definir los limites de atipicidad
error_t<-1.5 ##numero de desviaciones a partir del cual se considerara un dato atipico para temperatura
error_p<-1.5 ##numero de desviaciones a partir del cual se considerara un dato atipico para precipitaci�n
path_in<-"//dapadfs/Workspace_cluster_9/USAID_Project/Product_6_resilient_coffee/02-monthly-interpolations/outputs_yearly_v2_2011_2016/average/"
path_in1<-"//dapadfs/Workspace_cluster_9/USAID_Project/Product_6_resilient_coffee/02-monthly-interpolations/outputs_yearly_v2/average/"
dir_out<-"D:/ToBackup/Unidad Z/Usaid_Fedecafe/resultados id eventos extremos/raster_results"
tr<-"no"  ##yes or no, recomendado para transformar precipitaci�n y corregirle un poco la asimetria
varlist <- c("prec","tmax","tmin","tmean")
#varlist <- "prec"
mask<-readOGR(paste0(dir_out,"/mask/CapaRisaralda_WGS84.shp"), layer= "CapaRisaralda_WGS84")
seasons <- "monthly" #seasons, monthly or quarter

if(seasons=="monthly"){
  period<-as.list(1:12)
}
if(seasons=="quarter"){
  period<-list( "djf"=c(12,1,2), "mam"=3:5, "jja"=6:8, "son"=9:11) 
}
for (var in 1:length(varlist)){
for(i in 1:length(period)){
if(year_to<=2010){
  files1<-paste0(path_in1,varlist[var],"_",year_from:year_to,"_",period[[i]],".asc")
  layers_p<-stack(files1)
}else{
  
  files1<-paste0(path_in1,varlist[var],"_",year_from:2010,"_",period[[i]],".asc")
  layers_p1<-stack(files1)
  files<-paste0(path_in,varlist[var],"_",2011:year_to,"_",period[[i]],".asc")
  layers_p<-stack(files)
  layers_p<-stack(layers_p1,layers_p);rm(layers_p1)

}
  crs(layers_p) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  layers_p<-mask(layers_p,mask)
  layers_p<-stack(layers_p)
      ###establece limites de atipicidad
     if(tr=="yes"&varlist[var]=="prec"){
        layers_p<-log1p(layers_p)
        
      }else{
        layers_p<-layers_p
      }
     norm<-calc(layers_p,function(x)mean(x,na.rm=T))  
    if(varlist[var]=="prec"){
      r_low<- calc(layers_p, function(x)quantile(x,percentile[1],na.rm=T)-(error_p*IQR(x,na.rm=T)))
      r_low[which(r_low[]<0)]=0
      r_up<-  calc(layers_p, function(x)quantile(x,percentile[2],na.rm=T)+(error_p*IQR(x,na.rm=T)))
    }
     if( varlist[var]!="prec"){
      r_low<- calc(layers_p, function(x)quantile(x,percentile[1],na.rm=T)-(error_t*IQR(x,na.rm=T)))
      r_up<-  calc(layers_p, function(x)quantile(x,percentile[2],na.rm=T)+(error_t*IQR(x,na.rm=T)))
     }
   
      for(s in 1:nlayers(layers_p)){
        if(varlist[var]=="prec"){
    low_p <- overlay(x =layers_p[[s]], y = r_low, fun = function(x, y) ifelse(x <=y, x, 0))
    up_p<-overlay(x =layers_p[[s]], y = r_up, fun = function(x, y) ifelse(x >=y, x, 0))
    anomals<-overlay(x =layers_p[[s]], y = norm, fun = function(x, y)(x/y)*100 )
        } ##end conditional outlier
        if( varlist[var]!="prec"){ 
          low_p <- overlay(x =layers_p[[s]], y = r_low, fun = function(x, y) ifelse(x <=y, x, 0))   #% de alejamiento del atipico por debajo del limite
          up_p<-overlay(x =layers_p[[s]], y = r_up, fun = function(x, y) ifelse(x >=y, x, 0))  #% de alejamiento del atipico por encima del limite
          anomals<-overlay(x =layers_p[[s]], y = norm, fun = function(x, y)(y/10)-(x/10))  
        }
        
        if(tr=="yes"&varlist[var]=="prec"&quantile(low_p[],1,na.rm=T)!=0){
        writeRaster(crop(exp(low_p)-1,mask), filename=paste0(dir_out,"/outliers_low_",names(layers_p[[s]])),format="GTiff")
      }
        if(tr=="no"&quantile(low_p[],1,na.rm=T)!=0){
        writeRaster(crop(low_p,mask), filename=paste0(dir_out,"/outliers_low_",names(layers_p[[s]])),format="GTiff")
         }
       
        if(tr=="yes"&varlist[var]=="prec"&quantile(up_p[],1,na.rm=T)!=0){
            writeRaster(crop(exp(up_p)-1,mask), filename=paste0(dir_out,"/outliers_up_",names(layers_p[[s]])),format="GTiff")
          }
        if(tr=="no"&quantile(up_p[],1,na.rm=T)!=0){
            writeRaster(crop(up_p,mask), filename=paste0(dir_out,"/outliers_up_",names(layers_p[[s]])),format="GTiff")
          }
        
             
   writeRaster(crop(anomals,mask), filename=paste0(dir_out,"/anomalias_respec_normal_",names(layers_p[[s]])),format="GTiff")
   }#end cicle
    if(tr=="yes"&varlist[var]=="prec"){
    writeRaster(crop(exp(r_low)+1,mask), filename=paste0(dir_out,"/lim_low_",varlist[var],"_",year_from,"_",year_to,"_",period[[i]]),format="GTiff",overwrite=TRUE )
    writeRaster(crop(exp(r_up)+1,mask), filename=paste0(dir_out,"/lim_up_",varlist[var],"_",year_from,"_",year_to,"_",period[[i]]),format="GTiff",overwrite=TRUE )
    writeRaster(crop(exp(norm)+1,mask), filename=paste0(dir_out,"/climatology_",varlist[var],"_",period[[i]]),format="GTiff",overwrite=TRUE )
    }else{
      writeRaster(crop(r_low,mask), filename=paste0(dir_out,"/lim_low_",varlist[var],"_",year_from,"_",year_to,"_",period[[i]]),format="GTiff",overwrite=TRUE )
      writeRaster(crop(r_up,mask), filename=paste0(dir_out,"/lim_up_",varlist[var],"_",year_from,"_",year_to,"_",period[[i]]),format="GTiff",overwrite=TRUE )
      writeRaster(crop(norm,mask), filename=paste0(dir_out,"/climatology_",varlist[var],"_",period[[i]]),format="GTiff",overwrite=TRUE )
    }
    }
}


