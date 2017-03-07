##########################################################################################
## Purpose: Calculate anomalies from CIAT data CMIP5
## Author: Eliana Vallejo
##dpto Risaralda
##########################################################################################
library(gstat);require(rgdal);library(sp)
library(dplyr);library(plyr);library(reshape2)
#v=1
#m=1
#f=1
#####parameters
lat_s<-3.5
lat_u<-6.5
long_s<-(-77.5)
long_u<-(-74.5)
#var_list<-c("prec","tmin","tmax")
var_list<-"prec"
#rcp<-c("RCP26","RCP45","RCP85")
rcp<-"rcp45"
year_from_h<-1981
year_to_h<-2010
# "giss_e2_h",  "ipsl_cm5a_mr" este modelo no tiene prec para el rcp45
ideam_models<-c("bcc_csm1_1",   "ncar_ccsm4","csiro_mk3_6_0","fio_esm","gfdl_cm3",
             "giss_e2_r","nimr_hadgem2_ao","ipsl_cm5a_lr","miroc_esm","miroc_esm_chem","miroc_miroc5","mri_cgcm3")
#ideam_models<-"ipsl_cm5a_mr"
points_ext_prec<-read.csv("//dapadfs/Workspace_cluster_9/USAID_Project/Product_6_resilient_coffee/02-monthly-interpolations/stations-averages/rain_ris_1984.csv",header=T)[,c("LONG","LAT")]  ###puntos a extraer
points_ext_tem<-read.csv("//dapadfs/Workspace_cluster_9/USAID_Project/Product_6_resilient_coffee/02-monthly-interpolations/stations-averages/tmax_ris_1984.csv",header=T)[,c("LONG","LAT")]  
mask<-readOGR("//dapadfs/Workspace_cluster_9/USAID_Project/Product_6_resilient_coffee/02-monthly-interpolations/region/mask/ris_adm0_wgs84.shp",layer="ris_adm0_wgs84") 
region<-raster("//dapadfs/Workspace_cluster_9/USAID_Project/Product_6_resilient_coffee/02-monthly-interpolations/outputs_climatology_1981-2010/average/dtr_1981_2010_2.tif") 
##LOAD FUNCTIONS 
trimedia_tukey <- function(x){
  y1 <- quantile(x, probs = 0.25, na.rm = T)
  y2 <- quantile(x, probs = 0.5, na.rm = T)
  y3 <- quantile(x, probs = 0.75, na.rm = T)
  promedio <- 0.25*y1+0.5*y2+0.25*y3
  return(promedio)
}
##END LOAD FUNCTIONS
for(m in 1:length(ideam_models)){
  for(f in 1:length(rcp)){
    for(v in 1:length(var_list)){
  dir_in_h<-"//dapadfs/data_cluster_2/gcm/cmip5/raw/monthly/historical"
  dir_in_f<-'//dapadfs/data_cluster_2/gcm/cmip5/raw/monthly'
  oDir<-"D:/ToBackup/Unidad Z/Usaid_Fedecafe/datos/downscaling/anomalies"

   rcpv<-rcp[f]
   model=ideam_models[m]
   last_year<-max(as.numeric(list.files(paste0(dir_in_h,"/",model,"/r1i1p1/monthly-files"))))
   oDir<-paste0(oDir,"/",rcpv,"/ideam_models/",model)
   if (!file.exists(oDir)) {dir.create(oDir,recursive=T)} else {cat("Folder exists")}  ##crea carpeta para el rcp

    var=var_list[v]
###########################################################################################################
####################################READ STACKS
    #HISTORICAL
    if(year_to_h>last_year){
    dir_in_h<-paste0(dir_in_h,"/",model,"/r1i1p1/monthly-files/",year_from_h:last_year)
    stacks_year<- lapply( dir_in_h,function(x)rotate(stack(list.files(x,pattern=var,full.names = T))))
    names(stacks_year)<-year_from_h:last_year
   #COMPLETE HISTORICAL 1981-2010
   dir_in_fc<-paste0(dir_in_f,"/",rcpv)
   dir_in_fc<-  paste0(dir_in_fc,"/",model,"/r1i1p1/monthly-files/",(last_year+1):year_to_h)
   stacks_year_f<- lapply( dir_in_fc,function(x)rotate(stack(list.files(x,pattern=var,full.names = T))))
   
stacks_year_f<-lapply(seq_along(stacks_year_f),function(x) if(nlayers(stacks_year_f[[x]])>12){
  stacks_year_f[[x]][[c(1,3,5,7,9,11,13,15,17,19,21,23)]]
   }else{stacks_year_f[[x]]})

   names(stacks_year_f)<-(last_year+1):year_to_h
   stacks_year<-append(stacks_year,stacks_year_f);rm(stacks_year_f)
    }else if(year_to_h<=last_year){
      dir_in_h<-paste0(dir_in_h,"/",model,"/r1i1p1/monthly-files/",year_from_h:year_to_h)
      stacks_year<- lapply( dir_in_h,function(x)rotate(stack(list.files(x,pattern=var,full.names = T))))
      names(stacks_year)<-year_from_h:year_to_h
    }
##future   
future_20<-paste0(dir_in_f,"/",rcpv)
future_20<-  paste0(future_20,"/",model,"/r1i1p1/monthly-files/",2011:2040)
future_20<- lapply( future_20,function(x)rotate(stack(list.files(x,pattern=var,full.names = T))))
future_20<-lapply(seq_along(future_20),function(x) if(nlayers(future_20[[x]])>12){
    future_20[[x]][[c(1,3,5,7,9,11,13,15,17,19,21,23)]]
  }else{ future_20[[x]]})
names(future_20)<-2011:2040

future_30<-paste0(dir_in_f,"/",rcpv)
future_30<-  paste0(future_30,"/",model,"/r1i1p1/monthly-files/",2021:2050)
future_30<- lapply( future_30,function(x)rotate(stack(list.files(x,pattern=var,full.names = T))))
future_30<-paste0(dir_in_f,"/",rcpv)
future_30<-  paste0(future_30,"/",model,"/r1i1p1/monthly-files/",2021:2050)
future_30<- lapply( future_30,function(x)rotate(stack(list.files(x,pattern=var,full.names = T))))
future_30<-lapply(seq_along(future_30),function(x) if(nlayers(future_30[[x]])>12){
  future_30[[x]][[c(1,3,5,7,9,11,13,15,17,19,21,23)]]
}else{ future_30[[x]]})
names(future_30)<-2021:2050

   if(var=="prec"){
    stacks_year<-lapply( stacks_year, function(x)x*3600*24*30)
    future_20<-lapply(future_20, function(x)x*3600*24*30)
    future_30<-lapply(future_30, function(x)x*3600*24*30)
    anomals_20<-lapply(seq_along(stacks_year),function(x)(future_20[[x]]-stacks_year[[x]])/stacks_year[[x]])
    climat_20<-lapply(anomals_20,function(x)data.frame(coordinates(points_ext_prec),extract(x,coordinates(points_ext_prec))))
    names(climat_20)<-names(stacks_year)
    climat_20<-ldply(climat_20)
    colnames(climat_20)<-c("year","long",'lat',month.abb)
    anomals_30<-lapply(seq_along(stacks_year),function(x)(future_30[[x]]-stacks_year[[x]])/stacks_year[[x]])
    climat_30<-lapply(anomals_30,function(x)data.frame(coordinates(points_ext_prec),extract(x,coordinates(points_ext_prec))))
    names(climat_30)<-names(stacks_year)
    climat_30<-ldply(climat_30)
    colnames(climat_30)<-c("year","long",'lat',month.abb)
   } else{
     stacks_year<-lapply(stacks_year, function(x)x-273.15)
     future_20<-lapply(future_20, function(x)x-273.15)
     future_30<-lapply(future_30, function(x)x-273.15)
     anomals_20<-lapply(seq_along(stacks_year),function(x)future_20[[x]]-stacks_year[[x]])
     climat_20<-lapply(anomals_20,function(x)data.frame(coordinates(points_ext_tem),extract(x,coordinates(points_ext_tem))))
     names(climat_20)<-names(stacks_year)
     climat_20<-ldply(climat_20)
     colnames(climat_20)<-c("year","long",'lat',month.abb)
     anomals_30<-lapply(seq_along(stacks_year),function(x)future_30[[x]]-stacks_year[[x]])
     climat_30<-lapply(anomals_30,function(x)data.frame(coordinates(points_ext_tem),extract(x,coordinates(points_ext_tem))))
     names(climat_30)<-names(stacks_year)
     climat_30<-ldply(climat_30)
     colnames(climat_30)<-c("year","long",'lat',month.abb)
   } 

###escribe anomalias por cada punto extraido
climat_20<-climat_20[,-which(colnames(climat_20)%in%"year")]%>%group_by(long,lat)%>%summarise_each(funs(trimedia_tukey(.)))%>%data.frame()
climat_30<-climat_30[,-which(colnames(climat_30)%in%"year")]%>%group_by(long,lat)%>%summarise_each(funs(trimedia_tukey(.)))%>%data.frame()
write.csv(climat_20,paste0(oDir,"/anomalias_",var,"_20.csv"),row.names = F) ##nombre modelo + variable
write.csv(climat_30,paste0(oDir,"/anomalias_",var,"_30.csv"),row.names = F)
#creando rutas futuro
oDir_20<-paste0(oDir,"/futuro_20/")
if (!file.exists(oDir_20)) {dir.create(oDir_20,recursive=T)} else {cat("Folder exists")}  ##crea carpeta para el rcp
oDir_30<-paste0(oDir,"/futuro_30/")
if (!file.exists(oDir_30)) {dir.create(oDir_30,recursive=T)} else {cat("Folder exists")}  ##crea carpeta para el rcp
cat("interpolando anomalias")
####interpolando...
for(i in 1:12){
####futuro 20s
anom_region_20 <-climat_20[,c("long","lat",month.abb[i])]
colnames(anom_region_20) <- c("x", "y", "z")
#Set spatial coordinates to create a Spatial object:
coordinates(anom_region_20) = ~x + y

# Create a data frame from all combinations of the supplied vectors or factors. See the description of the return value for precise details of the way this is done. Set spatial coordinates to create a Spatial object. Assign gridded structure:
# Expand points to grid
grd <- expand.grid(x = seq(from = xmin(region), to = xmax(region), by = 0.004166667), y = seq(from = ymin(region), to = ymax(region), by = 0.004166667))  
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

# Apply idw model for the data
idw <- idw(formula = z ~ 1, locations = anom_region_20, newdata = grd, idp=3)
oIdw <- setExtent(raster(idw), region)
if(var=="prec"){
  oIdw_msk <- oIdw
} else{
  oIdw_msk <- oIdw * 10
  } 
oIdw_msk <- crop(oIdw, extent(mask))
oIdw_msk <- mask(oIdw_msk, mask)
writeRaster(oIdw_msk, paste0(oDir_20,  var, "_", i, ".asc"), overwrite=TRUE)
##futuro 30s
anom_region_30 <-climat_30[,c("long","lat",month.abb[i])]
colnames(anom_region_30) <- c("x", "y", "z")
#Set spatial coordinates to create a Spatial object:
coordinates(anom_region_30) = ~x + y

# Create a data frame from all combinations of the supplied vectors or factors. See the description of the return value for precise details of the way this is done. Set spatial coordinates to create a Spatial object. Assign gridded structure:
# Expand points to grid
grd <- expand.grid(x = seq(from = xmin(region), to = xmax(region), by = 0.004166667), y = seq(from = ymin(region), to = ymax(region), by = 0.004166667))  
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

# Apply idw model for the data
idw <- idw(formula = z ~ 1, locations = anom_region_30, newdata = grd, idp=3)
oIdw <- setExtent(raster(idw), region)
if(var=="prec"){
  oIdw_msk <- oIdw
} else{
  oIdw_msk <- oIdw * 10
} 
oIdw_msk <- crop(oIdw, extent(mask))
oIdw_msk <- mask(oIdw_msk, mask)
writeRaster(oIdw_msk, paste0(oDir_30, "/", var, "_", i, ".asc"), overwrite=TRUE)
       } #close ionterpolation monthly
    } #close var list
  } #close cicle rcp
} #close model

