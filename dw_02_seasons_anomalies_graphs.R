##########################################################################################
## Purpose: PLOTS FOR REPORT: anomalies downscaling from CIAT data
## Author: Eliana Vallejo
##dpto Risaralda
##########################################################################################
##LOAD PACKAGE
library(gstat);require(rgdal);library(sp);require(maptools);library(rasterVis)
require(rgdal)
library(dplyr);library(plyr);library(reshape2)
# Set libraries
##END LOAD PACKAGE

#####parameters
var_list<-c("prec","tmin","tmax")
rcp<-c("rcp26",'rcp45','rcp60','rcp85')
futuro<-c(20,30)
paste0(dir_in,"/",fut,"/")
seasons <- list("djf"=c(12,1,2), "mam"=3:5, "jja"=6:8, "son"=9:11, "ann"=1:12)
# List of seasons
#seasons <- list("djf"=c(12,1,2), "mam"=3:5, "jja"=6:8, "son"=9:11, "ann"=1:12)

for(r in 1:length(rcp)){
for(v in 1:length(var_list)){
  for(fut in 1:length(futuro)){

fut<-paste0("futuro_",futuro[fut])

dir_in<-"D:/ToBackup/Unidad Z/Usaid_Fedecafe/datos/downscaling/anomalies/ensemble"
dir_out<-"D:/ToBackup/Unidad Z/Usaid_Fedecafe/datos/downscaling/anomalies/performance"

month<-stack(paste0(dir_in,"/",fut,"/average/",rcp[r],"/",var_list[v],"_",1:12,".asc"))
month_cv<-stack(paste0(dir_in,"/",fut,"/cv/",rcp[r],"/cd_",var_list[v],"_",1:12,".asc"))
for (i in 1:length(seasons)){
 avg<-calc(month[[c(seasons[i], recursive=T)]],fun=function(x){mean(x,na.rm=T)})
  var_cv<-calc(month_cv[[c(seasons[i], recursive=T)]],fun=function(x){mean(x,na.rm=T)})
###WRITE
  writeRaster(avg,paste0(dir_out,"/",rcp[r],"_",var_list[v],"_",names(seasons)[i],".asc"),overwrite=TRUE)
  writeRaster(var_cv,paste0(dir_out,"/","cv_",rcp[r],"_",var_list[v],"_",names(seasons)[i],".asc"),overwrite=TRUE)
     }
    }
   }
}

###PLOTS ANOMALIES FOR REPORT
id <- c("djf", "mam", "jja", "son")
dir_in<-"D:/ToBackup/Unidad Z/Usaid_Fedecafe/datos/downscaling/anomalies/performance"
#s=1
#v=1
var_list="tmax"
mask<-readOGR("D:/ToBackup/Unidad Z/Usaid_Fedecafe/datos/shps_altitud_y otros/risaralda_admin/ris_adm0.shp",layer="ris_adm0")
geotopo <- readOGR("D:/ToBackup/Unidad Z/Usaid_Fedecafe/datos/shps_altitud_y otros/ecotopo_prj.shp", layer= "ecotopo_prj")
for(s in 1:length(id)){  ##id seassons
  for(v in 1:length(var_list)){
 
quarter<-stack(list.files(dir_in,pattern=glob2rx(paste0("*",var_list[v],"*",id[s],"*")),full.names=T))   
if(var_list[v]=="prec"){
  quarter<-quarter*100
}
    names(quarter)<-c("RCP26","RCP45","RCP60","RCP85")
    
   p1<- setZ(quarter, names(quarter))
     myTheme <- BuRdTheme() # Define squeme of colors
   if(var_list[v]=="prec"){
   myTheme$regions$col=colorRampPalette(c("#f1eef6","#d0d1e6","#a6bddb", "#74a9cf", "#2b8cbe","#045a8d"))(length(zvalues)-1) # Set new colors
   zvalues <- seq(0, 20,5) # Define limits
   }
   if(var_list[v]=="tmax"){
     myTheme$regions$col=colorRampPalette(c("#ffffd4","#fee391", "#fec44f", "#fe9929","#d95f0e","#993404"))(length(zvalues)-1) # Set new colors
     zvalues <- seq(0, 1.5,0.1) # Define limits
   }
   if(var_list[v]=="tmin"){
     myTheme$regions$col=colorRampPalette(c("#feedde","#fdd0a2", "#fdae6b", "#fd8d3c","#e6550d","#a63603"))(length(zvalues)-1) # Set new colors
     zvalues <- seq(0, 1.5,0.1) # Define limits
     }
   myTheme$strip.border$col = "white" # Eliminate frame from maps
   myTheme$axis.line$col = 'white' # Eliminate frame from maps
   # myTheme=rasterTheme(region=brewer.pal('Blues', n=9))  
   tiff(paste0(dir_in,"/",var_list[v],"_",id[s],"_compar.tif"), width=1000, height=1200, pointsize=8, compression='lzw',res=100)
   
   print(levelplot(p1, at = zvalues, scales = list(draw=FALSE),  xlab="", ylab="", 
                   par.settings = myTheme, colorkey = list(space = "bottom")) + layer(sp.polygons(mask)) + layer(sp.polygons(geotopo)))
   
   dev.off()
    
  }
  
}