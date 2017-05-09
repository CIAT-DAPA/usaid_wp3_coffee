##########################################################################################
## Purpose: Species distribution modelling of coffee robusta: Performance
## Author: Eliana Vallejo
##dpto Risaralda
##########################################################################################
##########################################################################################
list.of.packages <- c("raster", "rasterVis","maptools","rgdal","reshape2","dplyr","plyr","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
##LOAD PACKAGES
library(raster);library(rasterVis);require(maptools);require(rgdal)
library(reshape2);library(plyr);library(dplyr);library(ggplot2)
########################################params
dir_in<-"//dapadfs/Workspace_cluster_9/USAID_Project/Product_6_resilient_coffee/04-suitability/"
dir_current<-paste0(dir_in,"results/current/ensemble")
dir_f<-paste0(dir_in,"results/ensemble_modeling")
dir_results<-paste0(dir_in,"results/performance_suitability/")
if (!file.exists(dir_results)) {dir.create(dir_results,recursive=T)} else {cat("Folder exists")}
mask1 <-readOGR("//dapadfs/Workspace_cluster_9/USAID_Project/Product_6_resilient_coffee/02-monthly-interpolations/region/mask/ris_adm0_wgs84.shp",layer="ris_adm0_wgs84")
###################functions
mgsub <- function(pattern, replacement, x, ...) {
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  result
}
###end functions
###puntos de presencia
presencia1<-read.csv(paste0(dir_in,"presencia/presencia_ausencia/presencia/presencia.csv"),header=T)
p<-sample(1:nrow(presencia1),round((90/100)*nrow(presencia1),0))
p<-presencia1[p,]
p<-p[,1:2]
###LOAD DATA
current<-stack(list.files(dir_current,full.names=T,pattern=".tif"))
current<-current/10
current<-mask(crop(current,mask1),mask1)
names(current)<-c("Promedio",substr(names(current)[-1],12,19))
###load future
future<-stack(list.files(dir_f,pattern="suit_mean", full.names = T))
future<-mask(crop(future,mask1),mask1)
future_20<-mgsub(c("futuro_30","GLM","GAM",'RF',"GBM","MAXENT.Tsuruoka","MAXENT.Phillips"),rep(NA,7),names(future))
future_20<-future[[na.omit(future_20)]]
names(future_20)<-c("rcp26","rcp45","rcp60","rcp85")
future_30<-mgsub(c("futuro_20","GLM","GAM",'RF',"GBM","MAXENT.Tsuruoka"),rep(NA,6),names(future))
future_30<-future[[na.omit(future_30)]]
names(future_30)<-names(future_20)
###change 2020s
change_20<-future_20-current[["Promedio"]]
names(change_20)<-names(future_20)
###change 2030s
change_30<-future_30-current[["Promedio"]]
names(change_30)<-names(future_30)
region<-"//dapadfs/Workspace_cluster_9/USAID_Project/Product_6_resilient_coffee/02-monthly-interpolations/region/500m_v4"

######################################################################
######plots
###distribution 

latitud<-raster(paste0(region,"/latitude"))
latitud<-mask(crop(latitud,current[[1]]),current[[1]])
altitud<-raster(paste0(region,"/altitude"))
altitud<-mask(crop(altitud,current[[1]]),current[[1]])
alt_lat<-stack(altitud,latitud)
alt_lat<- projectRaster(alt_lat, future[[1]]) 
current1<- projectRaster(current[[1]], future[[1]]) 
region_idon<-stack(alt_lat,current1,future)

pointsd<-raster::extract(region_idon,coordinates(region_idon)[sample(1:nrow(coordinates(region_idon)),1000),],sp=T)
pointsd=data.frame(na.exclude(pointsd))
pointsd<-melt(pointsd,id.vars=c("latitude","altitude"))
pointsd$future<-as.factor(substr(pointsd$variable, 11, 19))
levels(pointsd$future)[3]<-levels(pointsd$future)[1]
pointsd$rcp<-as.factor(gsub("_","",substr(pointsd$variable, 21, 26)))
levels(pointsd$rcp)[1]<-"current"
current_30<-filter(pointsd,rcp%in%"current")
current_30$future<-rep(levels(pointsd$future)[2],nrow(current_30))
pointsd<-rbind(pointsd,current_30)
levels(pointsd$future)<-c(2020,2030)
lapply(c("altitude","latitude"),function(x){
  pointsd%>%
    group_by_(.dots = c('variable', 'future', 'rcp', x))%>%
    summarise_each(funs(mean(.)))%>%
    data.frame()%>%
    ggplot(aes(x=eval(parse(text=x)),y=value))+ 
    geom_point(color="white")+
    facet_wrap(~future)+
    stat_smooth(method = "gam", formula=y ~ s(x, bs = "cs"),aes(linetype=rcp,colour=rcp),se =F)+
    scale_linetype_manual(name="RCP",values = c(current = "solid", rcp26 = "longdash",rcp45="dashed",rcp60="dotdash",rcp85="twodash")) +
    theme_bw()+
    theme(    axis.text.y = element_text(size=10,hjust=.5,vjust=0,face="plain"),
              axis.text.x= element_text(size=13,hjust=0.5,vjust=0.5,face="plain"),
              axis.title.y = element_text(size = rel(1.5), angle = 90),
              axis.title.x = element_text(size = rel(1.5), angle = 360),
              legend.text=element_text(size=10),
              strip.text = element_text(size=10))+
    scale_colour_manual(name="RCP",
                        values=c(current="black",rcp26="#018571",rcp45="#80cdc1",rcp60="#dfc27d",rcp85="#a6611a"))+
    ylim(c(0,100))+labs(x=x,y="presence")+ggsave(paste0(dir_results,"distribucion_",x,".pdf"), width=15,height=10)})
rm(pointsd,alt_lat,current1,region_idon,latitud,altitud)
##############################################################################3
######evaluacion de modelos

dirs_eval<- "//dapadfs/Workspace_cluster_9/USAID_Project/Product_6_resilient_coffee/04-suitability/results/evaluacion de modelos"
eval<-ldply(lapply(list.files(dirs_eval,pattern=".csv",full.names=T),function(i)read.csv(i,header=T)))
eva_acu<-eval[eval$metrics%in%"Testing.data"&eval$eval%in%c("TSS","ROC"),]
eva_acu$model<-as.factor(substr(eva_acu$model,1,8))
eva_acu$model<-with(eva_acu, reorder(model, model, function(x) -length(value)))
ggplot(eva_acu,aes(x = model, y = as.numeric(value)),fill=model)+geom_boxplot()+ theme_bw()+labs(x="",y="")+ylim(c(0.7,1))+facet_wrap(~eval)+
  ggsave(paste0(dir_results,"evaluacon_modelos.pdf"), width=15,height=10)
rm(dirs_eval,eval,eva_acu)
###contribucion de variables
varc<-read.csv("//dapadfs/Workspace_cluster_9/USAID_Project/Product_6_resilient_coffee/04-suitability/results/importancia de variables/var_import_selection_vars_all_model.csv",header=T)
vars_exclude<-c('alt')
varc<-varc[-which(varc$var%in%vars_exclude),]
varc$var<-gsub("TA","TT",varc$var)
varc$var<-gsub("dtr","T.Amplitude",varc$var)
lapply(levels(varc$model),function(x)
  varc[varc$model%in%x,]%>% ggplot( aes(x=reorder(var, value), y=value,fill=type)) +  geom_bar( stat="identity") +
    theme_bw()+  scale_fill_manual(values = c("#e78ac3", "#1f78b4","#d95f02" ,"#33a02c"),guide="none")+
    coord_flip()+ labs(x="",y="")+ ggsave(paste0(dir_results,"mejores_predictoras_modelo_",x,".pdf"), width=15,height=10))
rm(varc,vars_exclude)
###observed
coordinates(p)<-~x+y
pdf(paste0(dir_results,"observed.pdf"), width=15,height=10)
plot(p,col="#238b45",pch=16)
plot(mask1,add=T)
dev.off()
#############################levels plot
graphs<-list(current,future_20,future_30,change_20,change_30)
names(graphs)<-c("suitability_current","suitability_future_20","suitability_future_30","change_suitability_fut20","change_suitability_fut30" )
unloadNamespace(c("biomod2","ggplot2"))
for(x in seq_along(graphs)){
  if(names(graphs)[x]=="suitability_current"){p1<- setZ(graphs[[x]] ,names(current))}else{ p1<- setZ(graphs[[x]] ,names(future_20))}
  myTheme <- BuRdTheme() # Define squeme of colors
 
  if(names(graphs)[x]=="change_suitability_fut20" |names(graphs)[x]=="change_suitability_fut30"){
    zvalues <- seq(-75, 75,1) # Define limits
    #zvalues <- seq(0, 100,10) # Define limits
    myTheme$regions$col=colorRampPalette(c("#cc4c02","#fe9929","#fed98e","white","#9ebcda","#edf8e9","#bae4b3","#74c476","#238b45"))(length(zvalues)-1) # Set colors for changes
  }else{
   zvalues <- seq(0, 100,1) # Define limits 
  myTheme$regions$col=colorRampPalette(c("#cc4c02","#fe9929","#fed98e","#edf8e9","#c7e9c0","#a1d99b","#74c476","#31a354","#006d2c"))(length(zvalues)-1)
  }
  myTheme$strip.border$col = "white" # Eliminate frame from maps
  myTheme$axis.line$col = 'white' # Eliminate frame from maps
  if(names(graphs)[x]=="suitability_current"){
  tiff(paste0(dir_results,names(graphs)[x],".tif"), width=1000, height=800, pointsize=8, compression='lzw',res=100)}else{
  tiff(paste0(dir_results,names(graphs)[x],".tif") , width=1200, height=400, pointsize=8, compression='lzw',res=100)
  }
  print(levelplot(p1, at = zvalues, margin = FALSE, scales = list(draw=FALSE),  xlab="", ylab="", par.settings = myTheme, colorkey = list(space = "bottom")) + 
          layer(sp.polygons(mask1)) )
  dev.off()
}
