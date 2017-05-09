##########################################################################################
## Purpose: Species distribution modelling of coffee robusta: description 
## Author: Eliana Vallejo
## dpto Risaralda
##########################################################################################
list.of.packages <- c("raster", "biomod2","dismo","gridExtra","reshape2","mgcv","VSURF","maptools","rgdal","sp","dplyr","plyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
##load packages
require(raster);require(rasterVis);require(maptools);require(rgdal);library(sp)
library(biomod2)
library(dismo)
library(gridExtra);library(reshape2)
#########variable selection
library(mgcv)
library(VSURF)
library(dplyr);library(plyr)
#######################################################################
#############################Params####################################
#######################################################################
#modeling
evot<-T ##si es T se incluye la evapotranspiracion de referencia
add_suelos<-"yes"  #para anadir suelos
##la cual no varia a futuro por que se calcula a partir de la altitud
cor_multi<-.7  ##correlacion a partir de la cual puede haber multicolinealidad
sampl<-35##porcentaje de datos usado para ajuste de modelo 
p_train<-70 #porcentaje de datos con los que se entrena el modelo
eval_model<- c('TSS',"ROC","KAPPA",'ACCURACY') ##medidas de evaluación de los modelos
sp_name<-"coffee" #nmobre variable respuesta 
mask1 <-readOGR("//dapadfs/Workspace_cluster_9/USAID_Project/Product_6_resilient_coffee/02-monthly-interpolations/region/mask/ris_adm0_wgs84.shp",layer="ris_adm0_wgs84")
########LOAD PATHS
bdir<-"//dapadfs/Workspace_cluster_9/USAID_Project/Product_6_resilient_coffee/04-suitability/" 
odir<-"//dapadfs/Workspace_cluster_9/USAID_Project/Product_6_resilient_coffee/04-suitability/results"
dir_alt<-"//dapadfs/Workspace_cluster_9/USAID_Project/Product_6_resilient_coffee/02-monthly-interpolations/region/500m_v4/alt-prj-ris.asc" ##path altitude
#######################################################################################################################
####################################END PARAMS
##########################################################LOAD DATA
##condition 
alt<-raster(dir_alt) ##altitud
#clima
evo_t<-raster(paste0(bdir,"indices_agroclimaticos/evot_ref.tif")) #evapotranspiración
clima_current<-stack(lapply(list.files(paste0(bdir,'indices_agroclimaticos/current'),full.names = T,pattern=".asc"),function(x)stack(x)))

clima_current<-crop(clima_current,extent(-76.35875, -75.40875, 4.67125, 5.475417)) 
clima_current<-stack(crop(alt,clima_current),clima_current)
names(clima_current)[1]<-"alt"
if(evot==T){
  clima_current<-stack(resample(evo_t, clima_current, method="ngb"),clima_current)
}
clima_current<-stack(mask(clima_current,mask1))
crs(clima_current) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

#suelos
suelos<-readOGR(paste0(bdir,"indices_agroclimaticos/suelos/shp/Risaralda_suelos_MCB1_var.shp"),layer="Risaralda_suelos_MCB1_var")
##points
presencia1<-read.csv(paste0(bdir,"presencia/presencia_ausencia/presencia/presencia.csv"),header=T)
##end load data
################################################################################################################################################################
###################################SELECCION DE VARIABLES
p<-presencia1[sample(1:nrow(presencia1),round((sampl/100)*nrow(presencia1),0)),]
p<-p[,1:2]
###
presvals <- extract(clima_current, p)
set.seed(0)
###random pseudo-absences
backgr <- randomPoints(clima_current, round((nrow(p))*.25))
absvals <- extract(clima_current, backgr)
##
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(rbind(p,backgr),cbind(pb, rbind(presvals, absvals)))
train<-sdmdata[sample(1:nrow(sdmdata),round(.2*nrow(sdmdata),0)),]
train<-melt(train,id.vars=c("x","y","pb"))
rm(absvals,presvals,presencia1)
####################ASOCIACION VARIABLES CLIMATICAS Y PRESENCIA
if (!file.exists(odir)) {dir.create(odir,recursive=T)} else {cat("Folder exists")}
odir_g<-paste0(odir,"/descripción_grafica_inicial/")
if (!file.exists(odir_g)) {dir.create(odir_g,recursive=T)} else {cat("Folder exists")}
clima_vars<-list(precip=14:17,variability_prec=3:7,tmean=33:37,tmin=38:42,tmax=28:32,thermal_time=24:27, thermal_amplitude= 8:12,altitud=2)
lapply(seq_along(clima_vars),function(x)
  ggplot(train[train$variable%in%levels(train$variable)[clima_vars[[x]]],],aes(x=as.numeric(value),y=as.numeric(pb)))+
    geom_point(color="white")+facet_wrap(~variable)+
    stat_smooth(method = "auto",col="darkblue", fullrange = TRUE)+lims(y=c(0,1))+
    labs(x = '', y = 'presence')+ geom_hline(yintercept = 0)+  geom_hline(yintercept = 1)+
    theme(    axis.text.y = element_text(size=10,hjust=.5,vjust=0,face="plain"),
              strip.text.x = element_text(size = 12),
              axis.text.x= element_text(size=10,hjust=0.5,vjust=0.5,face="plain"),
              axis.title.y = element_text(size = rel(1.5), angle = 90),
              axis.title.x = element_text(size = rel(1.5), angle = 360),
              legend.text=element_text(size=10),
              strip.text = element_text(size=10))+
    ggsave(file=paste0(odir_g,names(clima_vars)[x],".pdf"), width=15,height=10));rm(clima_vars)

##############################################################################################
###################JOIN SUELOS
#############################################suelos
######
p1<-p
coordinates(p1)<-~x+y
crs(p1)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
backg1<-data.frame(backgr)
coordinates(backg1)<- ~x+y
crs(backg1)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
suelos<-spTransform(suelos, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ph<- rasterize(suelos, clima_current[[1]], 'pH_d_',fun=mean,mask=F,na.rm=T)
names(ph)<-"ph"
mo<-rasterize(suelos, clima_current[[1]], 'Mtrrg')
names(mo)<-"om"
#textura
tx<-rasterize(suelos, clima_current[[1]], 'Tx___',fun="first")
names(tx)<-"tx"
levels(suelos[["Tx___"]])
suelos_data<-data.frame(pb,mo=c(extract(mo,p),extract(mo, backgr)),ph=c(extract(ph,p),extract(ph, backgr)),tx=c(extract(tx,p),extract(tx, backgr)))
#################################ANALISIS DE TEXTURAS
pre<-data.frame(table(suelos_data[pb%in%1,"tx"]))
pre$pb<-rep("precense",nrow(pre));pre$Freq<-(pre$Freq/sum(pre$Freq))*100 #precensias 
pse_ause<-data.frame(table(suelos_data[pb%in%0,"tx"]));pse_ause$pb<-rep("no-precense",nrow(pse_ause));pse_ause$Freq<-(pse_ause$Freq/sum(pse_ause$Freq))*100 #ausencias
pre<-rbind(pre,pse_ause);rm(pse_ause)
levels(pre[,1])<-levels(suelos[["Tx___"]])[-6]
ggplot(pre,aes(x=Var1,y=Freq,fill=pb))+geom_bar(stat = "identity", position="dodge")+
  labs(x = '', y = '%',fill="")+
  scale_fill_manual(values = c("#fec44f","#2ca25f"))+
  theme(    axis.text.y = element_text(size=16,hjust=.5,vjust=0,face="plain"),
            axis.text.x= element_text(size=16,hjust=0.5,vjust=0.5,face="plain"),
            axis.title.y = element_text(size = rel(1.5), angle = 90),
            axis.title.x = element_text(size = rel(1.5), angle = 360),
            legend.text=element_text(size=16),
            strip.text = element_text(size=16))+
  coord_flip()+  ggsave(file=paste0(odir_g, "texturas_vs_coffee_presence.pdf"), width=15,height=10);rm(pre)
##########join soils and weather 
sdmdata_s<-cbind(sdmdata,suelos_data[,-1])
sdmdata_s$tx<-as.factor(sdmdata_s$tx)
levels(sdmdata_s$tx)<-levels(suelos[["Tx___"]])
#Graficas de suelos
tiff(paste0(odir_g,"texturas.tif"), width=1200, height=400, pointsize=8, compression='lzw',res=100);spplot(suelos,"Tx___", col.regions= colorRampPalette(c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02","#a6761d","#8c2d04"))(20))
dev.off()
tiff(paste0(odir_g,"materia_organica.tif"), width=1200, height=400, pointsize=8, compression='lzw',res=100);spplot(suelos,"Mtrrg", col.regions= colorRampPalette(c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404"))(20))
dev.off()
tiff(paste0(odir_g,"ph.tif"), width=1200, height=400, pointsize=8, compression='lzw',res=100);spplot(suelos,"pH_d_", col.regions= colorRampPalette(c("#d7191c","#fdae61","#ffffbf","#a6d96a","#1a9641"))(20))
dev.off()
###materia organica y ph vs presencia de cafe
var_s<-c("ph","mo")
for(i in 1:2){
  p<-ggplot(suelos_data[,c("pb",var_s[i])],aes(x=as.numeric(eval(parse(text=var_s[i]))),y=as.numeric(pb)))+ geom_point(color="white")
  
  if(var_s[i]=="mo"){p=p+stat_smooth(method = "loess",  col="blue")}else{p=p+stat_smooth(method = "auto",  col="blue")}  
  p+lims(y=c(0,1))+
    labs(x = '', y = 'presence')+
    geom_hline(yintercept = 0)+
    geom_hline(yintercept = 1)+
    theme(    axis.text.y = element_text(size=16,hjust=.5,vjust=0,face="plain"),
              axis.text.x= element_text(size=16,hjust=0.5,vjust=0.5,face="plain"),
              axis.title.y = element_text(size = rel(1.5), angle = 90),
              axis.title.x = element_text(size = rel(1.5), angle = 360),
              legend.text=element_text(size=16),
              strip.text = element_text(size=16))+
    ggsave(file=paste0(odir_g,var_s[i],".pdf"), width=15,height=10),rm(var_s)
}
##########################################################################################################################
#################################################SELECCION DE VARIABLES###################################################
##########################################################################################################################
###############seleccion de variables para gam and glm
train<-sdmdata_s[sample(1:nrow(sdmdata_s),round(.2*nrow(sdmdata_s),0)),-ncol(sdmdata_s)]
train<-melt(train,id.vars=c("x","y","pb"))
train<-split(train,train$variable)
gam_ind<-lapply(train,function(x) gam(pb ~ s(as.numeric(value),k=3,bs = "cs"),family = binomial(link = 'logit'),dat=x,  method = 'GCV.Cp'))
aics<-lapply(gam_ind,function(x)AIC(x));rsq<-lapply(gam_ind,function(x)summary(x)$r.sq)
r2_gam<-ldply(rsq);names(r2_gam)<-c("var","value");r2_gam$model<-rep("GAM",nrow(r2_gam))
aic_gam<-ldply(aics);names(aic_gam)<-c("var","value");aic_gam$model<-rep("GAM",nrow(aic_gam))
##seleccion de variables para GLM
glm_ind<-lapply(train,function(x) glm(pb ~ value,family=binomial,dat=x))
aics<-lapply(glm_ind,function(x)summary(x)$aic)
aic_glm<-ldply(aics)
s_r2<-lapply(glm_ind,function(x)1-(summary(x)$deviance/summary(x)$null.deviance)) ##se calcula un pseudo-R2 de asociación para cada modelo individual
s_r2_glm<-ldply(s_r2)
names(s_r2_glm)<-c("var","value")
s_r2_glm$model<-rep("GLM",nrow(s_r2_glm))
aic_glm$model<-rep("GLM",nrow(aic_glm))
#######################################################################
####seleccion de variables para maxent y GBM
##en este caso se ajusta un modelo maxent a priori y se evalua la importancia de las variables
p1<-p[sample(1:nrow(p),round(nrow(p)*0.05,0)),];coordinates(p1)<-~x+y
myBiomodData<- BIOMOD_FormatingData(resp.var = p1, expl.var = stack(clima_current,mo,ph),resp.name = sp_name,PA.nb.rep =1,PA.nb.absences =length(p1),PA.strategy = 'sre')
myBiomodOption<- BIOMOD_ModelingOptions(MAXENT.Phillips= list(path_to_maxent.jar = "D:/evallejo/maxent.jar"))
myBiomodModelOut <- BIOMOD_Modeling(myBiomodData, models =  c("MAXENT.Tsuruoka","GBM"),models.options = myBiomodOption,NbRunEval=1,DataSplit=p_train, VarImport=3,Prevalence=0.5,
                                    models.eval.meth = eval_model,SaveObj = TRUE, modeling.id="test",do.full.models=T,rescal.all.models = FALSE)
var_import<-data.frame(getModelsVarImport(myBiomodModelOut));var_import$var<-rownames(var_import);var_import<-melt(var_import,id.vars="var");var_import$model<-as.factor(substr(var_import$variable,1,3))
var_import$variable<-NULL
#############################random forest
sdmdata_rf<-na.omit(sdmdata_s[sample(1:nrow(sdmdata_s),round(nrow(sdmdata_s)*0.15,0)),-1:-2])
sdmdata_rf<-sdmdata_rf[,-ncol(sdmdata_rf)]
vsurfparallel <- VSURF(sdmdata_rf[,-1], sdmdata_rf[,1], parallel = TRUE, ncores = 4,ntree =500)
summary(vsurfparallel )
names(vsurfparallel )
imp_v_rf<-data.frame(var=names(sdmdata_rf[,-1])[vsurfparallel$varselect.thres],value=vsurfparallel$imp.varselect.thres)
imp_v_rf$model<-rep("RF",nrow(imp_v_rf))
vsurfparallel$varselect.thres
##mejores variables predictoras para random forest
names(sdmdata_rf[,-1])[vsurfparallel$varselect.pred]
#######
var_import<-rbind(var_import, r2_gam,s_r2_glm,imp_v_rf)
var_import<-var_import%>%group_by(var,model)%>%summarise_each(funs(mean(.)))%>%data.frame(.)
levels(var_import$model)[2]<-"MAXENT"
var_import$type<-as.factor(substr(var_import$var, 1, 4))
levels(var_import$type)<-c("alt","prec","temp","evot",rep("soil",3),"prec",rep("temp",5))
odir_vi<-paste0(odir,"/importancia de variables/")
if (!file.exists(odir_vi)) {dir.create(odir_vi,recursive=T)} else {cat("Folder exists")}
write.csv(var_import,paste0(odir_vi,"var_import_selection_vars_all_model.csv"),row.names=F)
