#######################################################################
#######################recopilacion de datos ideam#####################
##PROYECTO USAID CAFE RESILIENTE EN RISARALDA
##ELIANA VALLEJO
##2016
##OBJETIVO: Consolidar los datos de las diferentes fuentes por las que se obtuvieron
############ datos de IDEAM
library(dplyr);library(plyr);library(reshape2)
###sources
dir_ide_soli<-"D:/ToBackup/Unidad Z/Usaid_Fedecafe/datos/ideam_solicitud/monthly_raw/"  ##primera solicitud de datos ideam
ideam_add<-'D:/ToBackup/Unidad Z/Usaid_Fedecafe/datos/datacleaning/step4_ideam_monthly/'  ##datos horarios de ideam existentes en el cluster provenientes de los datos horarios
ideam_add_m<-"D:/ToBackup/Unidad Z/Usaid_Fedecafe/datos/datacleaning/ideam monthly exis/monthly/" ##datos mensuales de ideam provenientes de los datos diarios del cluster
ideam_prec_dir<-"D:/ToBackup/Unidad Z/Usaid_Fedecafe/datos/ideam_solicitud_noviembre/data_cleaning/step2_monthly/" ##datos de precipitacion 01 11 2016
##files
ideam_s<-list.files(dir_ide_soli,pattern=".txt") ##primera solicitud ideam
ideam_in<-list.files(ideam_add,pattern=".txt") ##datos horarios
ideam_in_m<-list.files(ideam_add_m,pattern=".txt") ##datos mensuales
ideam_prec_dat<-list.files(ideam_prec_dir,pattern=".txt") ##datos de octubre 31
ideam_temp_dat<-c(list.files(ideam_prec_dir,pattern=glob2rx("*tmin*txt*")),list.files(ideam_prec_dir,pattern=glob2rx("*tmax*txt*")))
###PARAMETERS
lat_s<-3.5
lat_u<-6.5
long_s<-(-77.5)
long_u<-(-74.5)
###estaciones de Caldas, Tolima, Valle, Quindio, Risaralda primera solicitud
ideam_prec<-read.table("D:/ToBackup/Unidad Z/Usaid_Fedecafe/datos/info_estaciones/solicitud de informacion ideam 31_10_2016.csv",header=T,sep=",",fill=T) 
ideam_prec<-ideam_prec[,c("COD_CATALOGO_ES","NOMBRE_ES","LONGITUD",'LATITUD',"ALTITUD")]
colnames(ideam_prec)<-c("Codigo","Name","Long","Lat","Alt")
ideam_soli<-read.table("D:/ToBackup/Unidad Z/Usaid_Fedecafe/datos/info_estaciones/estaciones_ideam_solicitud.txt",header=T,sep=",")
colnames(ideam_soli)<-c("Codigo","Name","dpto","Lat","Long","Alt")
ideam_soli$Source<-rep("IDEAM",nrow(ideam_soli))
ideam<-read.csv("D:/ToBackup/Unidad Z/Usaid_Fedecafe/datos/info_estaciones/location.csv",header=T) ##catalogo de las estaciones existentes de ideam
colnames(ideam)[1:4]<-c("Codigo","Long","Lat","Alt")
ideam$Source<-rep("IDEAM",nrow(ideam))
info_estaciones<-rbind(ideam_prec[,c("Codigo","Name","Long","Lat","Alt")],ideam_soli[,c("Codigo","Name","Long","Lat","Alt")],ideam[,c("Codigo","Name","Long","Lat","Alt")])
info_estaciones<-subset(info_estaciones,info_estaciones$Lat>=lat_s&info_estaciones$Lat<=lat_u&info_estaciones$Long>=long_s&info_estaciones$Long<=long_u)
info_estaciones$dupli<-duplicated(info_estaciones)
info_estaciones<-info_estaciones[info_estaciones$dupli==FALSE,]
info_estaciones$dupli<-NULL
info_estaciones$dupli<-duplicated(info_estaciones$Codigo)
info_estaciones<-info_estaciones[info_estaciones$dupli==FALSE,]
info_estaciones$dupli<-NULL

############READ DATA
##primera solicitud
ideam_s1<-lapply(ideam_s,function(i)read.table(paste0(dir_ide_soli,i),sep=",",header=T))
names(ideam_s1)<-substr(ideam_s,1,13)
ideam_s1<-ldply(ideam_s1)
colnames(ideam_s1)[3]<-"Value"
ideam_s1$Codigo<-substr(ideam_s1$.id,1,8)
ideam_s1$var<-substr(ideam_s1$.id,10,13)
ideam_s1$Source<-rep("IDEAM",nrow(ideam_s1))
#horarios
ideamadd<-lapply(ideam_in,function(i)read.table(paste0(ideam_add,i),sep=",",header=T))
ideamadd<-lapply(ideamadd, setNames,c("Date","Value"))
names(ideamadd)<-substr(ideam_in,1,12)
ideamadd<-ldply(ideamadd)
ideamadd$Codigo<-substr(ideamadd$.id,1,8)
ideamadd$var<-substr(ideamadd$.id,9,12)
ideamadd$Source<-rep("IDEAM",nrow(ideamadd))
##mensuales
ideam_month<-lapply(ideam_in_m,function(i)read.table(paste0(ideam_add_m,i),sep=",",header=T))
ideam_month<-lapply(ideam_month, setNames,c("Date","Value"))
names(ideam_month)<-substr(ideam_in_m,1,17)
ideam_month<-ldply(ideam_month)
ideam_month$dev<-NULL
ideam_month$Codigo<-substr(ideam_month$.id,1,8)
ideam_month$var<-substr(ideam_month$.id,14,17)
ideam_month$Source<-rep("IDEAM",nrow(ideam_month))
##ideam solicitud 31 de octubre
ideam_prec1<-lapply(ideam_prec_dat,function(i)read.table(paste0(ideam_prec_dir,i),sep=",",header=T))
ideam_prec1<-lapply(ideam_prec1, setNames,c("Date","Value"))
names(ideam_prec1)<-substr(ideam_prec_dat,1,13)
ideam_prec1<-ldply(ideam_prec1)
ideam_prec1$Codigo<-substr(ideam_prec1$.id,1,8)
ideam_prec1$var<-substr(ideam_prec1$.id,10,13)
ideam_prec1$Source<-rep("IDEAM",nrow(ideam_prec1))

###ideam solicitud 31 de octubre temperatura

ideam_temp<-lapply(ideam_temp_dat,function(i)read.table(paste0(ideam_prec_dir,i),sep=",",header=T))
ideam_temp<-lapply(ideam_temp, setNames,c("Date","Value"))
names(ideam_temp)<-substr(ideam_temp_dat,1,13)
ideam_temp<-ldply(ideam_temp)
ideam_temp$Codigo<-substr(ideam_temp$.id,1,8)
ideam_temp$var<-substr(ideam_temp$.id,10,13)
ideam_temp$Source<-rep("IDEAM",nrow(ideam_temp))

IDEAM_EXIS<-rbind(ideamadd,ideam_month,ideam_s1,ideam_prec1,ideam_temp)
IDEAM_EXIS$dupli_id<-duplicated(IDEAM_EXIS)
dup1=length(which(IDEAM_EXIS$dupli_id==T))
if(dup1==0){
  ideam_exist=IDEAM_EXIS}
if(dup1>0){
  ###los datos duplicados se promedian conservando el valor, esto se da por las fuentes tanto
  #horarias como mensuales que se tuvieron en cuenta
  ideam_exist<-IDEAM_EXIS[,-ncol(IDEAM_EXIS)]%>%
    group_by(Codigo,.id,Date, var,Source)%>%
    summarise_each(funs(median(.,na.rm=T)))%>%data.frame() 
}

ideam_exist<-ideam_exist[ideam_exist$Codigo%in%info_estaciones$Codigo,]
##wirte data
write.table(ideam_exist,"D:/ToBackup/Unidad Z/Usaid_Fedecafe/datos/consolidados_ideam/consolidados_mensuales_ideam.txt",sep=",",row.names=F)
write.table(info_estaciones,"D:/ToBackup/Unidad Z/Usaid_Fedecafe/datos/consolidados_ideam/estaciones_consolidadas.txt",sep=",",row.names=F)
