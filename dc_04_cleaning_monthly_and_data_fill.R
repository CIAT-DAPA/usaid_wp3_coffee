#####################################################
######################PROYECTO USAID
##RESULTADO 6-ACTIVIDAD 1
##ORGANIZAR-LIMPIAR-RELLENAR DATOS CLIMATICOS POR ESTACIÓN
##NOTA: Ejecutarse despues de los filtros de calidad iniciales
##En caso de requerir llenar datos con CHRIPS o RMAUGEN remitirse a codigos previos de modelación
##PROYECTO USAID CAFE RESILIENTE EN RISARALDA
##ELIANA VALLEJO
##2016
##METODO DE LLENADO: Imputación multiple
###LOAD PACKAGES
list.of.packages <- c("dplyr", "plyr","ggplot2","reshape2","Amelia")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(dplyr);library(plyr);library(reshape2);library(ggplot2)
library(Amelia)
###END LOAD PACKAGES
##LOAD FUNCTIONS 
trimedia_tukey <- function(x){
  y1 <- quantile(x, probs = 0.25, na.rm = T)
  y2 <- quantile(x, probs = 0.5, na.rm = T)
  y3 <- quantile(x, probs = 0.75, na.rm = T)
  promedio <- 0.25*y1+0.5*y2+0.25*y3
  return(promedio)
}
##END LOAD FUNCTIONS
##IDENTIFICACIÓN DE OUTLIERS
#Parametros
p_ca_cs<-1.2 ##limite de NA para años consecutivos, tal vez no es buena idea quitar las que tengan un año consecutivo por que quedan muy pocas (1-9-2016)
##se realiza de esta manera para: 1. rescatar mas estaciones 2. se es mas flexible porque en la serie completa estos NA son menos
p_na<-0.3
year_from<-2000
year_to<-2016
years_climat<-2000:2016
e_t=4  ##error máximo tolerable de temperatura
e_p<-5  ##error máximo tolerable de precipitacióm
decim<-1 ##numero de cifras decimales
#############cuadrado de interpolacion
lat_s<-3.5
lat_u<-6.5
long_s<-(-77.5)
long_u<-(-74.5)
yearly<-"YES" #poner YES en caso de que la climatologia se vaya a calcular con medias moviles, para esto necesitaria datos desde
 ###minimo 2 años antes de year_from
###fuentes de datos
#cenicafe
dir_ceni<-"D:/ToBackup/Unidad Z/Usaid_Fedecafe/datos/datacleaning/datacleaning_fedecafe/data_monthly/"
dir_out<-"//dapadfs/Workspace_cluster_9/USAID_Project/Product_6_resilient_coffee/01-weather_stations/proceso 2000-2016 AEPS/climatology_opc"

##infoestaciones
cenicafe<-read.csv("Z:/Usaid_Fedecafe/datos/info_estaciones/catalogo estaciones12.csv",header=T)
colnames(cenicafe)[1]<-"Name"
colnames(cenicafe)[13]<-"Alt"
cenicafe$Codigo<-sprintf("%07d",cenicafe$Codigo)
cenicafe$Source<-rep("Cenicafe",nrow(cenicafe))
##files
ceni<-list.files(dir_ceni,pattern=".txt")
###INFO CENICAFE
cenis<-lapply(ceni,function(i)read.table(paste0(dir_ceni,i),sep=",",header=T)[,2:3])
names(cenis)<-substr(ceni,1,12)
cenis<-ldply(cenis)
cenis$Codigo<-substr(cenis$.id,1,7)
cenis$var<-substr(cenis$.id,9,12)
cenis$Source<-rep("Cenicafe",nrow(cenis))
##ideam
ideam<-read.table("D:/ToBackup/Unidad Z/Usaid_Fedecafe/datos/consolidados_ideam/consolidados_mensuales_ideam.txt",sep=",",header=T)
info_ideam_estaciones<-read.table("D:/ToBackup/Unidad Z/Usaid_Fedecafe/datos/consolidados_ideam/estaciones_consolidadas.txt",sep=",",header=T)
info_ideam_estaciones$Source<-rep("IDEAM",nrow(info_ideam_estaciones))
info_estaciones<-rbind(cenicafe[,c("Codigo" ,"Name","Long","Lat","Alt","Source")],info_ideam_estaciones)
datos<-rbind(cenis[,c(".id","Codigo",'var',"Date","Value","Source")],ideam[,c(".id","Codigo",'var',"Date","Value","Source")])

###eliminando estaciones con datos insuficientes
##id stations to remove
datos$year<-as.numeric(substr(datos$Date,1,4))
est_na_cons<-datos[,c("Codigo","var","year","Value")]%>%
  group_by(Codigo,year,var)%>%
  summarise_each(funs(sum(length(is.na(.)))/12) )%>%data.frame()

est_na_cons<-est_na_cons[est_na_cons$Value<p_ca_cs,]
est_na_cons<-est_na_cons[-which(duplicated(est_na_cons$Codigo)), ]
#numero inicial de estacionesw
n_est<-length(levels(as.factor(datos$Codigo)))
est_f1<-length(levels(as.factor(est_na_cons$Codigo)))

datosf1<-datos
datosf1<-datosf1[datosf1$year>=year_from&datosf1$year<=year_to,]
datosf1<-join(datosf1,info_estaciones,by="Codigo",match="first")
datosf1$alt_gr<-ifelse(datosf1$Alt<1500,1,ifelse(datosf1$Alt>=1500&datosf1$Alt<=4000,2,ifelse(datosf1$Alt>4000,3, NA)))

###########añade la información de las estaciones de ideam
ideamadd<-join(ideam,info_ideam_estaciones,by="Codigo")
ideamadd$alt_gr<-ifelse(ideamadd$Alt<1500,1,ifelse(ideamadd$Alt>=1500&ideamadd$Alt<=4000,2,ifelse(ideamadd$Alt>4000,3, NA)))
########
###outliers detect
outliers_temp<-ideamadd[,c("alt_gr","var","Value")]%>%group_by(alt_gr,var)%>%filter(var%in%c("tmin","tmax"))%>%
  summarise_each(funs(quantile(.,.25,na.rm=T)-(e_t*IQR(.,na.rm=T)),quantile(.,.75,na.rm=T)+(e_t*IQR(.,na.rm=T))))%>%
  data.frame()
colnames(outliers_temp)[3:4]<-c("ls","lu")

###la altitud es mas para la temperatura para la precipitacion los limites con y sin ella son muy similares
outliers_prec<-datosf1[,c("var","Value")]%>%group_by(var)%>%filter(var%in%c("prec"))%>%
  summarise_each(funs(quantile(.,.25,na.rm=T)-(e_p*IQR(.,na.rm=T)),quantile(.,.75,na.rm=T)+(e_p*IQR(.,na.rm=T))))%>%
  data.frame()
colnames(outliers_temp)[3:4]<-c("ls","lu")
####quitando duplicados
datosf1$dupli_id<-duplicated(datosf1)
length(which(datosf1$dupli_id==T))
datosf1<-datosf1[,-ncol(datosf1)]%>%
  group_by(Codigo,.id,Date, var,Source,Name)%>%
  summarise_each(funs(median(.,na.rm=T)))%>%data.frame()
##identificando valores atipicos
datosf1$idoulierp<-  ifelse(datosf1$var=="prec"&datosf1$Value>outliers_prec[1,3],1,0)

datosf1$idouliertmax<-ifelse(datosf1$var=="tmax"&datosf1$alt_gr==1&datosf1$Value>outliers_temp$lu[1]|datosf1$var=="tmax"&datosf1$alt_gr==1&datosf1$Value<outliers_temp$ls[1]|
  datosf1$var=="tmax"&datosf1$alt_gr==2&datosf1$Value>outliers_temp$lu[3]|datosf1$var=="tmax"&datosf1$alt_gr==2&datosf1$Value<outliers_temp$ls[3]|
  datosf1$var=="tmax"&datosf1$alt_gr==3&datosf1$Value>outliers_temp$lu[5]|datosf1$var=="tmax"&datosf1$alt_gr==3&datosf1$Value<outliers_temp$ls[5],1,0)

datosf1$idouliertmin<-ifelse(datosf1$var=="tmin"&datosf1$alt_gr==1&datosf1$Value>outliers_temp$lu[2]|datosf1$var=="tmin"&datosf1$alt_gr==1&datosf1$Value<outliers_temp$ls[2]|
                               datosf1$var=="tmin"&datosf1$alt_gr==2&datosf1$Value>outliers_temp$lu[4]|datosf1$var=="tmin"&datosf1$alt_gr==2&datosf1$Value<outliers_temp$ls[4]|
                               datosf1$var=="tmin"&datosf1$alt_gr==3&datosf1$Value>outliers_temp$lu[6]|datosf1$var=="tmin"&datosf1$alt_gr==3&datosf1$Value<outliers_temp$ls[6],1,0)
##se remueven los datos atipicos
datosf1$Value<-ifelse(datosf1$var=="prec"&datosf1$idoulierp==1,NA,datosf1$Value)
datosf1$Value<-ifelse(datosf1$var=="tmax"&datosf1$idouliertmax==1,NA,datosf1$Value)
datosf1$Value<-ifelse(datosf1$var=="tmin"&datosf1$idouliertmin==1,NA,datosf1$Value)                      

###############################contando NA posterior para saber si el periodo se encuentra completo
estaciones<-levels(as.factor(datosf1$Codigo))
n_m<-((year_to-year_from)+1)*12
datos1<-lapply(estaciones,function(x){
  #  x=estaciones[1]
  st<-as.Date(paste0(year_from,"-01-25"))
  en<- as.Date(paste0(year_to,"-12-25"))
  data.frame(Date=seq(st,en, by = "1 month"),var=rep("prec",n_m),Codigo=rep(x,n_m))}
)
datos1<-ldply(datos1)
datos2<-lapply(estaciones,function(x){
  #  x=estaciones[1]
  st<-as.Date(paste0(year_from,"-01-25"))
  en<- as.Date(paste0(year_to,"-12-25"))
  data.frame(Date=seq(st,en, by = "1 month"),var=rep("tmax",n_m),Codigo=rep(x,n_m))}
)                  
datos2<-ldply(datos2) 
datos3<-lapply(estaciones,function(x){
  #  x=estaciones[1]
  st<-as.Date(paste0(year_from,"-01-25"))
  en<- as.Date(paste0(year_to,"-12-25"))
  data.frame(Date=seq(st,en, by = "1 month"),var=rep("tmin",n_m),Codigo=rep(x,n_m))}
)                  
datos3<-ldply(datos3)    
dates<-rbind(datos1,datos2,datos3)
rm(datos1,datos2,datos3)

datosf1<-merge(dates,datosf1,by=c("Date","var","Codigo"),all.x=T) ##si la estacion tuviese los datos completos para todas las variables
rm(dates)
###calculando los NA
stationstoremove<-datosf1[,c("var","Codigo","Value")]%>%
  group_by(var,Codigo)%>%
  summarise_each(funs(sum(is.na(.))/length(.)))%>%
  data.frame()
stationstoremove<-stationstoremove[stationstoremove$Value>p_na,]
freq_estationtoremove<-data.frame(table(stationstoremove$Codigo))
freq_estationtoremove1<-freq_estationtoremove[which(freq_estationtoremove$Freq>2),]
length(freq_estationtoremove$Var1[freq_estationtoremove$Freq==2])
 
###########################
#####imputando precipitacion
datosf1$Date<-as.Date(datosf1$Date)
precip<-filter(datosf1,var%in%"prec")
precip<-precip[-which(precip$Codigo%in%stationstoremove[which(stationstoremove$var%in%"prec"),"Codigo"]),]
precip$Value_t<-log1p(precip$Value)
precip$month1<-as.numeric(substr(precip$Date,6,7))
###remove precipitacion muy atipica
precip$Value<-ifelse(precip$Value>2000,NA,precip$Value)
imsp<-list()
for(i in 1:12){
  a.out <- amelia(precip[precip$month1==i,c("Date","Codigo","Value_t")], ts = "Date", cs = "Codigo", m =5, boot.type = "ordinary",polytime=1,lags="Value_t" )
  imsp[[i]]<-a.out$imputations
  imsp[[i]]<-ldply(imsp[[i]])
  imsp[[i]]<-imsp[[i]][,-1]%>%group_by(Date,Codigo)%>%summarise_each(funs(mean(.)))%>%data.frame()
  imsp[[i]]$Value_i<-exp(imsp[[i]]$Value_t)-1
   }

imsp<-ldply(imsp )

precip<-merge(precip,imsp,by=c("Date","Codigo"))
####write_precip_data
precip_w<-precip[,c("Codigo","Source","Date","Value_i")]
precip_w$Codigo<-as.character(precip_w$Codigo)
precip_w$Codigo<-ifelse(nchar(precip_w$Codigo)<=7,paste0(0,precip_w$Codigo),precip_w$Codigo)
precip_w$Source<-ifelse(precip_w$Source=="Cenicafe","Cenic",precip_w$Source)
colnames(precip_w)<-c("Codigo","Source","Date","Value")
precip_w$Source<-ifelse(is.na(precip_w$Source)==T,"IDEAM",precip_w$Source)
precip_w$cs<-paste0(precip_w$Codigo,"_prec_",precip_w$Source)
precip_w[,1:2]<-NULL
precip_w<-split(precip_w,precip_w$cs)
##ESCRIBIENDO DATOS
#lapply(seq_along(precip_w),function(x)write.table(precip_w[[x]][,c("Date","Value") ], paste0("D:/ToBackup/Unidad Z/Usaid_Fedecafe/datos/datacleaning/data_montly_withoutNA_ambas_fuentes/",names(precip_w)[x],".csv"),row.names=F,sep=","))
##########################################################################
#######TEMPERATURA
temp<-filter(datosf1,var%in%c("tmin","tmax"))
temp<-temp[-which(temp$Codigo%in%stationstoremove[which(stationstoremove$var%in%c("tmin","tmax")),"Codigo"]),]
temp$Value<-ifelse(temp$var=="tmax"&temp$idouliertmax==1,NA,temp$Value)
temp$Value<-ifelse(temp$var=="tmin"&temp$idouliertmin==1,NA,temp$Value) 
##identificando las estaciones que tienen unicamente temperatura minima
freq_vartemp<-temp[,c("var","Codigo","Value")]%>%
  group_by(var,Codigo)%>%
  summarise_each(funs(length(.)))%>%
  data.frame()
freq_vartemp<-data.frame(table(freq_vartemp$Codigo))
temp<-temp[temp$var%in%c("tmin","tmax"),]
#temp<-temp[-which(temp$Codigo%in%26095180),]
temp<-dcast(temp[,c("Date","Codigo","var","Value")],Date+Codigo~var,value.var = "Value")
imsp<-list()
for(i in 1:12){
  a.out <- amelia(temp, ts = "Date", cs = "Codigo", m =5, boot.type = "ordinary",polytime=1,lags=c("tmin","tmax") )
  imsp[[i]]<-a.out$imputations
  imsp[[i]]<-ldply(imsp[[i]])
  imsp[[i]]<-imsp[[i]][,-1]%>%group_by(Date,Codigo)%>%summarise_each(funs(mean(.)))%>%data.frame()}

imsp<-ldply(imsp)
colnames(imsp)[3:4]<-c("tmax_i","tmin_i")
temp<-merge(temp,imsp,by=c("Date","Codigo"))
rm(imsp)
temp1<-melt(temp[,c("Date","Codigo","tmax_i","tmin_i")],id.vars=c("Date","Codigo"),value.name = "Value_i")
colnames(temp1)[3]<-"var"
levels(temp1$var)<-c("tmax","tmin")
#########write temp
temp_w<-temp1
temp_w$Codigo<-as.character(temp_w$Codigo)
temp_w$Source<-ifelse(nchar(temp_w$Codigo)<=7,"Ceni","IDEAM")
temp_w$Codigo<-ifelse(nchar(temp_w$Codigo)<=7,paste0(0,temp_w$Codigo),temp_w$Codigo)
colnames(temp_w)<-c("Date","Codigo","var","Value","Source")
temp_w$cs<-paste0(temp_w$Codigo,"_",temp_w$var,"_",temp_w$Source)
temp_w<-split(temp_w,temp_w$cs)
#lapply(seq_along(temp_w),function(x)write.table(temp_w[[x]][,c("Date","Value") ], paste0("D:/ToBackup/Unidad Z/Usaid_Fedecafe/datos/datacleaning/data_montly_withoutNA_ambas_fuentes/",names(temp_w)[x],".csv"),row.names=F,sep=","))
##########################################################################
rm(datos,datosf1,est_na_cons,freq_vartemp,freq_estationtoremove1,temp_w,precip_w)
colnames(temp1)[4]<-"Value_i"
datos_without_na<-rbind(precip[,c("Date","Codigo","var","Value_i")],temp1);rm(temp1)
datos_without_na$var<-as.factor(datos_without_na$var)
#############ORGANIZANDO CLIMATOLOGIAS
datos_without_na$month<-month.abb[as.numeric(substr(datos_without_na$Date,6,7))]
datos_without_na$year<-as.numeric(substr(datos_without_na$Date,1,4))
levels(datos_without_na$var)[1]<-"rain"
if(yearly=="NO") {
climatology<-datos_without_na[,c("Codigo","var","month","Value_i")]%>%
    group_by(Codigo,var,month)%>%
    summarise_each(funs(trimedia_tukey(.)))
} 
###climatology yearly
if(yearly=="YES"){
climatology<-list()
for(i in 1:length(years_climat)){
  climatology[[i]]<-datos_without_na[datos_without_na$year<=(years_climat[i])&datos_without_na$year>=(years_climat[i]-2),
                          c("Codigo","var","month","Value_i")]%>%
    group_by(Codigo,var,month)%>%
    summarise_each(funs(trimedia_tukey(.)))%>%data.frame(.)
  }
names(climatology)<-years_climat
climatology<-ldply(climatology)
climatology$Value_i<-round(climatology$Value_i,decim)
climatology<-dcast(climatology,Codigo+var+.id~month)
climatology$var<-paste0(climatology$var,"_ris_",climatology$.id) 
}else{
climatology$Value_i<-round(climatology$Value_i,decim)
climatology<-dcast(climatology,Codigo+var+~month)
climatology$var<-paste0(climatology$var,"_ris_2011",climatology$.id)  ##se pone el numero 2011 para ejecutar la interpolacion con los yearly

}
climatology<-join(climatology,info_estaciones,by=c("Codigo"),match="first")
climatology$old_id<-climatology$Codigo
climatology$country<-rep("Colombia",nrow(climatology))
climatology$nyears<-rep((year_to-year_from)+1,nrow(climatology))
climatology$.id<-NULL
climatology<-climatology[,c("var","Codigo","Source","old_id","Name","country","Long","Lat","Alt",month.abb,"nyears")]
colnames(climatology)[2]<-"ID"
colnames(climatology)<-toupper(colnames(climatology))
climatology<-split(climatology,climatology$VAR)
###write climatology
lapply(seq_along(climatology),function(x) write.table(climatology[[x]][,-1],
                                   paste0(dir_out,"/",names(climatology)[x],".csv"),row.names = F,sep=","))

