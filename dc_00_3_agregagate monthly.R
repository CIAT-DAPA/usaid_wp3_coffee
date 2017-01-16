############################fraccionando Cenicafe por variables y estacion
##PROYECTO USAID CAFE RESILIENTE EN RISARALDA
##ELIANA VALLEJO
##2016
##OBJETIVO: fraccionar datos de cenicafe en archivos txt por estación y variable
########### y agregar los datos a nivel mensual

##load packages
library(dplyr);library(plyr);library(reshape2)
##end load packages
##load libraries
dir_in<- "D:/ToBackup/Unidad Z/Usaid_Fedecafe/datos/ideam_solicitud_noviembre/data_cleaning/step1/"
datos_in<-read.csv("Z:/Usaid_Fedecafe/datos/cafetera_clima/Historicos.csv",header=T )
files <- list.files(dir_in, pattern=c(glob2rx("*tmin*txt*")))
TypeOrig<-length(grep("ReadyPaRelleno",files))#Aquí identifico si el archivo origen contiene info diaria desde la Fuente, o fue convertido desde info horaria a info diaria (DiarioReady)
#si TypeOrig es diferente de cero es porque hubo un proceso Horario previo

#Asignación de nombres de archivos según origen
if(TypeOrig>0){
  nom.files<-substring(files,1,13)
}else{nom.files<-substring(files,1,nchar(files)-4)}
Data.all.files <- lapply(paste(dir_in,"/",files,sep="",dec="."),function(x){read.table(x,header=T,sep="\t")})
names(Data.all.files)=nom.files

datos_df<-ldply(Data.all.files)[,1:3]

datos_df$month_year<-substr(datos_df$Date,1,8)
datos_df1<-datos_df[,c(".id","month_year","Value")]%>%
                      group_by(.id,month_year)%>%
                      summarise_each(funs(sum(.,na.rm=T),mean(.,na.rm=T)))
  
datos_df1$Var<-substr(datos_df1$.id,9,13)
datos_df1$Value<-ifelse(datos_df1$Var=="prec",datos_df1$sum,datos_df1$mean)
datos_df1$Date<-paste0(datos_df1$month_year,25)
datos_df1<-datos_df1[,c(".id","Date","Value")]
##writing data
datos_list<-split(datos_df1[,c("Date","Value")],datos_df1$.id)
lapply(seq_along(datos_list),function(i)write.table(
  datos_list[i],paste0("D:/ToBackup/Unidad Z/Usaid_Fedecafe/datos/ideam_solicitud_noviembre/data_cleaning/step2_monthly/",
                       names(datos_list)[i],"_monthly.txt"),sep=",",row.names = F))
       