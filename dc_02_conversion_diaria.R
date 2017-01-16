###NOTA; aplica cuando las variables se encuentran a nivel horario
## Componente 2
## Victor Hugo Patiño Bravo
## Conversion a escala diaria de información horaria, basado en diferentes reglas para garantizar la validez del dato resultante
a=gc()
rm(list=ls())

library(chron)
library(stringr)

###Lectura de los archivos

ruta="Z:/Usaid_Fedecafe/datos/datacleaning/step1_ideam"
Destino="Z:/Usaid_Fedecafe/datos/datacleaning/step2_ideam/"
files <- list.files(ruta, pattern="\\.txt$")
nom.files<-substring(files,1,nchar(files)-13)
Data.all.files <- lapply(paste(ruta,"/",files,sep="",dec="."),function(x){read.table(x,header=T,sep="\t")})
names(Data.all.files)=nom.files

Data.all.filesNAFree=Data.all.files
Data.all.files.OK=Data.all.files
Serie.diaria=Data.all.files
#Data.all.files=Serie.diaria
n=length(Data.all.files)
i=NULL
for(i in 5:n){

#Data.all.files[[i]]$Value<-ifelse(grepl("^[[:digit:]]+L",Data.all.files[[i]]$Value),NA,as.numeric(Data.all.files[[i]]$Value))
#Conversion de horas
colnames(Data.all.files[[i]])<-c("Date","Hour","Value")
TEMPhour=Data.all.files[[i]]$Hour
Data.all.files[[i]]$Hour=gsub("a.m.", "AM", Data.all.files[[i]]$Hour)
Data.all.files[[i]]$Hour=gsub("p.m.", "PM", Data.all.files[[i]]$Hour)
Data.all.files[[i]]$Hour=gsub("a,m,", "AM", Data.all.files[[i]]$Hour)
Data.all.files[[i]]$Hour=gsub("p,m,", "PM", Data.all.files[[i]]$Hour)
Data.all.files[[i]]$Hour=gsub("a", "AM", Data.all.files[[i]]$Hour)
Data.all.files[[i]]$Hour=gsub("p", "PM", Data.all.files[[i]]$Hour)

Cond=((strptime(Data.all.files[[i]]$Hour,"%I:%M:%S %p"))[1])#Condicion para identificar si la hora tiene segundos
#Si Cons=NA no tiene segundos
if(is.na(Cond)){TEMPhour<- strftime(strptime(Data.all.files[[i]]$Hour,"%I:%M %p"),"%H:%M:%S")##SI formato original hora es 12:35 am/pm
}else{TEMPhour<- strftime(strptime(Data.all.files[[i]]$Hour,"%I:%M:%S %p"),"%H:%M:%S")}##SI formato original hora es 12:35:59 am/pm

Data.all.files[[i]]$Hour=times(TEMPhour)
 modif=0
  # Crear series de trabajo
  
Data.all.filesNAFree[[i]]=Data.all.files[[i]][which(!is.na(Data.all.files[[i]]$Value)),]
##convert hour

Data.all.files.OK[[i]]=Data.all.filesNAFree[[i]]

##########
Data.all.files.OK[[i]]$Date=as.Date(as.character(Data.all.filesNAFree[[i]]$Date),"%Y%m%d")
Data.all.files.OK[[i]]$Hour=times(Data.all.filesNAFree[[i]]$Hour) #se comenta 27 oct 16 pq no funciona


  # Lectura de la variable
VAR=gsub("[^[:alpha:]]+","",nom.files[i])
VAR=toupper(VAR)
#str_replace_all(nom.files, "[^[:alnum:]]", " ") esta chevere extrae todos los caracteres no alpha-numericos
  # Calculo de las medianas de intervalo de tiempo
Data.all.files.OK[[i]]["DIFF"]=NA
HOURDATE=strptime(paste(Data.all.files.OK[[i]]$Date, Data.all.files.OK[[i]]$Hour), "%Y-%m-%d %H:%M") 
Decal=c(HOURDATE[1],HOURDATE[-length(HOURDATE)])
Data.all.files.OK[[i]]$DIFF=as.numeric(difftime(HOURDATE,Decal,units="mins"))
medianas=aggregate(Data.all.files.OK[[i]]$DIFF~Data.all.files.OK[[i]]$Date, Data.all.files.OK[[i]],function(i)median(i,na.rm=F))
colnames(medianas)=c("Date", "Median")

if(VAR=="TMAX"){
  # Definir fechas usables  
  Hmin=times(strftime(strptime("5:58 AM","%I:%M %p"),"%H:%M:00"))
  Hmax=times(strftime(strptime("6:02 PM","%I:%M %p"),"%H:%M:00"))
  minutos.dia=720
  Data.in.time=Data.all.files.OK[[i]][Data.all.files.OK[[i]]$Hour>Hmin & Data.all.files.OK[[i]]$Hour<Hmax,]
  registros=aggregate(Data.in.time$Value~Data.in.time$Date, Data.in.time,length, simplify=TRUE)
  colnames(registros)=c("Date", "RegNumber")
  Fechas.rescat=merge(registros, medianas, by.x="Date", by.y="Date")
  Fechas.rescatOK=Fechas.rescat[which(Fechas.rescat$RegNumber>(0.8*minutos.dia/Fechas.rescat$Median)),]
  
  # Calcular dato diario
  FechasOK.POS=which(!is.na(match(Data.in.time$Date, Fechas.rescatOK[[1]])))
  SerieOP=Data.in.time[FechasOK.POS,]
  Serie.diaria[[i]]=aggregate(SerieOP$Value~SerieOP$Date, SerieOP[,c("Date","Value")], function(i)max(as.numeric(as.character(i))))

  # Indicadores del trabajo efectuado
  registros.totales=dim(Data.all.files[[i]])[1]
  NAentrada=dim(Data.all.files[[i]])[1]-dim(Data.all.filesNAFree[[i]])[1]
  registros.totales.dias=length(unique(Data.all.files[[i]]$Date))
  registros.rescatados=dim(Serie.diaria[[i]])[1]
  
}else if(VAR=="TMIN"){
  # Definir fechas usables  
  Hmin=strptime("6:02 AM","%I:%M %p")
  Hmax=strptime("5:58 PM","%I:%M %p")
  minutos.dia=720
  Data.in.time=Data.all.files.OK[[i]][Data.all.files.OK[[i]]$Hour<Hmin | Data.all.files.OK[[i]]$Hour>Hmax,]
  registros=aggregate(Data.in.time$Value~Data.in.time$Date, Data.in.time,length, simplify=TRUE)
  colnames(registros)=c("Date", "RegNumber")
  Fechas.rescat=merge(registros, medianas, by.x="Date", by.y="Date")
  Fechas.rescatOK=Fechas.rescat[which(Fechas.rescat$RegNumber>(0.8*minutos.dia/Fechas.rescat$Median)),]
  
  # Calcular dato diario
  FechasOK.POS=which(!is.na(match(Data.in.time$Date, Fechas.rescatOK[[1]])))
  SerieOP=Data.in.time[FechasOK.POS,]
  Serie.diaria[[i]]=aggregate(SerieOP$Value~SerieOP$Date,SerieOP,function(i)min(as.numeric(as.character(i))))
  
  # Indicadores del trabajo efectuado
  registros.totales=dim(Data.all.files[[i]])[1]
  NAentrada=dim(Data.all.files[[i]])[1]-dim(Data.all.filesNAFree[[i]])[1]
  registros.totales.dias=length(unique(Data.all.files[[i]]$Date))
  registros.rescatados=dim(Serie.diaria[[i]])[1]
  
}else if(VAR=="PREC"){
  # Definir fechas usables  
  minutos.dia=1440
  Data.in.time=Data.all.files.OK[[i]]
  registros=aggregate(Data.in.time$Value~Data.in.time$Date, Data.in.time,length, simplify=TRUE)
  colnames(registros)=c("Date", "RegNumber")
  Fechas.rescat=merge(registros, medianas, by.x="Date", by.y="Date")
  Fechas.rescatOK=Fechas.rescat[which(Fechas.rescat$RegNumber>(0.8*minutos.dia/Fechas.rescat$Median)),]
  
  # Calcular dato diario
  FechasOK.POS=which(!is.na(match(Data.in.time$Date, Fechas.rescatOK[[1]])))
  SerieOP=Data.in.time[FechasOK.POS,]
  
  #fun=function(serie){
   # serie=na.omit(serie)
    #pos=length(serie)
    #val=serie[pos]
    #return(val)
#  }
 
  Serie.diaria[[i]]=aggregate(SerieOP$Value~SerieOP$Date, SerieOP, function(i)sum(as.numeric(as.character(i))))
  
  # Indicadores del trabajo efectuado
  registros.totales=dim(Data.all.files[[i]])[1]
  NAentrada=dim(Data.all.files[[i]])[1]-dim(Data.all.filesNAFree[[i]])[1]
  registros.totales.dias=length(unique(Data.all.files[[i]]$Date))
  registros.rescatados=dim(Serie.diaria[[i]])[1]
  
}  

colnames(Serie.diaria[[i]])=c("Dates","Value")
  # Generacion del reporte
  resul=c(registros.totales, NAentrada, NAentrada/registros.totales*100, registros.totales.dias, registros.rescatados, registros.rescatados/registros.totales.dias*100)
  Tabla.fin=as.data.frame(resul,row.names=c("Numero de datos totales", "Numero de NA en entrada", "% NA en entrada", "Dias con datos entrada", "Dias rescatables", "% dias rescatables"))
  write.csv(Tabla.fin,paste0(Destino,nom.files[i], "_ResumenTransfDia.csv"))
  if(modif==1){
    varCCM2=paste0(substring(nom.files[i],first=1,nchar(nom.files[i])-9),"CCM2_ESOL")
    write.table(Serie.diaria[[i]],paste0(Destino,varCCM2, "_DiarioReady.txt"), sep="\t",row.names=F)
  }else {write.table(Serie.diaria[[i]],paste0(Destino,nom.files[i], "_DiarioReady.txt"), sep="\t",row.names=F)}

 }
