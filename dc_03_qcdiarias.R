## Componente 2
## Victor Hugo Patiño Bravo
## QC para información a nivel diario, hallan o no pasado por proceso de conversión a escala diaria.
a=gc()
rm(list=ls())
####Cargar paquetes
library(chron)
library(stringr)

###Lectura de los archivos
#ruta<-"Z:/Usaid_Fedecafe/datos/cafetera_clima"
ruta="Z:/Usaid_Fedecafe/datos/datacleaning/step2_ideam"
Destino="Z:/Usaid_Fedecafe/datos/datacleaning/step3_ideam/"
#Destino="Z:/Usaid_Fedecafe/datos/datacleaning/datacleaning_fedecafe/"
files <- list.files(ruta, pattern=c("\\.txt$"))

TypeOrig=length(grep("DiarioReady",files))#Aquí identifico si el archivo origen contiene info diaria desde la Fuente, o fue convertido desde info horaria a info diaria (DiarioReady)
#si TypeOrig es diferente de cero es porque hubo un proceso Horario previo

#Asignación de nombres de archivos según origen
if(TypeOrig>0){
nom.files<-substring(files,1,nchar(files)-16)
}else{nom.files<-substring(files,1,nchar(files)-4)}

Data.all.files <- lapply(paste(ruta,"/",files,sep="",dec="."),function(x){read.table(x,header=T,sep="\t")})
names(Data.all.files)=nom.files
#row_lt2 <- which(sapply(Data.all.files,function(x)colSums(is.na(x)))==sapply(Data.all.files, nrow))

Data.all.filesNAFree=Data.all.files
Serie.diaria=Data.all.files

# Tabla de valores de referencia
REF=read.csv("Z:/Usaid_Fedecafe/datos/valoes de referencia/Val_REF_QCHour.csv", header=T, row.names=1)
colnames(REF)[3]<-"PREC"

for(i in 1:length(Data.all.files)) {

 ProbDUP=FALSE
  colnames(Data.all.files[[i]])=c("Date", "Value")
  #####filtro 
  count.na<-sum(is.na(Data.all.files[[i]]$Value))/length(Data.all.files[[i]]$Value)

  
# Quitar NA para trabajar solo con valores
Data.all.filesNAFree[[i]]=Data.all.files[[i]][which(!is.na(Data.all.files[[i]]$Value)),]
# Lectura de fechas
  if(TypeOrig>0){
DateOK=as.Date(as.character(Data.all.filesNAFree[[i]]$Date), "%Y-%m-%d")  #Formato de fecha CON separadores
}else{DateOK=as.Date(as.character(Data.all.filesNAFree[[i]]$Date), "%Y-%m-%d")}   #Formato de fecha SIN separadores
  
Data.all.filesNAFree[[i]]$Date=DateOK

# Deteccion de duplicados con valores distintos
DUP=duplicated(Data.all.filesNAFree[[i]]$Date)
if(any(DUP)){
  POS=which(DUP==TRUE)
  setDUP=Data.all.filesNAFree[[i]][POS,]
  DUPserious=duplicated(setDUP)
  if(any(!DUPserious)){
    ProbDUP=TRUE
    print("Hay un problema de duplicados con valores distintos")
    DUPcsv=Data.all.filesNAFree[[i]][which(DUPserious==TRUE),]
    write.csv(DUPcsv, paste0(Destino,"/",nom.files[i], "_DUPlicados.csv"))
  }
}

# Detectar variable
VAR=substring(nom.files[i],nchar(nom.files[i])-3, nchar(nom.files[i]))
  VAR=substring(nom.files[i],nchar(nom.files[i])-3, nchar(nom.files[i]))  
VAR=toupper(VAR)
VAR<-ifelse(VAR=="MEAN","TMIN",VAR)  ##para que ejecute el proceso de calidad de tmin en tmean
VAR<-ifelse(VAR=="IGHT","S_BRIGHT",VAR)
# Detecar valores fuera de rango
a=paste0("subset(Data.all.filesNAFree[[i]],Data.all.filesNAFree[[i]]$Value>=REF$",VAR,"[2]&Data.all.filesNAFree[[i]]$Value<REF$",VAR,"[1])")
Bonnes1=eval(parse(text=a))

# Detectar valores fuera de 4 dev
Dev=sd(Bonnes1$Value,na.rm=T)
PROMEDIO=mean(Bonnes1$Value,na.rm=T)
Bonnes1$CUATRODEV=NA
Bonnes1$CUATRODEV[(Bonnes1$Value>(PROMEDIO+4*Dev)|Bonnes1$Value<(PROMEDIO-4*Dev))]=c("Fuera de 4DEV")

# Deteccion rango de fechas
Date.min=min(Bonnes1$Date)
Date.max=max(Bonnes1$Date)

# Construir serie resultante
SEQ=as.data.frame(seq(Date.min,Date.max,1))
colnames(SEQ)=c("Date")

POSpot=match(SEQ$Date, Bonnes1$Date)
POSlimpio=POSpot[!is.na(POSpot)]
Bonnes3=Bonnes1[POSlimpio,]
POS=match(Bonnes3$Date,SEQ$Date)
SEQ$Value=NA
SEQ$CUATRODEV=NA
SEQ$Value[POS]=Bonnes3$Value
SEQ$CUATRODEV[POS]=Bonnes3$CUATRODEV
Serie.diaria[[i]]=SEQ
  
# Generar reporte
size=dim(Data.all.files[[i]])[1]
Good=dim(Bonnes3)[1]
Duplicados=dim(Bonnes1)[1]-Good
Error=dim(Data.all.filesNAFree[[i]])[1]-(Good+Duplicados)
NAentrada=size-dim(Data.all.filesNAFree[[i]])[1]
POSdecal=POS[-1]
POSlimpio.small=POS[1:length(POSdecal)]
MAYORsalto=max(POSdecal-POSlimpio.small)

Fuera4DEV=sum(!is.na(Bonnes3$CUATRODEV))

resul=c(size, Duplicados, Duplicados/size, Error, Error/size, Fuera4DEV, NAentrada, NAentrada/size, ProbDUP, MAYORsalto)
Tabla.fin=as.data.frame(resul,row.names=c("Numero de datos totales", "Numero de Duplicados", "% de duplicados", "Errores", "% errores", "Numero de valores fuera de 4 DEV","NA en entrada", "% de NA en entrada", "Problema de duplicados con valores distintos", "Mayor hueco"))
write.csv(Tabla.fin,paste0(Destino,nom.files[i], "_ResumenQCdiario.csv"))
write.table(Serie.diaria[[i]],paste0(Destino,nom.files[i], "_ReadyPaRelleno.txt"), sep="\t", row.names=FALSE)

jpeg(paste0(Destino,nom.files[i], "_Plotserie.jpg"), width=600, height=400, units="px")
plot(Serie.diaria[[i]]$Value~Serie.diaria[[i]]$Date, ylab="Value", xlab="Date", type="l")
dev.off()
  
jpeg(paste0(Destino,nom.files[i], "_Boxplotserie.jpg"), width=600, height=400, units="px")
boxplot(Serie.diaria[[i]]$Value)
dev.off()

  }
#try(clean(Data.all.files,REF))
#####################################################################################
#####################################################################################
#####################################################################################
## QC para información a nivel diario, hallan o no pasado por proceso de conversión a escala diaria.
## Estaciones con información desagregada o un archivo por variable
## TMAX>TMIN, DIARIA
rutOrigen=Destino
destino=paste0(rutOrigen)

files <- list.files(rutOrigen, pattern=".txt")

nom.files<-substring(files,1,nchar(files)-4)#
fil=files[grep("ReadyPaRelleno",nom.files)]#
files=fil[grep("tmax|tmin",fil)]#
nom.files<-substring(files,1,nchar(files)-19)
Datos <- lapply(paste(rutOrigen,"/",files,sep=""),function(x){read.table(x,header=T,sep="\t")})
names(Datos)=nom.files
head(Datos[[1]])

for(i in 1:length(Datos)){
  #si esta condicion se cumple quiere decir que la fecha original viene con separadores
  if(class(Datos[[i]][,1])=="factor"){
    Datos[[i]][,1]=as.Date(Datos[[i]][,1])#,format="%d/%m/%Y")
  }
}

#Cuales de las estaciones contienen info sobre Tmax y Tmin
TempMax=Datos[grep("tmax",nom.files)]
TempMin=Datos[grep("tmin",nom.files)]
#ID's de las estaciones de cada variable
IDtmax=substring(names(TempMax),1,nchar(names(TempMax))-5)
IDtmin=substring(names(TempMin),1,nchar(names(TempMin))-5)
#cbind(IDtmin)
vec=(IDtmax==IDtmin)
vec1=length(vec[vec==FALSE])

#Para hacer la comparacion correctamente de todas las estaciones debe haber igual cantidad
#de estaciones, y que sean efectivamente las mismas estaciones entre Tmax y Tmin
if(length(TempMax)==length(TempMin) &  vec1==0){
  print("Puede realizarce la comparacion")
}else{print("Error, una o más estaciones no coinciden")}

cant.error=0
percent=0
for(i in 1:max(length(TempMax),length(TempMin))){
  #Posicion que ocupa la estacion de Tmax entre las estaciones de Tmin  
  pos=grep(IDtmax[i],names(TempMin))
  #Fecha mínima y máxima entre las dos bases de datos
  
  Datemin=min(min(TempMax[[i]][,1]),min(TempMin[[pos]][,1]))
  Datemax=max(max(TempMax[[i]][,1]),max(TempMin[[pos]][,1]))
  Dates=seq(Datemin,Datemax,1)
  
  newTmax=0
  for(j in 1:length(Dates)) {
    datasub=TempMax[[i]][TempMax[[i]][,1]==Dates[j],]
    if(dim(datasub)[1]!=0){    
      newTmax[j]=TempMax[[i]][TempMax[[i]][,1]==Dates[j],2]
    }else{
      newTmax[j]=NA
    }
  }
  
  newTmin=0
  for(j in 1:length(Dates)) {
    data_sub=TempMin[[pos]][TempMin[[pos]][,1]==Dates[j],]
    if(dim(data_sub)[1]!=0){    
      newTmin[j]=TempMin[[i]][TempMin[[pos]][,1]==Dates[j], 2]
    }else{
      newTmin[j]=NA
    }
  }
  
  Station.Unit=data.frame(Dates,newTmax,newTmin)
  write.csv(Station.Unit,paste0(destino,IDtmax[i],"_unificado.csv"))
  
  Station.Unit.Error=which(Station.Unit$newTmax<=Station.Unit$newTmin)
  originalTMAX=paste0(IDtmax[i],"_TMAX")
  originalTMIN=paste0(IDtmax[i],"_TMIN")
  #Reemplazando los valores de las inconsistencias en las fechas de las estaciones involucradas
  if(length(Station.Unit.Error)>0){
    
    Datos[[originalTMAX]][Station.Unit.Error,2]=NA
    Datos[[originalTMIN]][Station.Unit.Error,2]=NA
    ##Reescribiendo con las correcciones
    write.table(Datos[[originalTMAX]],paste0(destino,originalTMAX,"_02.txt"),sep="\t",row.names=F)
    write.table(Datos[[originalTMIN]],paste0(destino,originalTMIN,"_02.txt"),sep="\t",row.names=F)
  }
  cant.error[i]=length(Station.Unit.Error)
  percent[i]=round((cant.error[i]/dim(Station.Unit)[1])*100,3)
}

cant.err=paste(percent,"%")
cant.errorf=data.frame(IDtmax,cant.error,cant.err)
write.csv(cant.errorf,paste0(destino,"Coherencia,Tmax_Tmin.csv"),row.names=F)
