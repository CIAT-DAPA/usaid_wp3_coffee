######NOTA; Aplica cuando la información se encuentra a nivel horario
# Componente 2
# Victor Hugo Patiño Bravo & Sylvain Delerce
# Control de calidad basico para datos capturados a nivel horario
# Estructura de los archivos de entrada :
# Date-Hour-Value
a=gc()
rm(list=ls())

library(chron)
library(stringr)
library(tcltk)

# inicializacion de indicadores reporte
ProbCohernciaTEMP=FALSE
ProbUNIDAD=FALSE
ProbHEADER=FALSE
ProbDUP=FALSE
REPORTSINDATA=NA

# Lectura de los datos
Filesroutesdestino="D:/ToBackup/Unidad Z/Usaid_Fedecafe/datos/datacleaning/"

###PRECIPITACION
##params

#depto<-c("Risaralda","Valle","Tolima","Choco","Caldas","Antioquia","Quindio")
##end params
###read data
path_in<-"D:/ToBackup/Unidad Z/Usaid_Fedecafe/datos/ideam0/"
##end read data
carpeta<-"hourly-raw"
###read location stations ideam
location<-read.csv("D:/ToBackup/Unidad Z/Usaid_Fedecafe/datos/info_estaciones/location.csv",header = T,sep=",")
location$Station<-as.numeric(location$Station)
location$Departament<-as.factor(location$Departament)
levels(location$Departament)[31:32]<-rep("Valle",2)
#location<-subset(location,location$Departament%in%depto)
location=location[location$Lon>=-77.5 & location$Lon<=-75.5,]
location=location[location$Lat >=-3.5 & location$Lat<=6.5,]



var<-list.files(paste0(path_in,carpeta),pattern="per-station")
var<-gsub("-per-station","",var)
files_stations<-lapply(var,function(x)
  list.files(paste0(path_in,carpeta,"/",var,"-per-station"),pattern="txt",full.names = T)
)
files_stations<-unlist(files_stations)
files_stations<-grep(paste(location$Station,collapse="|"),files_stations,value=T)
FilesNames<-gsub(paste0(path_in,carpeta,"/"),"",files_stations)
NumFiles=length(FilesNames)
nom.files=substring(FilesNames,31,nchar(FilesNames)-4) 
stations= gsub("[^0-9]", "", FilesNames, "")
Data.all.files=lapply(files_stations,function(x){read.table(x,sep="\t",header=T,row.names=NULL)})
names(Data.all.files)=nom.files
datos_df<-ldply(Data.all.files)
# Deteccion de las variables
VAReasy=grep("tmax|tmin|_tmean|rhum|w_wsmean", nom.files)
VARaccu=grep("prec|MJM2|CCM2", nom.files)


#verificacion que se clasificaron todos los archivos
if (length(VAReasy)+length(VARaccu)==NumFiles){
  Data.easy=Data.all.files[VAReasy]
  Data.accu=Data.all.files[VARaccu]
}else{
  print("Problema con la clasificacion de los archivos !!")
  stop
  }

# Verificacion de los header:
for(i in 1:length(Data.all.files)){

  if (all(colnames(Data.all.files[[i]])==c("Date","Hour","Value"))){    
    }else{
    ProbHEADER=TRUE
    mens=paste(("Hay un problema con los header del archivo"),i)
    print(mens)
  }
}

##############################################################################
# Crear series de destino
Data.all.files.OK=Data.all.files

#lapply(Data.all.files.OK, function(x){apply(x[3],2,summary)})
perc=0

for(i in 1:length(Data.all.files)){

  Data.all.files.OK[[i]][,3]=as.numeric(as.character(Data.all.files.OK[[i]][,3]))  
  ProbDUP=FALSE
  
  dataFech=Data.all.files.OK[[i]]$Date
  
  typeOrig=length(grep("-|/",dataFech))
  #Ajuste de formato Fecha
  if(typeOrig>0){
  #Si estas vienen con separador yyyy-mm-dd
  Data.all.files.OK[[i]]$Date=as.Date(as.character(Data.all.files[[i]]$Date))
  }else{Data.all.files.OK[[i]]$Date=as.Date(as.character(Data.all.files[[i]]$Date), "%Y%m%d")}#Si estas vienen SIN separador yyyymmdd
  
  #Conversion de horas
  TEMPhour=Data.all.files[[i]]$Hour
  Data.all.files.OK[[i]]$Hour=gsub("a.m.", "AM", Data.all.files.OK[[i]]$Hour)
  Data.all.files.OK[[i]]$Hour=gsub("p.m.", "PM", Data.all.files.OK[[i]]$Hour)
  Data.all.files.OK[[i]]$Hour=gsub("a,m,", "AM", Data.all.files.OK[[i]]$Hour)
  Data.all.files.OK[[i]]$Hour=gsub("p,m,", "PM", Data.all.files.OK[[i]]$Hour)
  Data.all.files.OK[[i]]$Hour=gsub("a", "AM", Data.all.files.OK[[i]]$Hour)
  Data.all.files.OK[[i]]$Hour=gsub("p", "PM", Data.all.files.OK[[i]]$Hour)
  
  Cond=((strptime(Data.all.files.OK[[i]]$Hour,"%I:%M:%S %p"))[1])#Condicion para identificar si la hora tiene segundos
  #Si Cons=NA no tiene segundos
  if(is.na(Cond)){TEMPhour<- strftime(strptime(Data.all.files.OK[[i]]$Hour,"%I:%M %p"),"%H:%M:%S")##SI formato original hora es 12:35 am/pm
  }else{TEMPhour<- strftime(strptime(Data.all.files.OK[[i]]$Hour,"%I:%M:%S %p"),"%H:%M:%S")}##SI formato original hora es 12:35:59 am/pm
 
  Data.all.files.OK[[i]]$Hour=times(TEMPhour)

  # Calculo del intervalo de tiempo
  Data.all.files.OK[[i]]["DIFF"]=NA
  HOURDATE=strptime(paste(Data.all.files.OK[[i]]$Date, Data.all.files.OK[[i]]$Hour), "%Y-%m-%d %H:%M") 
  Decal=c(HOURDATE[1],HOURDATE[-length(HOURDATE)])
  Data.all.files.OK[[i]]$DIFF=as.numeric(difftime(HOURDATE,Decal,units="mins"))
  
  # Reemplazar intervalo del primer dato de cada dia
  NUMDATA=aggregate(Data.all.files.OK[[i]], list(Data.all.files.OK[[i]]$Date), length)
  diasEst=dim(NUMDATA)[1] ###Dias que hay realmente
  PREMJOUR=cumsum(c(1,(NUMDATA$DIFF[-nrow(NUMDATA)])))
  MEDIANES=aggregate(Data.all.files.OK[[i]]$DIFF~Data.all.files.OK[[i]]$Date, Data.all.files.OK[[i]], median)
  Data.all.files.OK[[i]]$DIFF[PREMJOUR]=MEDIANES[,2]
  
  # Remplazar dias con menos de 3 datos con NA
  SINDATA=subset(NUMDATA, NUMDATA$Date<3)
  SINDATA.POS=which(!is.na(match(Data.all.files.OK[[i]]$Date, SINDATA[[1]])))
  Data.all.files.OK[[i]]$Value[SINDATA.POS]=NA
  Data.all.files.OK[[i]]$DIFF[SINDATA.POS]=NA
  REPORTSINDATA[i]=dim(SINDATA)[1] ###Dias que no podrán calcularse por escazos datos
 
 if(dim(SINDATA)[1]!=0){
  write.csv(SINDATA,paste0(Filesroutesdestino,nom.files[i], "_SINDATA.csv"),row.names=T)
  }
  perc[i]=(REPORTSINDATA[i]/diasEst)*100
  
  if(perc[i]>15){   
    diasperd=data.frame(REPORTSINDATA[i],perc[i],diasEst)
    colnames(diasperd)=c("Dias Perdidos", "% Dias Perdidos","Dias Reales")
    write.csv(diasperd, paste0(Filesroutesdestino,"DiasPerdidos_",nom.files[i],".csv"),row.names=T)  
  }
  # Detectar valores duplicados
  HOURDATE=strptime(paste(Data.all.files.OK[[i]]$Date, Data.all.files.OK[[i]]$Hour), "%Y-%m-%d %H:%M:%S")
  DUP=duplicated(HOURDATE)
    
  if(sum(DUP, na.rm=TRUE)>0){
    ProbDUP=TRUE
    print("Hay un problema de duplicados")
    DUPcsv=HOURDATE[which(DUP==TRUE)]
    write.csv(DUPcsv, paste0(Filesroutesdestino,nom.files[i], "_DUPlicados.csv"),row.names=T)
  }
  
  Data.all.files.OK2=Data.all.files.OK
    
  posiZero=(which(Data.all.files.OK2[[i]][,3]<=1))
  Data.all.files.OK2[[i]][posiZero,3]=NA###temporal
  sub=Data.all.files.OK2[[i]]
  rez1=sub$Value[-1]
  rez1[length(sub$Value)]=sub$Value[length(sub$Value)]
  difer=sub$Value-rez1
  sub2=cbind(sub,difer)
  sub2=subset(sub2,sub2$difer==0)
  
  if(dim(sub2)[1]!=0){
    summValrep=aggregate(sub2,list(sub2$Date),length)[,c(1,4)]
    summValrep=subset(summValrep,summValrep$Value>=1)
    DateProm=dim(summValrep)[1]
    percent=round(DateProm/diasEst*100,2)
    
    if(DateProm>diasEst*0.1){
      write.csv(summValrep,paste0(Filesroutesdestino,"ValRep_",percent,"_",nom.files[i],".csv"),row.names=F)
    }
  }
}

##*****************************

# Tabla de valores de referencia
REF=read.csv("D:/ToBackup/Unidad Z/Usaid_Fedecafe/datos/valoes de referencia/Val_REF_QCHour.csv", header=T, row.names=1)

# Detectar y remplazar los valores fuera de rango
if(length(VAReasy)!=0){
  for (j in 1:length(VAReasy)){

    modif=0
    
    UNIT=NA
    ProbCohernciaTEMP=FALSE
    
    IND=VAReasy[j]
    VAR=substring(nom.files[IND],nchar(nom.files[IND])-3, nchar(nom.files[IND]))
    VAR=toupper(VAR)    
    a=paste0("subset(Data.all.files.OK[[IND]],Data.all.files.OK[[IND]]$Value<REF$",VAR,"[2]|Data.all.files.OK[[IND]]$Value>REF$",VAR,"[1])")
   
    ###a partir de aqui me esta generando error
     error=eval(parse(text=a))
    #head(error)
    NUMBER=dim(error)[1]
    Indexes=as.numeric(row.names(error))
    Data.all.files.OK[[IND]]$Value[Indexes]=NA
    #summary(Data.all.files.OK[[IND]])
    # Control de coherencia temporal de la serie
    
    if(VAR=="ESOL"){
      UNIT=substring(nom.files[IND],nchar(nom.files[IND])-8, nchar(nom.files[IND])-5)
      UNIT=toupper(UNIT)
      if(UNIT=="WAM2"){
      Hmin=times(strftime(strptime("7:00 PM","%I:%M %p"),"%H:%M:00"))
      Hmax=times(strftime(strptime("4:30 AM","%I:%M %p" ),"%H:%M:00"))
      Noche=Data.all.files.OK[[IND]][(Data.all.files.OK[[IND]]$Hour>Hmin)|(Data.all.files.OK[[IND]]$Hour<Hmax),] 
      
      maxi=function(x){
        serie=na.omit(x)
        max=max(serie)
        return(max)
      }
      PROBLEM=aggregate(Noche$Value~Noche$Date, Noche, maxi)
       
      
      if(dim(PROBLEM[PROBLEM[,2]>0,])[1]!=0){
        ProbCohernciaTEMP=TRUE
        print(paste0("Problema de incoherencia temporal en la serie !!",nom.files[[IND]]))}
      }else{
        print("Unidad no contemplada")
      }
    }
  
    # Generacion del Reporte
    if(dim(error)[1]!=0){    
    write.csv(error, paste0(Filesroutesdestino,nom.files[IND],"_Datos_erroneos.csv"))
    }
    size=dim(Data.all.files[[IND]])[1]
    resul=c(size,REPORTSINDATA[IND],NUMBER,NUMBER/size*100,as.character(ProbCohernciaTEMP), as.character(ProbDUP))
    Tabla.fin=as.data.frame(resul,row.names=c("Numero de datos","Datos inusables por poca cantidad","Datos erroneos","% datos erroneos","Problema de coherencia temporal Radiacion", "Problema de duplicados"))
    write.csv(Tabla.fin,paste0(Filesroutesdestino,nom.files[IND],"_ResumenQC.csv"))
    write.table(Data.all.files.OK[[IND]],paste0(Filesroutesdestino,stations[IND],nom.files[IND],"_QC1ready.txt"), sep="\t",row.names=F)
  }
}

##
if(length(VARaccu)!=0){
  for (j in 1:length(VARaccu)){
    
    UNIT=NA
    ProbCohernciaTEMP=FALSE
    ProbUNIDAD=FALSE
    
    IND=VARaccu[j]
    VAR=substring(nom.files[IND],nchar(nom.files[IND])-3, nchar(nom.files[IND]))
    VAR=toupper(VAR)
    
    # Control RAIN
    if(VAR=="PREC"){
    #  Funcion que define el umbral de RAIN segun el intervalo de tiempo
      UmbralRAIN=function(x){
        if(is.na(x)){
          y=NA
        }else if(x<=60){y=-0.0207*x^2+34.239*x+15.424
                        
        }else{y=2000
              
        }
        return(y)
      }
      # Detectar valores fuera de rango
      error=subset(Data.all.files.OK[[IND]],Data.all.files.OK[[IND]]$Value<0|Data.all.files.OK[[IND]]$Value>lapply(Data.all.files.OK[[IND]]$DIFF, UmbralRAIN))
      NUMerror=dim(error)[1]
      # remplazar valores fuera de rango
      Indexes=as.numeric(row.names(error))
      Data.all.files.OK[[IND]]$Value[Indexes]=NA
      
      # Control de radiacion solar
      
    }else if(VAR=="ESOL"){
      #if(VAR=="ESOL"){
      UNIT=substring(nom.files[IND],nchar(nom.files[IND])-8, nchar(nom.files[IND])-5)
      UNIT=toupper(UNIT)
      
      # Control de coherencia temporal de la serie
      Hmin=times(strftime(strptime("7:00 PM","%I:%M %p"),"%H:%M:00"))
      Hmax=times(strftime(strptime("4:30 AM","%I:%M %p"),"%H:%M:00"))
      Noche=Data.all.files.OK[[IND]][(Data.all.files.OK[[IND]]$Hour>Hmin)|(Data.all.files.OK[[IND]]$Hour<Hmax),] 
      
      maxi=function(x){
        serie=na.omit(x)
        max=max(serie)
        return(max)
      }
      
      PROBLEM=aggregate(Noche$Value~Noche$Date, Noche, maxi)
      
      if(dim(PROBLEM[PROBLEM[,2]>0,])[1]!=0){
        ProbCohernciaTEMP=TRUE
        print(paste0("Problema de incoherencia temporal en la serie !!",nom.files[[IND]]))}
      
      # definicion de la funcion de umbral de ESOL
      UmbralESOL=function(x){
        y=-0.0007*x^2+1.9751*x+1.3536
        return(y)
      }
      
      # Detectar valores fuera de rango
      if(UNIT=="MJM2"){
        # conversion a ccm2
        Data.all.files.OK[[IND]]$Value=Data.all.files[[IND]]$Value*100/4.18
        # Deteccion
        error=subset(Data.all.files.OK[[IND]],Data.all.files.OK[[IND]]$Value<0|Data.all.files.OK[[IND]]$Value>UmbralESOL(Data.all.files.OK[[IND]]$DIFF))
        NUMerror=dim(error)[1]
        # remplazar valores fuera de rango
        Indexes=as.numeric(row.names(error))
        Data.all.files.OK[[IND]]$Value[Indexes]=NA
        modif=1
        
      }else if(UNIT=="CCM2"){
        # Deteccion
        error=subset(Data.all.files.OK[[IND]],Data.all.files.OK[[IND]]$Value<0|Data.all.files.OK[[IND]]$Value>UmbralESOL(Data.all.files.OK[[IND]]$DIFF))
        NUMerror=dim(error)[1]
        # remplazar valores fuera de rango
        Indexes=as.numeric(row.names(error))
        Data.all.files.OK[[IND]]$Value[Indexes]=NA
        
      }else{
        ProbUNIDAD=TRUE
        "Unidad no contemplada en el script"
      }
    }
    # Generacion del Reporte
    if(dim(error)[1]!=0){
      write.csv(error, paste0(Filesroutesdestino,nom.files[IND],"_Datos_erroneos.csv"))
    }
    size=dim(Data.all.files[[IND]])[1]
    resul=c(size,REPORTSINDATA[IND],NUMerror,NUMerror/size*100,as.character(ProbCohernciaTEMP), as.character(ProbUNIDAD), as.character(ProbDUP))
    Tabla.fin=as.data.frame(resul,row.names=c("Numero de datos","Datos inusables por poca cantidad","Datos erroneos","% datos erroneos","Problema de coherencia temporal Radiacion", "Problema de unidad", "Problema de duplicados"))
    write.csv(Tabla.fin,paste0(Filesroutesdestino,nom.files[IND],"_ResumenQC.csv"))
    
    if(modif==1){
      varCCM2=paste0(substring(nom.files[IND],first=1,nchar(nom.files[IND])-9),"CCM2_ESOL")
      write.table(Data.all.files.OK[[IND]],paste0(Filesroutesdestino,stations[IND],varCCM2,"_QC1ready.txt"), sep="\t")
    }else{
      write.table(Data.all.files.OK[[IND]],paste0(Filesroutesdestino,stations[IND],nom.files[IND],"_QC1ready.txt"), sep="\t")
    }
  }
}

##################### FIN ################