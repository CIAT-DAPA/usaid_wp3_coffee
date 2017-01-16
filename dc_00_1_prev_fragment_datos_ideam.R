#######################################################################
#########################################READ DATA IDEAM###############
##PROYECTO USAID CAFE RESILIENTE EN RISARALDA
##ELIANA VALLEJO
##2016
##OBJETIVO: Leer los datos del formato enviado por IDEAM, extrayendo 
############ información de estaciones y datos enviados

##load packages
library(dplyr);library(plyr);library(reshape2)
##end load packages
##load directories
dir_in<-"Z:/Usaid_Fedecafe/datos/ideam_solicitud/"
##end load directories

# read in the raw file
lines <- readLines(paste0(dir_in,"data_ideam.txt"))
lines<-gsub("\t",",",lines)
estaciones <- grep("ESTACION", lines)
estaciones<-substr(lines[estaciones],43,50)
estaciones_name<-substr(lines[ grep("ESTACION", lines)],52,82)
estaciones_name<-gsub(",","",estaciones_name)
variables<-grep("VALORES",lines)
variables<-as.factor(substr(lines[variables],2,43))
levels(variables)<-c("tmax","tmean","tmin","prec")
list_names<-paste0(estaciones,"_",variables)
start_p<-grep("A#O",lines)+3
linesread<-grep("SISTEMA",lines)
linesread<-c(linesread[-1],length(lines))
list_datos=lapply(seq_along(start_p),function(i)read.table(paste0(dir_in,"data_ideam.txt"),skip=start_p[i]-1,nrow=linesread[i]-start_p[i],sep="\t",quote="", fill = TRUE,dec="."))
names(list_datos)<-list_names
list_datos_df<-ldply(list_datos)
years<-as.factor(1940:2016)
list_datos_df<-list_datos_df[which(list_datos_df$V2%in%years),]
list_datos_df<-list_datos_df[,c(".id","V2",paste0("V",5:16))]
colnames(list_datos_df)[3:14]<-paste0("m",1:12)
list_datos_df<-melt(list_datos_df,id.vars=c(".id","V2"))
list_datos_df$variable1<- sprintf("%02d",as.numeric(gsub("m","",list_datos_df$variable)))
list_datos_df$Date<-paste0(list_datos_df$V2,"-",list_datos_df$variable1,"-25")
list_datos_df$value<-ifelse(list_datos_df$value=="3",NA,list_datos_df$value)
list_datos_df$estaciones<-substr(list_datos_df$.id,1,8)

####info estaciones ideam
elevacion<-grep("ELEVACION",lines)
elevacion<-substr(lines[elevacion],12,15)
elevacion<-as.numeric(gsub("([0-9]+).*$", "\\1", elevacion))*1

latitud<-grep("LATITUD",lines)
latitud<-substr(lines[latitud],10,12)
latitud<-as.numeric(gsub("([0-9]+).*$", "\\1", latitud))/100

longitud<-grep("LONGITUD",lines)
longitud<-substr(lines[longitud],11,14)
longitud<-as.numeric(gsub("([0-9]+).*$", "\\1", longitud))/-100

dpto<-grep("DEPTO",lines)
dpto<-as.factor(substr(lines[dpto],34,42))
levels(dpto)<- c("Caldas","Risaralda","Tolima","Valle") 
dpto<-as.character(dpto)
estaciones_info<-data.frame(estaciones,estaciones_name,dpto,latitud,longitud,elevacion)

estaciones_info=estaciones_info[estaciones_info$longitud>=-77.5 & estaciones_info$longitud<=-75.5,]
estaciones_info=estaciones_info[estaciones_info$latitud >=-3.5 & estaciones_info$latitud<=6.5,]

##write data in txt
write.table(estaciones_info,"Z:/Usaid_Fedecafe/datos/info_estaciones/estaciones_ideam_solicitud.txt",row.names=F,sep=",")
list_datos_df1<-list_datos_df[which(list_datos_df$estaciones%in%as.character(unique(estaciones_info$estaciones))),]
list_datos<-split(list_datos_df,list_datos_df$.id)
lapply(seq_along(list_datos),function(x)write.table(list_datos[[x]][,c("Date","value")],
                                                    paste0("Z:/Usaid_Fedecafe/datos/ideam_solicitud/monthly_raw/monthly_row_selecc/",names(list_datos)[x],".txt"),row.names=F,sep=","))

