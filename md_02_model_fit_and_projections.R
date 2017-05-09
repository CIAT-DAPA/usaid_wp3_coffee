##########################################################################################
## Purpose: Species distribution modelling of coffee robusta
## Author: Eliana Vallejo
##dpto Risaralda
##########################################################################################
list.of.packages <- c("raster", "biomod2","dismo","gridExtra","reshape2","maptools","rgdal","sp")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
##load packages
require(raster);require(maptools);require(rgdal);library(sp)
library(biomod2)
library(dismo)
library(gridExtra);library(reshape2)
#######################################################################
#############################Params####################################
#######################################################################
#modeling
evot<-T ##si es T se incluye la evapotranspiracion de referencia
add_suelos<-"yes"  #para anadir suelos
##la cual no varia a futuro por que se calcula a partir de la altitud
cor_multi<-.7  ##correlacion a partir de la cual puede haber multicolinealidad
sampl<-100##porcentaje de datos usado para ajuste de modelo 100 default
p_train<-70 #porcentaje de datos con los que se entrena el modelo
ext_ref<-extent(-76.35875, -75.40875, 4.67125, 5.475417)   ##cuadrado de modelación 
####NOTA: PROBAR CONEL 60% PARA SACAR EL MTREE PARA RF DE ENTRENAMIENT
n_runs<-25 #numero de repeticiones de cada modelo default 25
typ_model<-  c("GAM","GLM","MAXENT.Tsuruoka","MAXENT.Phillips",'GBM', "RF")  ##se recomienda ejecutar aparte los modelos de regresion y 
#hacer para estos las pseudo-ausencias aleatorias
strategy<-"sre"  ##para el caso de modelos de regresión se recomienda usar "random"
replicates<-15 ##replicas de pseusdo-ausencias
install_maxent<-"//dapadfs/Workspace_cluster_9/USAID_Project/Product_6_resilient_coffee/04-suitability/maxent/maxent.jar" ##maxent path
eval_model<- c('TSS',"ROC","KAPPA",'ACCURACY') ##medidas de evaluación de los modelos
##projections
proj_name<-'current'  ##nombre de la proyeccion
sel_model<-"all" #seleccion de modelos
sp_name<-"coffee" #nmobre variable respuesta 
name_eval_vi<-"prueba" #nombre
selec_var<-c("prec_acum_djf","cv_prec_djf","TA_acum_djf","sd_tmean_ann") ###nombre de las variables para ajustar los modelos seleccionadas a partir de la descripción previa
#future
rcp<-c("rcp26","rcp45","rcp60","rcp85")
futuro<-c(20,30)
mask1 <-readOGR("//dapadfs/Workspace_cluster_9/USAID_Project/Product_6_resilient_coffee/02-monthly-interpolations/region/mask/ris_adm0_wgs84.shp",layer="ris_adm0_wgs84")
alt<-raster("//dapadfs/Workspace_cluster_9/USAID_Project/Product_6_resilient_coffee/02-monthly-interpolations/region/500m_v4/alt-prj-ris.asc")
ideam_models<-c("giss_e2_h","bcc_csm1_1", "ncar_ccsm4","csiro_mk3_6_0","fio_esm","gfdl_cm3","giss_e2_r","nimr_hadgem2_ao","ipsl_cm5a_lr", "ipsl_cm5a_mr",
                "miroc_esm","miroc_esm_chem","miroc_miroc5","mri_cgcm3")

########LOAD PATHS
bdir<-"//dapadfs/Workspace_cluster_9/USAID_Project/Product_6_resilient_coffee/04-suitability/" 
odir<-"//dapadfs/Workspace_cluster_9/USAID_Project/Product_6_resilient_coffee/04-suitability/results"
##current 
current<-paste0(bdir,"indices_agroclimaticos/current/")
#######################################################################################################################
####################################END PARAMS
##################AJUSTANDO MODELOS####################################################################################
#######################################################################################################################
#NOTA: Se podrian unicamente cargar los layers de las variables seleccionadas pero se prefiere cargarlo todo en caso de 
#requerir de modificaciones u otras pruebas
evo_t<-raster(paste0(bdir,"indices_agroclimaticos/evot_ref.tif"))
clima_current<-stack(lapply(list.files(current,full.names = T,pattern=".asc"),function(x)stack(x)))
clima_current<-crop(clima_current,ext_ref) 
clima_current<-stack(crop(alt,clima_current),clima_current)
names(clima_current)[1]<-"alt"
if(evot==T){clima_current<-stack(resample(evo_t, clima_current, method="ngb"),clima_current)}
clima_current<-stack(mask(clima_current,mask1))
crs(clima_current) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
###Anadir suelos se rasteriza unicamente ph y materia organica ya que se descartaron las texturas en la descripcíon previa
suelos<-readOGR(paste0(bdir,"indices_agroclimaticos/suelos/shp/Risaralda_suelos_MCB1_var.shp"),layer="Risaralda_suelos_MCB1_var")
suelos<-spTransform(suelos, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ph<- rasterize(suelos, clima_current[[1]], 'pH_d_',fun=mean,mask=F,na.rm=T)
names(ph)<-"ph"
mo<-rasterize(suelos, clima_current[[1]], 'Mtrrg')
names(mo)<-"om"
suelos<-stack(ph,mo)
rm(ph,mo)
if(add_suelos=="yes"){clima_current<-stack(resample(suelos, clima_current, method="ngb"),clima_current)}
presencia1<-read.csv(paste0(bdir,"presencia/presencia_ausencia/presencia/presencia.csv"),header=T) ##load presence points
###################################LOAD CLIMA
clima_current<-clima_current[[selec_var]]
###sampled
p<-sample(1:nrow(presencia1),round((sampl/100)*nrow(presencia1),0))
p<-presencia1[p,]
p<-p[,1:2]
coordinates(p)<-~x+y
myBiomodData<- BIOMOD_FormatingData(resp.var = p, expl.var = clima_current,resp.name = sp_name,PA.nb.rep =replicates,PA.nb.absences =length(p), PA.strategy = strategy)
plot(myBiomodData)
###a continuación se exponen los parametros de cada modelo, algunos son por defecto de biomod2, pero se pueden modificar según requerimientos del usuario
myBiomodOption<- BIOMOD_ModelingOptions(
  ###regresion models params 
  GAM = list( type = 's_smoother',
              k = 3,
              interaction.level = 0,
              myFormula = NULL,
              family = binomial(link = 'logit'),
              method = 'GCV.Cp',
              optimizer = c('outer','newton'),
              select = TRUE,
              knots = NULL,
              paraPen = NULL,
              control = list(nthreads = 1, irls.reg = 0, epsilon = 1e-07, maxit = 200, trace = FALSE ,
                             mgcv.tol = 1e-07, mgcv.half = 15, rank.tol = 1.49011611938477e-08,
                             nlm = list(ndigit=7, gradtol=1e-06, stepmax=2, steptol=1e-04, iterlim=200, check.analyticals=0),
                             optim = list(factr=1e+07), newton = list(conv.tol=1e-06, maxNstep=5, maxSstep=2, maxHalf=30, use.svd=0),
                             outerPIsteps = 0, idLinksBases = TRUE, scalePenalty = TRUE, keepData = FALSE) ),
  GLM = list( type = 'quadratic',
              interaction.level = 0,
              myFormula = NULL,
              test = 'BIC',
              family = 'binomial',
              control = glm.control(epsilon = 1e-08, 
                                    maxit = 1000, 
                                    trace = FALSE)),    
              ##machine learning models
  GBM = list( distribution = 'bernoulli',
                                                    n.trees = 2000,
                                                    interaction.depth = 7,
                                                    n.minobsinnode = 5,
                                                    shrinkage = 0.001,
                                                    bag.fraction = 0.5,
                                                    train.fraction = 1,
                                                    cv.folds = 3,
                                                    keep.data = FALSE,
                                                    verbose = FALSE,
                                                    perf.method = 'cv'),
                                        
                                        RF = list( do.classif = TRUE,
                                                   ntree = 500,
                                                   mtry = 2,
                                                   nodesize = 5,
                                                   maxnodes = NULL),
                                        MAXENT.Phillips= list(path_to_maxent.jar = install_maxent,memory_allocated = 512,
                                                              background_data_dir = 'default',
                                                              maximumbackground = 'default',
                                                              maximumiterations = 200,
                                                              visible = FALSE,
                                                              linear = F,
                                                              quadratic = TRUE,
                                                              product = TRUE,
                                                              threshold = F,
                                                              hinge = F,
                                                              betamultiplier = 1,
                                                              defaultprevalence = 0.5)
)

# 3. Ajustando modelos
myBiomodModelOut <- BIOMOD_Modeling(myBiomodData, models =  typ_model,models.options = myBiomodOption,NbRunEval=n_runs,DataSplit=p_train, VarImport=3,Prevalence=0.5,
                                    models.eval.meth = eval_model,SaveObj = TRUE, modeling.id="test",do.full.models=T, rescal.all.models = FALSE)

###EVALUACION DE LOS MODELOS
myBiomodModelEval <- getModelsEvaluations(myBiomodModelOut)
model_eval_df <- as.data.frame.table(myBiomodModelEval)
colnames(model_eval_df)<-c("eval","metrics","model","run","PA","value")
model_eval_df$model<-as.factor(model_eval_df$model)
odir_ev<-paste0(odir,"/evaluacion de modelos/")
if (!file.exists(odir_ev)) {dir.create(odir_ev,recursive=T)} else {cat("Folder exists")}
write.csv(model_eval_df,paste0(odir_ev,"/evaluation_",name_eval_vi,".csv"),row.names=F) ##añadir nombre como parametro
rm(odir_ev)
###importancia de variables
var_import<-data.frame(getModelsVarImport(myBiomodModelOut))
var_import$var<-rownames(var_import)
var_import<-melt(var_import,id.vars="var")
var_import$model<-as.factor(substr(var_import$variable,1,3))
odir_vi<-paste0(odir,"/importancia de variables/")
if (!file.exists(odir_vi)) {dir.create(odir_vi,recursive=T)} else {cat("Folder exists")}
write.csv(var_import,paste0(odir_vi,"var_import_",name_eval_vi,".csv"),row.names=F)
rm(odir_vi)
##############################PROYECCIONES PRESENTES
odir_current<-paste0(odir,"/current")
if (!file.exists(odir_current)) {dir.create(odir_current,recursive=T)} else {cat("Folder exists")}
myBiomomodProj <- BIOMOD_Projection(modeling.output = myBiomodModelOut,new.env = stack(clima_current),proj.name = proj_name,
                                    binary.meth = NULL,clamping.mask = T)
project_current<-myBiomomodProj 
rcpraster<-getProjection(myBiomomodProj)
writeRaster(rcpraster,filename=paste0(odir_current,"/",names(rcpraster)), bylayer=TRUE,format="GTiff",overwrite=TRUE )
##############################PROYECCIONES FUTURAS
for(m in 1:length(ideam_models)){
  for(f in 1:length(rcp)){
    for(fut in 1:length(futuro)){
      idir_f<-bdir
      fut<-paste0("futuro_",futuro[fut])
      rcpv<-rcp[f]
      model=ideam_models[m]
      idir_f<-paste0(idir_f,"/",fut,"/",rcpv,"/",model)
      odir_f<-paste0(odir,"/",fut,"/",rcpv,"/",model)  
      if (!file.exists(odir_f)) {dir.create(odir_f,recursive=T)} else {cat("Folder exists")}
        clima<-stack(lapply(list.files(idir_f,full.names = T,pattern=".tif"),function(x)stack(x)))
      clima<-mask(crop(clima,clima_current[[1]]),clima_current[[1]])
      alt<-mask(crop(alt,clima_current[[1]]),clima_current[[1]])
      clima<-stack(alt ,clima)
      names(clima)[1]<-"alt"
      if(evot==T){
        clima<-stack(resample(evo_t, clima, method="ngb"),clima)
            }
      if(add_suelos=="yes"){
        clima<-stack(resample(suelos, clima, method="ngb"),clima)
      }
      clima<-clima[[names(clima_current)]] 
      clima<-mask(crop(clima,clima_current[[1]]),clima_current[[1]])

      crs(clima) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
      project_fut<-BIOMOD_Projection(modeling.output = myBiomodModelOut,new.env = stack(clima),proj.name =  paste0(rcpv,"_",model),
                                     clamping.mask = T)
      writeRaster(getProjection( project_fut),filename=paste0(odir_f,"/",names(getProjection( project_fut))), bylayer=TRUE,format="GTiff",overwrite=TRUE )
      
    }}}

