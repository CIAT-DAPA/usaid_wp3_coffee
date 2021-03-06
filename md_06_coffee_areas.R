## Set parameters
library(raster);library(maptools);library(rgdal);library(sp)

src.dir <- "C:/_tools/usaid_wp3_coffee/usaid_wp3_coffee/src-impacts"
bd <- "D:/OneDrive - CGIAR/CIAT/Projects/col-usaid/04_suitability/ensemble_modeling"
iDir <- "D:/OneDrive - CGIAR/CIAT/Projects/col-usaid/04_suitability/ensemble_modeling/concenso"
oDir <- "D:/OneDrive - CGIAR/CIAT/Projects/col-usaid/04_suitability/ensemble_modeling/impacts_v4"
cropLs <- c("coffee")
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
periodLs <- c("2020", "2030")
shpStudyArea <- "D:/OneDrive - CGIAR/CIAT/Projects/col-usaid/02_monthly_interpolation/region/shp_AEPS/Risaralda_npa.shp"
shpSubLevel <- "D:/OneDrive - CGIAR/CIAT/Projects/col-usaid/02_monthly_interpolation/region/shp_AEPS/RIS1_adm_v3_npa.shp"


### Note: In the shapefile must have a field call "COUNTRY" with the name of the country or the districts. 

########  Calculate impact metrics for the study area  ######
sh <- readShapePoly(shpStudyArea)

#Source scripts and libraries
stop("no")
source(paste(src.dir,"/createMask.R",sep=""))
source(paste(src.dir,"/futureRuns.tmp.R",sep=""))
source(paste(src.dir,"/impacts.R",sep=""))
source(paste(src.dir,"/uncertainty.R",sep=""))

cat("Calculate impact metrics for the area of study\n")

for(crop in cropLs){
  
  if (!file.exists(paste(oDir, "/", crop, "/impacts-ris.csv", sep=""))) {
    
  # crop <- cropLs[1]
  impDir <- paste(oDir, "/", crop, sep="")
  if (!file.exists(impDir)) {dir.create(impDir, recursive = T)}
  
  for (rcp in rcpLs){
    
    rcpDir <- paste0(iDir, "/", crop, "/runs-", rcp)
    gls <- list.files(rcpDir, full.names = F, include.dirs = F)
    gls <- gls[gls != "desktop.ini"] 
    
    for(period in periodLs){
      
        for (gcm in gls) {
          
          cat("\tImpact StudyArea ", gcm, "\n")
          
          od <- paste(oDir, "/", crop, "/", gcm, "/", rcp, "_", period, sep="")  
          if (!file.exists(od)) {dir.create(od, recursive = T)}
          
          r1 <- raster(paste0(iDir, "/", crop, "/runs/", crop, "_suit.tif")) /10
          r2 <- raster(paste0(rcpDir, "/", gcm, "/", period, "/", crop, "_suit.tif"))
          pp <- iMetrix(r1,r2,sh,od, chggrid=T, impact=T, classes=T)
          
          # im <- cbind(GCM=rep(gcm,times=nrow(pp$IMPACT)), pp$IMPACT)
          # 
          # if (gcm == gls[1]) {
          #   res.im <- im
          # } else {
          #   res.im <- rbind(res.im, im)
          # }
          # 
          im <- cbind(RCP=rep(rcp,times=nrow(pp$IMPACT)),PERIOD=rep(period,times=nrow(pp$IMPACT)),GCM=rep(gcm,times=nrow(pp$IMPACT)), pp$IMPACT)
          
          if (gcm == gls[1] && rcp == rcpLs[1] && period == periodLs[1]) {
            res.im <- im
          } else {
            res.im <- rbind(res.im, im)
          }
          
          
          rm(im) #; rm(cl)
        }
        

        
      } 
      
      
    }
    
  write.csv(res.im, paste(oDir, "/", crop, "/impacts-ris.csv", sep=""), quote=T, row.names=F)
  cat("Calcs impact metrics for the area of study done\n")
  
  } else {cat("Calcs impact metrics for the area of study done!\n")}
  
}


########  Calculate impact metrics for subadministrative levels  ######

sh <- readOGR(shpSubLevel)

#Source scripts and libraries
stop("no")
source(paste(src.dir,"/createMask.R",sep=""))
source(paste(src.dir,"/futureRuns.tmp.R",sep=""))
source(paste(src.dir,"/impacts.R",sep=""))
source(paste(src.dir,"/uncertainty.R",sep=""))

cat("Calculate impact metrics for subadministrative levels\n")

for(crop in cropLs){
  
  if (!file.exists(paste(oDir, "/", crop, "/impacts-rwa-sub.csv", sep=""))) {
    
    impDir <- paste(oDir, "/", crop, sep="")
    if (!file.exists(impDir)) {dir.create(impDir, recursive = T)}
    
    for (rcp in rcpLs){
      
      rcpDir <- paste0(iDir, "/", crop, "/runs-", rcp)
      gls <- list.files(rcpDir, full.names = F, include.dirs = F)
      gls <- gls[gls != "desktop.ini"] 
      
      for(period in periodLs){
       
        for (gcm in gls) {
          
          cat("\tImpact StudyArea ", rcp, gcm, period, "\n")
          
          od <- paste(oDir, "/", crop, "/", gcm, "/", rcp, "_", period, sep="")  
          if (!file.exists(od)) {dir.create(od, recursive=T)}
          
          r1 <- raster(paste0(iDir, "/", crop, "/runs/", crop, "_suit.tif")) / 10
          r2 <- raster(paste0(rcpDir, "/", gcm, "/", period, "/", crop, "_suit.tif"))
          pp <- iMetrix(r1,r2,sh,od, chggrid=T, impact=T, classes=T)
          
          im <- cbind(RCP=rep(rcp,times=nrow(pp$IMPACT)),PERIOD=rep(period,times=nrow(pp$IMPACT)),GCM=rep(gcm,times=nrow(pp$IMPACT)), pp$IMPACT)
          
          if (gcm == gls[1] && rcp == rcpLs[1] && period == periodLs[1]) {
            res.im <- im
          } else {
            res.im <- rbind(res.im, im)
          }
          rm(im) #; rm(cl)
        }

      }
      
    }
    
    write.csv(res.im, paste(oDir, "/", crop, "/impacts-ris-sub.csv", sep=""), quote=T, row.names=F)
    cat("Calcs impact metrics for subadministrative levels done!", crop, "\n")
    
  } else {cat("Calcs impact metrics for subadministrative levels done!", crop, "\n")}
}





src.dir <- "C:/_tools/usaid_wp3_coffee/usaid_wp3_coffee/src-impacts"
bd <- "D:/OneDrive - CGIAR/CIAT/Projects/col-usaid/04_suitability/ensemble_modeling"
iDir <- "D:/OneDrive - CGIAR/CIAT/Projects/col-usaid/04_suitability/ensemble_modeling/concenso"
oDir <- "D:/OneDrive - CGIAR/CIAT/Projects/col-usaid/04_suitability/ensemble_modeling/impacts_v4"
cropLs <- c("coffee")
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
periodLs <- c("2020", "2030")
shpStudyArea <- "D:/OneDrive - CGIAR/CIAT/Projects/col-usaid/02_monthly_interpolation/region/shp_AEPS/Risaralda_npa.shp"
shpSubLevel <- "D:/OneDrive - CGIAR/CIAT/Projects/col-usaid/02_monthly_interpolation/region/shp_AEPS/RIS1_adm_v3_npa.shp"


### Note: In the shapefile must have a field call "COUNTRY" with the name of the country or the districts. 

########  Calculate impact metrics for the study area  ######
sh <- readShapePoly(shpSubLevel)

cat("Calculate total area for the area of study\n")

for(crop in cropLs){
  
  # crop <- cropLs[1]
  impDir <- paste(oDir, "/", crop, sep="")
  if (!file.exists(impDir)) {dir.create(impDir, recursive = T)}
  
  r1 <- raster(paste0(iDir, "/", crop, "/runs/", crop, "_suit.tif")) /10
  
  thr <- 50
  
  r1con = (r1 >= thr) 
  r1con[!r1con]=0
  r1con[!is.na(r1con)]=1
  
  # Convert polygons to raster with an specific ID
  sh_crop <- crop(sh, r1con)
  extent(r1con) <- extent(sh_crop)
  sh_rs <- rasterize(sh_crop, r1con, 'NEWID')
  
  # Get the zonal statistics
  r1zonal <- zonal(r1con, sh_rs, 'sum')
  
  mat <- cbind(MUN=paste(sh$ID_ESPACIA), CURRENT=r1zonal[,2]/4)
  
  
  for (rcp in rcpLs){
    
    rcpDir <- paste0(iDir, "/", crop, "/runs-", rcp)
    gls <- list.files(rcpDir, full.names = F, include.dirs = F)
    gls <- gls[gls != "desktop.ini"] 
    
    for(period in periodLs){
      
      for (gcm in gls) {
        
        cat("\tImpact StudyArea ", gcm, "\n")
        
        od <- paste(oDir, "/", crop, "/", gcm, "/", rcp, "_", period, sep="")  
        if (!file.exists(od)) {dir.create(od, recursive = T)}
        
        r2 <- raster(paste0(rcpDir, "/", gcm, "/", period, "/", crop, "_suit.tif"))
        
        r2con = (r2 >= thr) 
        r2con[!r2con]=NA
        r2con[!is.na(r2con)]=1
        
        # Convert polygons to raster with an specific ID
        sh_crop <- crop(sh, r2con)
        extent(r2con) <- extent(sh_crop)
        sh_rs2 <- rasterize(sh_crop, r2con, 'NEWID')
        
        # Get the zonal statistics
        r2zonal <- zonal(r2con, sh_rs2, 'sum')
        
        mat <- cbind(mat, r2zonal[,2]/4)
        
      }
      
    } 
    
  }
  
  write.csv(mat, paste(oDir, "/", crop, "/impacts-area-ris-sub.csv", sep=""), quote=T, row.names=F)
  cat("Calcs impact metrics for the area of study done\n")
  
  
}




### Note: In the shapefile must have a field call "COUNTRY" with the name of the country or the districts. 

########  Calculate impact metrics for the study area  ######
sh <- readShapePoly(shpStudyArea)

cat("Calculate total area for the area of study\n")

for(crop in cropLs){
  
  # crop <- cropLs[1]
  impDir <- paste(oDir, "/", crop, sep="")
  if (!file.exists(impDir)) {dir.create(impDir, recursive = T)}
  
  r1 <- raster(paste0(iDir, "/", crop, "/runs/", crop, "_suit.tif")) /10
  
  thr <- 50
  
  r1con = (r1 >= thr) 
  r1con[!r1con]=0
  r1con[!is.na(r1con)]=1
  
  # Convert polygons to raster with an specific ID
  sh_crop <- crop(sh, r1con)
  extent(r1con) <- extent(sh_crop)
  sh_rs <- rasterize(sh_crop, r1con, 'NEWID')
  
  # Get the zonal statistics
  r1zonal <- zonal(r1con, sh_rs, 'sum')
  
  mat <- cbind(MUN=paste(sh$ID_ESPACIA), CURRENT=r1zonal[,2]/4)
  
  
  for (rcp in rcpLs){
    
    rcpDir <- paste0(iDir, "/", crop, "/runs-", rcp)
    gls <- list.files(rcpDir, full.names = F, include.dirs = F)
    gls <- gls[gls != "desktop.ini"] 
    
    for(period in periodLs){
      
      for (gcm in gls) {
        
        cat("\tImpact StudyArea ", gcm, "\n")
        
        od <- paste(oDir, "/", crop, "/", gcm, "/", rcp, "_", period, sep="")  
        if (!file.exists(od)) {dir.create(od, recursive = T)}
        
        r2 <- raster(paste0(rcpDir, "/", gcm, "/", period, "/", crop, "_suit.tif"))
        
        r2con = (r2 >= thr) 
        r2con[!r2con]=NA
        r2con[!is.na(r2con)]=1
        
        # Convert polygons to raster with an specific ID
        sh_crop <- crop(sh, r2con)
        extent(r2con) <- extent(sh_crop)
        sh_rs2 <- rasterize(sh_crop, r2con, 'NEWID')
        
        # Get the zonal statistics
        r2zonal <- zonal(r2con, sh_rs2, 'sum')
        
        mat <- cbind(mat, r2zonal[,2]/4)
        
      }
      
    } 
    
  }
  
  write.csv(mat, paste(oDir, "/", crop, "/impacts-area-ris.csv", sep=""), quote=T, row.names=F)
  cat("Calcs impact metrics for the area of study done\n")
  
  
}
