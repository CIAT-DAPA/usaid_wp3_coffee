# Load libraries
require(raster)
require(rasterVis)
require(maptools)
require(rgdal)
require(plyr)

#########################
## Current suitability ##
#########################

iDir <- "Y:/04-suitability/indices_agroclimaticos"
oDir <- "Y:/04-suitability/results/ejecucion_mayo/performance_suitability"
mask <- readOGR("Y:/02-monthly-interpolations/region/mask/ris_adm0_wgs84.shp", layer= "ris_adm0_wgs84")
geotopo <- readOGR("Y:/04-suitability/presencia/presencia_ausencia/presencia/presencia_v2_npa_ris.shp", layer= "presencia_v2_npa_ris")
shpStudyArea <- readOGR("D:/OneDrive - CGIAR/CIAT/Projects/col-usaid/02_monthly_interpolation/region/shp_AEPS/Risaralda.shp")
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
rcpLsNames <- c("Current", "RCP 2.6", "RCP 4.5", "RCP 6.0", "RCP 8.5")
varLs <- c("ta_acum_djf", "prec_acum_djf", "cv_prec_djf", "sd_tmean_ann")
maxLs <- c(3000, 1200, 40, 3)
colorLs <- c("YlGn", "YlGnBu", "Blues", "Reds")

# geotopo <- readOGR("Y:/02-monthly-interpolations/region/mask/ecotopo_prj_intersect_npa.shp", layer= "ecotopo_prj_intersect")
# geotopo_simdiff <- readOGR("Y:/02-monthly-interpolations/region/mask/ecotopo_prj_simdiff.shp", layer= "ecotopo_prj_simdiff")

if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}

for (i in 1:length(varLs)){

  var <- varLs[i]
  rsC <- raster(paste0(iDir, "/current/", var, ".tif"))
  rsF <- stack(paste0(iDir, "/ensemble/futuro_30/", rcpLs, "/", var, ".tif"))
  
  stk <- stack(rsC, rsF)
  stk_crop <- mask(crop(stk, extent(shpStudyArea)), shpStudyArea)
  
  plot <- setZ(stk_crop, rcpLsNames)
  names(plot) <- toupper(rcpLsNames)
  
  zmin <- min(minValue(stk_crop))
  unit <- ( maxLs[i] - zmin) / 9

  zvalues <- seq(zmin, maxLs[i], unit) # Define limits
  myTheme <- BuRdTheme() # Define squeme of colors
  myTheme$regions$col = brewer.pal(9, colorLs[i])
  myTheme$strip.border$col = "white" # Eliminate frame from maps
  myTheme$axis.line$col = 'white' # Eliminate frame from maps
  
  
  tiff(paste(oDir, "/", var, ".tif", sep=""), width=3000, height=750, pointsize=8, compression='lzw',res=300)
  
  print(levelplot(plot, at = zvalues, 
                  layout=c(5, 1), 
                  scales = list(draw=FALSE),
                  xlab="", 
                  ylab="", 
                  par.settings = myTheme, 
                  colorkey = list(space = "right",width=1.2, height=0.8) 
                  # xlim=c(-76.22, -75.36), 
                  # ylim=c(4.6, 5.6)
  )
  + layer(sp.polygons(geotopo, col= "black", fill = "transparent"))
  )
  
  
  dev.off()  
  
}

  
