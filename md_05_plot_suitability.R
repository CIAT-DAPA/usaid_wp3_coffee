# Load libraries
require(raster)
require(rasterVis)
require(maptools)
require(rgdal)
require(plyr)

#########################
## Current suitability ##
#########################

iDir <- "Y:/04-suitability/results/ejecucion_mayo/current/ensemble"
oDir <- "Y:/04-suitability/results/ejecucion_mayo/performance_suitability"
mask <- readOGR("Y:/02-monthly-interpolations/region/mask/ris_adm0_wgs84.shp", layer= "ris_adm0_wgs84")
geotopo <- readOGR("Y:/04-suitability/presencia/presencia_ausencia/presencia/presencia_v2_npa_ris.shp", layer= "presencia_v2_npa_ris")
geotopo_simdiff <- readOGR("Y:/04-suitability/presencia/presencia_ausencia/presencia/no_presencia_v2_npa_ris.shp", layer ="no_presencia_v2_npa_ris")

# geotopo <- readOGR("Y:/02-monthly-interpolations/region/mask/ecotopo_prj_intersect_npa.shp", layer= "ecotopo_prj_intersect")
# geotopo_simdiff <- readOGR("Y:/02-monthly-interpolations/region/mask/ecotopo_prj_simdiff.shp", layer= "ecotopo_prj_simdiff")

if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}

varLs <- c("_GAM", "_GBM", "_GLM", "_MAXENT.Tsuruoka", "_RF", "current")  
varLsNames <- c("GAM", "GBM", "GLM", "MAXENT", "RF", "ENSEMBLE")

stk <- stack(paste0(iDir, "/suit_mean_", varLs, ".tif"))
stk_crop <- mask(crop(stk, extent(geotopo)) / 10, geotopo)

plot <- setZ(stk_crop, varLsNames)
names(plot) <- toupper(varLsNames)

zvalues <- seq(0, 100, 10) # Define limits
myTheme <- BuRdTheme() # Define squeme of colors
myTheme$regions$col = brewer.pal(10, "RdYlGn")
# myTheme$strip.border$col = "white" # Eliminate frame from maps
myTheme$axis.line$col = 'gray' # Eliminate frame from maps


tiff(paste(oDir, "/suitability_current.tif", sep=""), width=3000, height=2800, pointsize=8, compression='lzw',res=300)
  
print(levelplot(plot, at = zvalues, 
                layout=c(3, 2), 
                xlab="", 
                ylab="", 
                par.settings = myTheme, 
                colorkey = list(space = "bottom"),
                xlim=c(-76.22, -75.36), 
                ylim=c(4.6, 5.6)
                )
      + layer(sp.polygons(geotopo_simdiff, col= "transparent", fill = "gray95"))
      )

  
dev.off()
  


########################
## Future suitability ##
########################

iDir <- "Y:/04-suitability/results/ejecucion_mayo/ensemble_modeling"
oDir <- "Y:/04-suitability/results/ejecucion_mayo/performance_suitability"
mask <- readOGR("Y:/02-monthly-interpolations/region/mask/ris_adm0_wgs84.shp", layer= "ris_adm0_wgs84")
geotopo <- readOGR("Y:/04-suitability/presencia/presencia_ausencia/presencia/presencia_v2_npa_ris.shp", layer= "presencia_v2_npa_ris")
# geotopo_simdiff <- readOGR("Y:/04-suitability/presencia/presencia_ausencia/presencia/no_presencia_v2_npa_ris.shp", layer ="no_presencia_v2_npa_ris")

if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}

rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
rcpLsNames <- c("RCP 2.6", "RCP 4.5", "RCP 6.0", "RCP 8.5")
periodLs <- c("20", "30")

stk <- stack(c(paste0(iDir, "/suit_mean_futuro_", periodLs[1], "_", rcpLs, ".tif"), 
                    paste0(iDir, "/suit_mean_futuro_", periodLs[1], "_", rcpLs, ".tif")))
stk_crop <- mask(crop(stk, extent(mask)), mask)

plot <- setZ(stk_crop, c(rcpLsNames, rep(" ", 4)) )
names(plot) <- toupper(c(rcpLsNames, rep(" ", 4)))

zvalues <- seq(0, 100, 10) # Define limits
myTheme <- BuRdTheme() # Define squeme of colors
myTheme$regions$col = brewer.pal(10, "RdYlGn")
# myTheme$strip.border$col = "white" # Eliminate frame from maps
myTheme$axis.line$col = 'gray' # Eliminate frame from maps


tiff(paste(oDir, "/suitability_future_v2.tif", sep=""), width=3400, height=2000, pointsize=8, compression='lzw',res=300)

print(levelplot(plot, at = zvalues, 
                layout=c(4, 2), 
                xlab="", 
                ylab="2030s                                     2020s", 
                par.settings = myTheme, 
                names.attr=c(rcpLsNames, rep(" ", 4)), 
                colorkey = list(space = "bottom")
                # xlim=c(-76.22, -75.4), 
                # ylim=c(4.6, 5.6)
)
+ layer(sp.polygons(geotopo, col= "black"))
)


dev.off()



############################
## Future changes classes ##
############################

iDir <- "Y:/04-suitability/results/ejecucion_mayo/current/ensemble"
fDir <- "Y:/04-suitability/results/ejecucion_mayo/ensemble_modeling"
oDir <- "Y:/04-suitability/results/ejecucion_mayo/performance_suitability"
geotopo <- readOGR("Y:/04-suitability/presencia/presencia_ausencia/presencia/presencia_v2_npa_ris.shp", layer= "presencia_v2_npa_ris")
pa <- readOGR("Y:/04-suitability/presencia/presencia_ausencia/presencia/areas_protegidas_ris.shp", layer= "areas_protegidas_ris")

# Load current suitability
rsC <- raster(paste0(iDir, "/suit_mean_current.tif")) / 10

# Load future suitability
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
rcpLsNames <- c("RCP 2.6", "RCP 4.5", "RCP 6.0", "RCP 8.5")
periodLs <- c("20", "30")

stk <- stack(c(paste0(fDir, "/suit_mean_futuro_", periodLs[1], "_", rcpLs, ".tif"), 
               paste0(fDir, "/suit_mean_futuro_", periodLs[1], "_", rcpLs, ".tif")))

# Load threshold 
sDir <- "Y:/04-suitability/results/ejecucion_mayo"
eval <- lapply(list.files(paste0(sDir,"/evaluacion de modelos"),pattern=glob2rx(("*eval*.csv*")),full.names=T),read.csv)
eval <- ldply(eval)
bin <- eval[eval$eval%in%"ROC"& eval$metrics%in%"Cutoff" & eval$run%in%"Full",]
thr <- min(bin$value) / 10

# Categories

# 1. Not suitable for coffee (neither current nor future)
# 2. Big challenges to continue growing coffee (current apt, future not suitable)
# 3. Minor challenges for coffee growing (current apt, future apt, but reduced in suitability)
# 4. Opportunities to increase production (current apt, future fit, but increase suitability)
# 5. Potential for new areas (current unfit, future apt)

chgStk <- stack()
for(i in 1:nlayers(stk)){
  
  rsF <- stk[[i]]
  
  #Analysis
  outCon1 = ((rsC < thr) & (rsF <  thr))
  outCon1[!outCon1]=NA
  outCon1[!is.na(outCon1)]=1
  
  outCon2 = ((rsC >= thr) & (rsF <  thr))
  outCon2[!outCon2]=NA
  outCon2[!is.na(outCon2)]=2
  
  # #Areas suitable and same suitability in the rsF (YELLOW)
  outCon3 = ((rsC >= thr) & (rsF >= thr)) & ((rsF - rsC) < 0)
  outCon3[!outCon3]=NA
  outCon3[!is.na(outCon3)]=3

  outCon4 = ((rsC >= thr) & (rsF >= thr)) & ((rsF - rsC) > 0)
  outCon4[!outCon4]=NA
  outCon4[!is.na(outCon4)]=4
  
  outCon5 = ((rsC < thr) & (rsF >= thr))
  outCon5[!outCon5]=NA
  outCon5[!is.na(outCon5)]=5
  
  rsChg <- merge(outCon1,outCon2,outCon3,outCon4,outCon5)
  
  chgStk <- stack(chgStk, rsChg)
  
}


chgStk <- crop(chgStk, extent(-76.22, -75.41, 4.67, 5.47) )




               
# Plot changes 
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
rcpLsNames <- c("RCP 2.6", "RCP 4.5", "RCP 6.0", "RCP 8.5")
periodLs <- c("20", "30")

plot <- setZ(chgStk, c(rcpLsNames, rep(" ", 4)) )
names(plot) <- toupper(c(rcpLsNames, rep(" ", 4)))

zvalues <- c(0, 1, 2, 3, 4, 5) # Define limits
myTheme <- BuRdTheme() # Define squeme of colors

myTheme$regions$col=colorRampPalette(c("lightsalmon", "wheat1", "yellowgreen", "forestgreen", "mediumblue"))
# myTheme$strip.border$col = "white" # Eliminate frame from maps
myTheme$axis.line$col = 'gray' # Eliminate frame from maps


tiff(paste(oDir, "/suitability_changes_classes.tif", sep=""), width=3400, height=2200, pointsize=8, compression='lzw',res=300)

print(levelplot(plot, at = zvalues, 
                layout=c(4, 2), 
                xlab="", 
                ylab="2030s                                                 2020s", 
                par.settings = myTheme, 
                names.attr=c(rcpLsNames, rep(" ", 4)), 
                colorkey = FALSE,
                key = list(rectangles=list(col = c("lightsalmon", "wheat1", "yellowgreen", "forestgreen", "mediumblue", "gray95")), 
                           text=list(c("No apto para café ", 
                                       "Retos grandes para continuar cultivando café", 
                                       "Retos menores para la caficultura", 
                                       "Oportunidades para incrementar producción", 
                                       "Potencial para nuevas áreas", 
                                       "Áreas protegidas"
                                       )),
                           cex=1.3,
                           space='bottom'
                           # height=1.4, width=1.4
                           )
                )
      + layer(sp.polygons(pa, col= "transparent", fill = "gray95"))
      + layer(sp.polygons(geotopo, col= "black", lwd=0.7))
      )


dev.off()



# Percent Changes 
chgStk_p <- stk - rsC

plot <- setZ(chgStk_p, c(rcpLsNames, rep(" ", 4)) )
names(plot) <- toupper(c(rcpLsNames, rep(" ", 4)))

zvalues <- seq(-100, 100, 10) # Define limits
myTheme <- BuRdTheme() # Define squeme of colors
myTheme$regions$col=colorRampPalette(c("darkred", "red", "orange", "snow", "yellowgreen", "forestgreen", "darkgreen"))(length(zvalues)-1)
# myTheme$strip.border$col = "white" # Eliminate frame from maps
myTheme$axis.line$col = 'gray' # Eliminate frame from maps


tiff(paste(oDir, "/suitability_precent_changes_v2.tif", sep=""), width=3400, height=2000, pointsize=8, compression='lzw',res=300)

print(levelplot(plot, at = zvalues, 
                layout=c(4, 2), 
                xlab="", 
                ylab="2030s                                     2020s", 
                par.settings = myTheme, 
                names.attr=c(rcpLsNames, rep(" ", 4)), 
                colorkey = list(space = "bottom")
                # xlim=c(-76.22, -75.4), 
                # ylim=c(4.6, 5.6)
)
+ layer(sp.polygons(pa, col= "transparent", fill = "gray95"))
+ layer(sp.polygons(geotopo, col= "black", lwd=0.7))
)


dev.off()




#######################################
### Comparisson current and future suit 
#######################################


iDir <- "Y:/04-suitability/results/ejecucion_mayo/current/ensemble"
fDir <- "Y:/04-suitability/results/ejecucion_mayo/ensemble_modeling"
oDir <- "Y:/04-suitability/results/ejecucion_mayo/performance_suitability"
mask <- readOGR("Y:/02-monthly-interpolations/region/mask/ris_adm0_wgs84.shp", layer= "ris_adm0_wgs84")
geotopo <- readOGR("Y:/04-suitability/presencia/presencia_ausencia/presencia/presencia_v2_npa_ris.shp", layer= "presencia_v2_npa_ris")
pa <- readOGR("Y:/04-suitability/presencia/presencia_ausencia/presencia/areas_protegidas_ris.shp", layer= "areas_protegidas_ris")

if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}



periodLs <- c(20, 30)
rcp <- "rcp85"
varLsNames <- c("Actual", "Futuro-2020-RCP8.5", "Futuro-2030-RCP8.5")

rsCur <- raster(paste0(iDir, "/suit_mean_current.tif")) / 10
rsFut <- stack(paste0(fDir, "/suit_mean_futuro_", periodLs, "_", rcp, ".tif"))
stk_crop <- stack(rsCur, rsFut)
stk_crop <- crop(stk_crop, extent(-76.22, -75.41, 4.67, 5.47) )

plot <- setZ(stk_crop, varLsNames)
names(plot) <- toupper(varLsNames)

zvalues <- seq(0, 100, 10) # Define limits
myTheme <- BuRdTheme() # Define squeme of colors
myTheme$regions$col = brewer.pal(10, "RdYlGn")
# myTheme$strip.border$col = "white" # Eliminate frame from maps
myTheme$axis.line$col = 'gray' # Eliminate frame from maps


tiff(paste(oDir, "/suitability_comparisson_", rcp ,".tif", sep=""), width=2600, height=1200, pointsize=8, compression='lzw',res=300)

print(levelplot(plot, at = zvalues, 
                layout=c(3, 1), 
                xlab="", 
                ylab="", 
                par.settings = myTheme, 
                colorkey = list(space = "bottom")
                # xlim=c(-76.22, -75.36), 
                # ylim=c(4.6, 5.6)
)
+ layer(sp.polygons(pa, col= "transparent", fill = "gray95"))
+ layer(sp.polygons(geotopo, col= "black", lwd=0.7))
)


dev.off()


