## --------------------------------------------------
## --------------------------------------------------
##
## biodiversityDS.
## https://github.com/jorgeassis
##
## --------------------------------------------------
## --------------------------------------------------

rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)
library(raster)


# read layers and stack in a unique raster object
maxSST <- raster("Data/rasterLayers/Present Temperature LtMax.tif")
minSST <- raster("Data/rasterLayers/Present Temperature LtMin.tif")
thermalConditions <- stack(minSST,maxSST)

# generate a reclassification table
temperateConditions <- data.frame(from = c(-3,10,18) , to=c(10,18,40) , reclassValue=c(0,1,0))
temperateConditions

# reclassify raster
temperateRegions <- reclassify(meanThermalConditions, rcl = temperateConditions)