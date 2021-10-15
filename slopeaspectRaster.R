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

bathymetry <- raster("Data/BathymetryDepthMean.tif")

slopeEurope <- terrain(bathymetryEurope, opt = "slope", unit = "degrees")
aspectEurope <- terrain(bathymetryEurope, opt = "aspect", unit = "degrees")