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
library(ggnewscale)

dtm <- raster(" ")

dtm[dtm <= 0] <- NA
dtm[dtm > 0] <- 1
plot(dtm)

myLandmass <- rasterToPolygons(dtm, n=8, digits=12, dissolve=TRUE)