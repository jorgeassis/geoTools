## --------------------------------------------------
## --------------------------------------------------
##
## Pretty maps [R]
## Muffins 'n' Code
## https://github.com/jorgeassis
##
## --------------------------------------------------
## --------------------------------------------------

## https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/

rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)

source("mainFunctions.R")

## -------------------

name <- "Ice_LGMMin"
r.1 <- "/Volumes/Jellyfish/Dropbox/Manuscripts/The Modelling Factory/Climate Data/LGM/Sea.ice.thickness.Surface.Var.Lt.Min.tif"

cExtent <- extent(c(-180,180,-90,90))

## -------------------

r.1 <- raster(r.1)
r.1 <- crop(r.1,cExtent)
r.1[r.1 <= 0.005] <- NA
r.1[r.1 >= 0.005] <- 1

r.1.poly <- rasterToPolygons(r.1, fun=NULL, n=8, na.rm=TRUE, digits=16, dissolve=TRUE)
plot(r.1.poly)

writeOGR(obj=r.1.poly, dsn="/Volumes/Jellyfish/Dropbox/Data/Shapefiles/Global Ice", layer=name, driver="ESRI Shapefile" , overwrite_layer=TRUE)

## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## End of code
