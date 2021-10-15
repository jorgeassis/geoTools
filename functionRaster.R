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

# apply function to raster
above30SST <- calc(maxSST, fun=function(x){ x[x < 30] <- 0; x[x >= 30] <- 1; return(x) } )

# apply function to stack of rasters
rangeSSTEurope <- calc(thermalConditionsEurope, fun=function(x){ return( x[[2]] - x[[1]] ) } )

# lower raster resolution
maxSSTEuropeLR <- aggregate(maxSSTEurope, fact=20, fun=mean)
plot(maxSSTEuropeLR)

# increase raster resolution
maxSSTEuropeHR <- disaggregate(maxSSTEurope, fact=20)
plot(maxSSTEuropeHR)

