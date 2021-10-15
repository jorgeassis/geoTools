## -------------------------------------------------------------------------------
## -------------------------------------------------------------------------------
##
##                                      #####
##                                ####  #####
##                                ####       
##          ####                         
##         ##################             
##           ##################           
##       #######################
##   ##################################   
##  #######################################
##  ######################################
##  ###################################### 
##  ####################################
##  ##################################     
##  ####################                   
##  ###################                    
##  ##################                     
##  #################                      
##  ###############                                     
##      
##  theMarineDataScientist
##
##  github.com/jorgeassis
##  medium.com/themarinedatascientist
##  medium.com/@jorgemfa
##
## -------------------------------------------------------------------------------
##
##  R Pipelines to produce a landmass shapefile of the Last Glacial Maximum
##
## -------------------------------------------------------------------------------
## -------------------------------------------------------------------------------

library(raster)
library(rgdal)
library(rgeos)
bathymetryFile <- "https://github.com/jorgeassis/shapefileLGM/blob/master/data/BathymetryDepthMean.tif?raw=true"
bathymetry <- raster(bathymetryFile)
rclmat <- matrix(c(-Inf,-121,NA,-120,0,1,NA,NA,1), ncol=3, byrow=TRUE)
bathymetry <- reclassify(bathymetry, rclmat)
plot(bathymetry)

bathymetryPoly <- rasterToPolygons(bathymetry, fun=NULL, n=4, na.rm=TRUE, digits=6, dissolve=TRUE)
plot(bathymetryPoly)
writeOGR(bathymetryPoly, ".", "landmassLGMPolygon", driver="ESRI Shapefile")

ggplot() +
  geom_polygon(data = bathymetryPoly, aes(x = long, y = lat, group = group), fill="#848484", colour = NA) +
 geom_path(data = bathymetryPoly, aes(x = long, y = lat, group = group), color = "#848484", size = 0.1) +
  coord_equal() + 
  theme_minimal() +
  theme( axis.text = element_blank(),axis.ticks = element_blank(),axis.title = element_blank() )
