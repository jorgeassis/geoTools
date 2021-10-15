

# Spherical Geodesic Grids
# https://github.com/r-barnes/dggridR


library(dggridR)
library(dplyr)
library(rnaturalearth)
library(maptools)
library(rgeos)
library(raster)

world <- ne_countries(scale = 'small')

# Construct a discrete global grid (geodesic) with cells of km^2
dggs <- dgconstruct(area=250000, metric=FALSE, resround='nearest')
dgmaxcell(dggs)
dggs <- dgearthgrid(dggs,frame=FALSE, wrapcells = TRUE)

mean( area(dggs) / 10000000 ) # km2

ggplot() + 
  geom_polygon(data=dggs, aes(x=long, y=lat, group=group), alpha=0.4) +
  geom_path(data=dggs, aes(x=long, y=lat, group=group), alpha=0.4, color="white") +
  coord_map("ortho", orientation = c(151, 150, 90))

# Construct a discrete global grid (geodesic) with cells of km wide
dggs <- dgconstruct(spacing=500, metric=TRUE, resround='nearest', projection = "ISEA") # ISEA 
dgmaxcell(dggs)
dgverify(dggs)

dggs <- dgearthgrid(dggs,frame=FALSE, wrapcells = FALSE)
dggs$id <- 1:length(dggs)
mean( area(dggs) / 10000000 ) # km2

ggplot() + 
  geom_polygon(data=world, aes(x=long, y=lat, group=group)) +
  geom_polygon(data=dggs, aes(x=long, y=lat, group=group), alpha=0.4) +
  geom_path(data=dggs, aes(x=long, y=lat, group=group), alpha=0.4, color="white") +
  coord_map("ortho", orientation = c(0, -80, 0))

writeOGR(dggs, ".", "geodesicGridL1", driver="ESRI Shapefile",overwrite_layer=TRUE)

Ocean <- shapefile("/Volumes/Jellyfish/Dropbox/Data/Shapefiles/Global Ocean/ne_10m_ocean.shp")
crs(Ocean) <- crs(dggs)

Ocean <- gBuffer(Ocean, byid=TRUE, width=0)
dggs <- gBuffer(dggs, byid=TRUE, width=0)

dggsOcean <- over(dggs, Ocean)
dggsOcean <- dggs[which(!is.na(dggsOcean[,1])),]

ggplot() + 
  geom_polygon(data=world, aes(x=long, y=lat, group=group)) +
  geom_polygon(data=dggsOcean, aes(x=long, y=lat, group=group), alpha=0.8) +
  geom_path(data=dggsOcean, aes(x=long, y=lat, group=group), alpha=0.8, color="white") +
  coord_map("ortho", orientation = c(0, -20, 0)) +
  xlab('') + ylab('') + theme_bw() +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.ticks.y=element_blank()) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_blank(), panel.border = element_blank())

writeOGR(dggsOcean, ".", "geodesicGridOceanL1", driver="ESRI Shapefile") #also you were missing the driver argument

# Correct for date-line crossings

listToRemove <- numeric()
listToAdd <- list()

for(i in 1:length(dggsOcean)) {
  
 if( extent(dggsOcean[i,])[1] < 0 & extent(dggsOcean[i,])[2] > 0 & max(extent(dggsOcean[i,])[1:2])-min(extent(dggsOcean[i,])[1:2]) > 180 ) { 
   
   listToRemove <- c(listToRemove,i)
   
   coords1 <- fortify(dggsOcean[i,])[,1:2]
   coords2 <- fortify(dggsOcean[i,])[,1:2]
   coords1[coords1[,1] < 0 ,1] <- 180
   coords2[coords2[,1] > 0 ,1] <- -180
   coords2 <- rbind(coords2,data.frame(long=-180,lat=max(coords2[,2])))
   coords2 <- rbind(coords2,data.frame(long=-180,lat=min(coords2[,2])))
   coords2 <- coords2[chull(coords2),] 

   coords1 <- spPolygons(as.matrix(coords1))
   coords2 <- spPolygons(as.matrix(coords2))
   
   # plot(dggsOcean[i,])
   # plot(coords1,add=TRUE,col="red")
   # plot(coords2,add=TRUE,col="green")

   if( length(listToAdd) >  0 ) { listToAdd <- union(listToAdd, unionSpatialPolygons(union(coords1, coords2), c(1,1))    )  }
   if( length(listToAdd) == 0 ) { listToAdd <- unionSpatialPolygons(union(coords1, coords2), c(1,1))  }
   
 }
  
  if( (extent(dggsOcean[i,])[1] > 0 & extent(dggsOcean[i,])[2] < 0) ) { stop(i) }

}

plot(dggsOcean[-listToRemove,])

dggsOcean <- dggsOcean[-listToRemove,]
dggsOcean <- union(dggsOcean,listToAdd)
dggsOcean$id <- 1:length(dggsOcean)

writeOGR(dggsOcean, ".", "geodesicGridOceanL1Corrected", driver="ESRI Shapefile") #also you were missing the driver argument

