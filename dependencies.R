
library(raster)
library(rnaturalearth)
library(spatstat)
library(ggplot2)
library(gridExtra)
library(rgdal)
library(rgeos)



getMapExtent <- function() {
  
  worldMap <- ne_countries(scale = 110, returnclass = "sp")
  plot(worldMap,col="Gray",border="Gray")
  poly <- clickpoly(add=TRUE)
  p = Polygon(cbind(poly$bdry[[1]]$x,poly$bdry[[1]]$y))
  ps = Polygons(list(p),1)
  sps = SpatialPolygons(list(ps))
  plot(sps,add=TRUE,col="#727272")
  crs(sps) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  sps <- as(sps, "SpatialPolygons")
  
  return( extent(sps) )
  
}

gplot_data <- function(x, maxpixels = 50000)  {
  x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)
  coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
  ## Extract values
  dat <- utils::stack(as.data.frame(raster::getValues(x))) 
  names(dat) <- c('value', 'variable')
  
  dat <- dplyr::as.tbl(data.frame(coords, dat))
  
  if (!is.null(levels(x))) {
    dat <- dplyr::left_join(dat, levels(x)[[1]], 
                            by = c("value" = "ID"))
  }
  dat
}

addLabelsPlot <- function() {
  
  if( "graticulate" %in% elementsToHide ) { gradLon <<- NULL ; gradLat <<- NULL } else { gradLon <<- "Longitude" ; gradLat <<- "Latitude" }
  
  labelsPlot <- labs(x = gradLon, y = gradLat)
  
  if( fillLabel != "" ) { labelsPlot$fill <- fillLabel }
  if( titleMap != "" ) { labelsPlot$title <- titleMap }
  if( subtitleMap != "" ) { labelsPlot$subtitle <- subtitleMap }
  if( captionMap != "" ) { labelsPlot$caption <- captionMap }
  
  labelsPlot <<- labelsPlot }

  
theme_map <- function() {
  
  basicTheme <- theme(text = element_text(family = "Helvetica", color = "#22211d"), axis.line = element_blank() )
  
  if( "graticulate" %in% elementsToHide ) { hideAxisText <- theme( axis.text = element_blank(),
                                                                   axis.ticks = element_blank(),
                                                                   axis.title = element_blank() ) } else { hideAxisText <- basicTheme }
  
  if( "graticulate" %in% elementsToHide ) { gradLon <<- NULL ; gradLat <<- NULL } else { gradLon <<- "Longitude" ; gradLat <<- "Latitude" }
  
  if( "legend" %in% elementsToHide ) { hideLegend <- theme(legend.position = "none") } else { hideLegend <- basicTheme }
  
  theme_minimal() +
  basicTheme +
  hideAxisText +
  hideLegend +

  theme(          
    
          panel.grid.major = element_line(color = "#48474b", size = 0.05),
          panel.grid.minor = element_line(color = "#48474b", size = 0.05),
          
          # panel.grid = element_blank(),
          # panel.border = element_blank() ,
              
          #panel.grid.minor = element_blank(),
          
          plot.background = element_rect(fill = panelColor, color = NA), 

          panel.background = element_rect(fill = oceanColor, color = oceanColor), 
          
          legend.background = element_rect(fill = "#f5f5f2", color = NA),
      
          plot.caption = element_text(hjust = 0),
          panel.spacing = unit(c(0, 0, 0, 0), "cm"),       
          plot.margin=unit(c(0,0,0,0),"cm") ) 
  
}


