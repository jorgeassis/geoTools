
defineRegion(records,"Lon","Lat")
records <- selectRecords(records,"Lon","Lat")

# ----------------------------------------------
# ----------------------------------------------
# Functions

defineRegion <- function(records,lonName,latName) {
  
  records <- records[which(!is.na(records[,lonName])),c(lonName,latName)] 
  
  ui <- fluidPage(leafletOutput("mymap",height=500))
  
  server <- function(input, output) {
    
    output$mymap <- renderLeaflet(
      leaflet() %>%
        addProviderTiles("Esri.OceanBasemap",group = "Ocean Basemap") %>%
        
        addCircles(lng=records[,lonName], lat=records[,latName] , weight = 3,color="Black",radius = 6) %>%
        
        addDrawToolbar(
          targetGroup='draw')  
      
    )
    
    observeEvent(input$mymap_draw_new_feature,{
      feature <<- input$mymap_draw_new_feature
      
      print(feature)
      
    })
    
  }
  
  return(shinyApp(ui = ui, server = server))
  
}

# -----------

selectRecords <- function(records,lonName,latName) {
  
  records <- records[which(!is.na(records[,lonName])),] 
  
  nPoints <- length(feature$geometry$coordinates[[1]])
  sapply(1:nPoints,function(x) { unlist(feature$geometry$coordinates[[1]][[x]]) })
  poly <- spPolygons(t(sapply(1:nPoints,function(x) { unlist(feature$geometry$coordinates[[1]][[x]]) })))
  
  spobj1 <- SpatialPointsDataFrame(records[,c(lonName,latName)], data=records)
  crs(spobj1) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  crs(poly) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  records <- records[as.numeric(which( ! is.na(over(spobj1,poly) ))),]
  
  return(records)
}

# -----------
# -----------
