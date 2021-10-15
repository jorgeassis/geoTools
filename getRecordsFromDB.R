## --------------------------------------------------
## --------------------------------------------------
##
## biodiversityDS.
## https://github.com/jorgeassis
##
## --------------------------------------------------
## --------------------------------------------------


records.1 <- getExternalDataINaturalist(sp)
records.2 <- getExternalDataObis(sp,getCitation=FALSE)
records.3 <- getExternalDataGbif(sp,getCitation=FALSE)

## --------------------------------------------------
## --------------------------------------------------

getExternalDataINaturalist <- function(taxa) {
  
  df <- NULL
  taxa <- stri_trans_general(str = taxa, id = "Latin-ASCII")
  
  if( missing(taxa)) { errormessage("no taxa (Worms name) introduced.") }
  
  if( exists("my_occs_inatu") ) { rm(my_occs_inatu) }
  
  tryCatch( my_occs_inatu <- get_inat_obs(query=taxa) , error=function(e) { error <- TRUE })
  
  if( exists("my_occs_inatu") ) { if( nrow(my_occs_inatu) == 0 ) { my_occs_inatu <- data.frame() } }
  
  if( ! exists("my_occs_inatu") ) { my_occs_inatu <- data.frame() }
  
  if( nrow(my_occs_inatu) > 0) {
    
    my_occs_inatu <- subset(my_occs_inatu, my_occs_inatu$longitude !=0 & my_occs_inatu$latitude !=0)
    my_occs_inatu <- my_occs_inatu[my_occs_inatu$scientific_name == taxa,]
    
  }
  
  if( nrow(my_occs_inatu) > 0) {
    
    my_occs_inatu <- data.frame(my_occs_inatu,year=substr(my_occs_inatu$observed_on, 1, 4),month=substr(my_occs_inatu$observed_on, 6, 7),day=substr(my_occs_inatu$observed_on, 9, 10))
    
    df <- my_occs_inatu
    
  }
  
  return(df)
  
}

# ---------------------------------------------------------------------------------------------------------------

getExternalDataObis <- function(taxa,getCitation) {
  
  df <- NULL
  taxa <- stri_trans_general(str = taxa, id = "Latin-ASCII")
  
  if( missing(taxa)) { errormessage("no taxa (Worms name) introduced.") }
  
  if( exists("my_occs_obis") ) { rm(my_occs_obis) }
  
  tryCatch( my_occs_obis <- occurrence(scientificname = taxa , areaid = "31912" ) , error=function(e) { error <- TRUE })
  
  if( exists("my_occs_obis") ) { if( nrow(my_occs_obis) == 0 ) { my_occs_obis <- data.frame() } }
  
  if( ! exists("my_occs_obis") ) { my_occs_obis <- data.frame() }
  
  if( nrow(my_occs_obis) > 0) {
    
    my_occs_obis <- subset(my_occs_obis, my_occs_obis$decimalLongitude !=0 & my_occs_obis$decimalLatitude !=0)
    
  }
  
  if( ! getCitation ) { df <- data.frame(my_occs_obis) }
  
  if( getCitation ) {
    
    if( nrow(my_occs_obis) > 0) {
      
      my_occs_obisInfo <- my_occs_obis$dataset_id
      my_occs_obisInfo <- unique(my_occs_obis$dataset_id)
      
      for(z in 1:length(my_occs_obisInfo) ) {
        
        error <- TRUE
        errortrials <- 0
        
        while(error & errortrials < 10) {
          error <- FALSE
          errortrials <- errortrials + 1
          tryCatch(  z.Res <- RJSONIO::fromJSON(paste0("https://api.obis.org/v3/dataset/",my_occs_obisInfo[z])) , error=function(e) { error <- TRUE })
        }
        
        if(!error) {  
          
          institutionCode <- my_occs_obis[my_occs_obis$dataset_id == my_occs_obisInfo[z],"institutionCode"]
          collectionCode <- my_occs_obis[my_occs_obis$dataset_id == my_occs_obisInfo[z],"collectionCode"]
          
          my_occs_obis[my_occs_obis$dataset_id == my_occs_obisInfo[z],"accessRights"] <- z.Res$results[[1]]$intellectualrights
          
          z.Res <- paste0( z.Res$results[[1]]$citation,
                           " ",
                           ifelse(!is.na(institutionCode) | !is.null(institutionCode) , institutionCode , ""),
                           " ",
                           ifelse(!is.na(collectionCode) | !is.null(collectionCode) , collectionCode , ""),
                           " (Available: Ocean Biogeographic Information System. Intergovernmental Oceanographic Commission of UNESCO. www.iobis.org. Accessed: ", Sys.time())
          
          my_occs_obis[my_occs_obis$dataset_id == my_occs_obisInfo[z],"bibliographicCitation"] <- z.Res
          
        }
        
      }
      
      df <- my_occs_obis
      
    }
    
  }
  
  return(df)
  
}

# ---------------------------------------------------------------------------------------------------------------

getExternalDataGbif <- function(taxa, getCitation) {
  
  if( exists("my_occs_gbif") ) { rm(my_occs_gbif) }
  
  taxa <- stri_trans_general(str = taxa, id = "Latin-ASCII")
  nRecords <- gbif(strsplit(as.character(taxa), " ")[[1]][1], strsplit(as.character(taxa), " ")[[1]][2], ext=shapeExtent, geo=T, removeZeros=T , download=FALSE, ntries=999)
  
  if( nRecords < 200000 ) { my_occs_gbif <- gbif(strsplit(as.character(taxa), " ")[[1]][1], strsplit(as.character(taxa), " ")[[1]][2], ext=shapeExtent, geo=T, removeZeros=T , download=TRUE, ntries=999) }
  
  if( nRecords >= 200000 ) { 
    
    seqListing <- seq(0,nRecords,by =300)
    if(max(seqListing) < nRecords) { seqListing <- c(seqListing,nRecords) }
    parallelChunks <- data.frame(from = seqListing[-length(seqListing)], to = c(seqListing[-c(1,length(seqListing))] -1 , nRecords ) )
    parallelChunks <- parallelChunks[parallelChunks$from <= 99700,]
    
    tmpfile <- paste(tempfile(), ".json", sep = "")
    my_occs_gbif <- data.frame()
    
    for( ch in 1:nrow(parallelChunks)) {
      
      error <- TRUE
      chunck <- NULL
      
      while(error) {
        tryCatch( chunck <- download.file(paste0("http://api.gbif.org/v1/occurrence/search?scientificname=",strsplit(as.character(taxa), " ")[[1]][1],"+",strsplit(as.character(taxa), " ")[[1]][2],"&offset=",parallelChunks[ch,1],"&limit=300"), tmpfile, quiet = TRUE) , error=function(e) { error <- TRUE })
        if(!is.null("chunck")) { error <- FALSE }
      }
      
      json <- scan(tmpfile, what = "character", quiet = TRUE, sep = "\n", encoding = "UTF-8")
      json <- chartr("\a\v", "  ", json)
      x <- jsonlite::fromJSON(json)
      r <- x$results
      r <- r[, !sapply(r, class) %in% c("data.frame", "list")]
      rownames(r) <- NULL
      my_occs_gbif <- rbind.fill(my_occs_gbif,r)
      
    }
  }
  
  if( ! is.null(my_occs_gbif) & getCitation ) {
    
    my_occs_gbif_all <- unique(my_occs_gbif$datasetKey)
    
    for(z in 1:length(my_occs_gbif_all) ) {
      
      z.Res <- gbif_citation(x=my_occs_gbif_all[z])
      
      my_occs_gbif[my_occs_gbif$datasetKey == my_occs_gbif_all[z] ,"accessRights"] <- ifelse(!is.null(z.Res$rights),z.Res$rights,"")
      
      z.Res <- z.Res$citation$citation
      
      my_occs_gbif[my_occs_gbif$datasetKey == my_occs_gbif_all[z],"bibliographicCitation"] <- z.Res
      
    }
    
  }
  
  return(my_occs_gbif)
  
}

# ---------------------------------------------------------------------------------------------------------------

