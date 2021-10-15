
theme_map <- 
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica", color = "#22211d"),
    #axis.line = element_blank(),
    #axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    #axis.title.x = element_blank(),
    #axis.title.y = element_blank(),
    legend.position = c(1,1),
    legend.justification=c(1, 1),
    # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.major = element_line(color = "black", size = 0.1),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#F3F3F3", color = NA), 
    panel.background = element_rect(fill = "#F3F3F3", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.border = element_blank(),
    axis.text=element_text(size=14),
    axis.title=element_text(size=14),
    plot.title = element_text(size = 12),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)) ,
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))
  )

library(ggplot2)
library(ggridges)

rasters <- list.files(mainDirectory,full.names = TRUE,pattern="tif")
rasters

file <- 97 # 30 32,34,36 9
rasterMap1 <- raster(rasters[file])
names(rasterMap1)

file <- 115 #  32,34,36 9
rasterMap2 <- raster(rasters[file])
names(rasterMap2)

file <- 90 #  32,34,36 9
rasterMap3 <- raster(rasters[file])
names(rasterMap3)

# ----------------------------
# ----------------------------
# Ecoregions

regions <- shapefile("../Data/marine_ecoregions.shp")
regionsID <- unique(regions$REALM)
resultsRegion <- data.frame(Realm=regionsID,region=1:length(regionsID),KelpArea=NA,KelpBio=NA,KelpBioM=NA,SeagrassArea=NA,SeagrassBio=NA,SeagrassBioM=NA,FucoidArea=NA,FucoidBio=NA,FucoidBioM=NA)

for( i in 1:length(regionsID)) {
  
  region.i <- regions[regions$REALM == regionsID[i],]
  
  r1 <- mask(rasterMap1,region.i) 
  r2 <- mask(rasterMap2,region.i) 
  r3 <- mask(rasterMap3,region.i) 
  
  resultsRegion[i,"KelpBio"] <- cellStats(r1,mean)
  resultsRegion[i,"KelpBioM"] <- cellStats(r1,max)
  resultsRegion[i,"SeagrassBio"] <- cellStats(r2,mean)
  resultsRegion[i,"SeagrassBioM"] <- cellStats(r2,max)
  resultsRegion[i,"FucoidBio"] <- cellStats(r3,mean)
  resultsRegion[i,"FucoidBioM"] <- cellStats(r3,max)
  
  r1[r1 >= 1] <- 1
  r2[r2 >= 1] <- 1
  r3[r3 >= 1] <- 1
  
  resultsRegion[i,"KelpArea"] <- sum(getValues(raster::area(r1) * r1),na.rm=T)
  resultsRegion[i,"SeagrassArea"] <- sum(getValues(raster::area(r2) * r1),na.rm=T)
  resultsRegion[i,"FucoidArea"] <- sum(getValues(raster::area(r3) * r1),na.rm=T)
    
}


write.csv(resultsRegion,file=paste0("../../Global biodiversity patterns of Marine Forest Species/Results/resultEcoregions.csv"))

# --------------------------------------

dataFolder <- "/Volumes/Jellyfish/GDrive/Manuscripts/Modelling the global distribution of marine forests/Data/"
resultsFolder <- "/Volumes/Jellyfish/GDrive/Manuscripts/Modelling the global distribution of marine forests/Results/"

mainDirectory <- "../Results/"
resultsAccuracyAll <- read.csv(paste0(mainDirectory,"/_ Summary/resultsSummaryAllSp.csv"))

load(paste0(dataFolder,"/datasetMF.RData"))
dataset <- data.frame(dataset,stringsAsFactors = FALSE)
load(paste0(dataFolder,"/addOnRecords.RData"))
newDataFinal <- data.frame(acceptedName=newDataFinal$species,
                           decimalLongitude=as.numeric(as.character(newDataFinal$decimalLongitude)),
                           decimalLatitude=as.numeric(as.character(newDataFinal$decimalLatitude)),
                           stringsAsFactors = FALSE)

dataset <- rbind.fill(dataset,newDataFinal)
dataset$decimalLongitude <- as.numeric(as.character(dataset$decimalLongitude))
dataset$decimalLatitude <- as.numeric(as.character(dataset$decimalLatitude))
dataset$genus <- as.character(dataset$genus)

load(paste0(dataFolder,"/taxonRealmMF.RData"))
taxonRealm <- data.frame(taxonRealm,stringsAsFactors = FALSE)

subseter <- taxonRealm$nRecords >= 5 & 
  taxonRealm$acceptedName %in% resultsAccuracyAll[which(resultsAccuracyAll$auc > 0.75),"acceptedName"]

taxonRealm <- taxonRealm[which(subseter), ]
dataset <- dataset[dataset$acceptedName %in% taxonRealm$acceptedName, ]

unique(dataset$genus)

# -------

regions <- shapefile("../Data/marine_ecoregions.shp")
regionsID <- unique(regions$REALM)

# Based on family

resultsRegionFamily <- data.frame(matrix(0,ncol=length(regionsID),nrow =length(unique(dataset$family)) ))

colnames(resultsRegionFamily) <- regionsID
rownames(resultsRegionFamily) <- unique(dataset$family)

datasetPts <- dataset
coordinates(datasetPts) <- ~decimalLongitude+decimalLatitude
crs(datasetPts) <- crs(regions)

for( i in 1:length(regionsID)) {
  
  region.i <- regions[regions$REALM == regionsID[i],]
  recordsOver <- over(datasetPts,region.i)
  recordsOver <- dataset[which(!is.na(recordsOver[,1])),]
  
  recordsOver <- aggregate(recordsOver, list(recordsOver$family,recordsOver$acceptedName), length)
  
  for(g in 1:length(unique(recordsOver$Group.1))) {
    
    family <- unique(recordsOver$Group.1)[g]
    resultsRegionFamily[ which(rownames(resultsRegionFamily) == family) , i] <- sum(recordsOver$Group.1 == family)
    
  }
}

resultsRegionFamily
write.csv(resultsRegionFamily,file=paste0("../../Global biodiversity patterns of Marine Forest Species/Results/resultEcoregionsFamily.csv"))

# Based on Genus

resultsRegionGenus <- data.frame(matrix(0,ncol=length(regionsID),nrow =length(unique(dataset$genus)) ))

colnames(resultsRegionGenus) <- regionsID
rownames(resultsRegionGenus) <- unique(dataset$genus)

datasetPts <- dataset
coordinates(datasetPts) <- ~decimalLongitude+decimalLatitude
crs(datasetPts) <- crs(regions)

for( i in 1:length(regionsID)) {
  
  region.i <- regions[regions$REALM == regionsID[i],]
  recordsOver <- over(datasetPts,region.i)
  recordsOver <- dataset[which(!is.na(recordsOver[,1])),]
  
  recordsOver <- aggregate(recordsOver, list(recordsOver$genus,recordsOver$acceptedName), length)
  
  for(g in 1:length(unique(recordsOver$Group.1))) {
    
    genus <- unique(recordsOver$Group.1)[g]
    resultsRegionGenus[ which(rownames(resultsRegionGenus) == genus) , i] <- sum(recordsOver$Group.1 == genus)
    
  }
}

write.csv(resultsRegionGenus,file=paste0("../../Global biodiversity patterns of Marine Forest Species/Results/resultEcoregionsGenus.csv"))

# Based on species

resultsRegionSpecies <- data.frame(matrix(0,ncol=length(regionsID),nrow =length(unique(dataset$acceptedName)) ))

colnames(resultsRegionSpecies) <- regionsID
rownames(resultsRegionSpecies) <- unique(dataset$acceptedName)

datasetPts <- dataset
coordinates(datasetPts) <- ~decimalLongitude+decimalLatitude
crs(datasetPts) <- crs(regions)

for( i in 1:length(regionsID)) {
  
  region.i <- regions[regions$REALM == regionsID[i],]
  recordsOver <- over(datasetPts,region.i)
  recordsOver <- dataset[which(!is.na(recordsOver[,1])),]
  resultsRegionSpecies[ which(rownames(resultsRegionSpecies) %in% as.character(unique(recordsOver$acceptedName))) , i] <- 1
  
}

write.csv(resultsRegionSpecies,file=paste0("../../Global biodiversity patterns of Marine Forest Species/Results/resultEcoregionsSpecies.csv"))

# -------

resultsRegionGenus
resultsRegionSpecies

# resultsRegionGenus <- read.csv(paste0("../../Global biodiversity patterns of Marine Forest Species/Results/resultEcoregionsGenus.csv"),row.names=1)
# resultsRegionSpecies <- read.csv(paste0("../../Global biodiversity patterns of Marine Forest Species/Results/resultEcoregionsSpecies.csv"),row.names=1)

distanceMatrix <- resultsRegionSpecies # resultsRegionSpecies resultsRegionGenus

distanceMatrix <- distanceMatrix[rownames(distanceMatrix) %in% as.character(unique(taxonRealm[taxonRealm$phylum != "Ochrophyta","acceptedName"])) ,]
distanceMatrix <- distanceMatrix[rownames(distanceMatrix) %in% as.character(unique(taxonRealm[taxonRealm$phylum != "Ochrophyta","genus"])) ,]

distanceMatrix <- distanceMatrix[,which(apply(distanceMatrix,2,sum) > 0)]

library(vegan)

distanceRegions <- vegdist(t(sqrt(distanceMatrix)), method="bray", binary=FALSE, diag=FALSE, upper=FALSE, na.rm = TRUE)
#distanceRegions <- dist(t(resultsRegionGenus), diag=FALSE, upper=FALSE)

plot(hclust(distanceRegions))

library("ggdendro")

mainTheme <- theme(panel.grid.major = element_blank() ,
                   text = element_text(size=12) ,
                   axis.title.y = element_text(margin = margin(t = 0, r = 12, b = 0, l = 0)) ,
                   axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)) ,
                   axis.text.x =element_text(size=9, margin = margin(t = 6, r = 0, b = 0, l = 0)),
                   axis.text.y =element_text(size=9, margin = margin(t = 0, r = 6, b = 0, l = 0)))

f1 <- ggdendrogram(hclust(distanceRegions), rotate = TRUE, theme_dendro = FALSE) + mainTheme +
  xlab("Marine realms") + ylab("Relative distance")
f1

pdf(file=paste0("../../Global biodiversity patterns of Marine Forest Species/Paper/Figures/","/Fig 5 Species Seagrass.pdf"),width=10,useDingbats=FALSE)
f1
dev.off()
