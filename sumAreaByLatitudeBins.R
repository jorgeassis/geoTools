
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

# Kelp

file <- 96 # Present 95 113
rasterMap1 <- raster(rasters[file])
names(rasterMap1)

# Seagrass

file <- 114 #  32,34,36 9
rasterMap2 <- raster(rasters[file])
names(rasterMap2)

# Fucoid

file <- 90 #  32,34,36 9
rasterMap3 <- raster(rasters[file])
names(rasterMap3)

# Fucoid Intertidal

file <- 75 # 30 ,34,36 9
rasterMap4 <- raster(rasters[file])
rasterMap4[rasterMap4 == 1] <- NA
names(rasterMap4)

# Fucoid Subtidal

file <- 76 # 30 ,34,36 9
rasterMap5 <- raster(rasters[file])
rasterMap5[rasterMap5 == 1] <- NA
names(rasterMap5)

area <- raster::area(rasterMap1)

# --------------

bins <- seq(-90,90,by=1)

resultBinDiversity <- data.frame(bins,Kelp=NA,Seagrass=NA,Fucoid=NA,FucoidIntertidal=NA,FucoidSubtidal=NA)
resultBinArea <- data.frame(bins,Kelp=NA,Seagrass=NA,Fucoid=NA,FucoidIntertidal=NA,FucoidSubtidal=NA)

# resultBinDiversity <- data.frame(bins,Present=NA,RCP26=NA,RCP45=NA,RCP60=NA,RCP85=NA)
# resultBinArea <- data.frame(bins,Present=NA,RCP26=NA,RCP45=NA,RCP60=NA,RCP85=NA)

for(bin in bins) {
  
  row = rowFromY(rasterMap1, bin)
  cells = cellFromRow(rasterMap1, row)
  
  resultBinDiversity[resultBinDiversity$bins == bin,2] <- mean(rasterMap1[cells]  , na.rm=T)
  resultBinDiversity[resultBinDiversity$bins == bin,3] <- mean(rasterMap2[cells]  , na.rm=T)
  resultBinDiversity[resultBinDiversity$bins == bin,4] <- mean(rasterMap3[cells]  , na.rm=T)
  #resultBinDiversity[resultBinDiversity$bins == bin,5] <- mean(rasterMap4[cells]  , na.rm=T)
  #resultBinDiversity[resultBinDiversity$bins == bin,6] <- mean(rasterMap5[cells]  , na.rm=T)
  
  r1 <- rasterMap1[cells]; r1[!is.na(r1)] <- 1
  r2 <- rasterMap2[cells]; r2[!is.na(r2)] <- 1
  r3 <- rasterMap3[cells]; r3[!is.na(r3)] <- 1
  #r4 <- rasterMap4[cells]; r4[!is.na(r4)] <- 1
  #r5 <- rasterMap5[cells]; r5[!is.na(r5)] <- 1
  
  resultBinArea[resultBinArea$bins == bin,2] <- sum(r1 * area[cells] , na.rm=T)
  resultBinArea[resultBinArea$bins == bin,3] <- sum(r2 * area[cells] , na.rm=T)
  resultBinArea[resultBinArea$bins == bin,4] <- sum(r3 * area[cells] , na.rm=T)
  #resultBinArea[resultBinArea$bins == bin,5] <- sum(r4 * area[cells] , na.rm=T)
  #resultBinArea[resultBinArea$bins == bin,6] <- sum(r5 * area[cells] , na.rm=T)
  
}

# Arctic 

resultBinArea[which(resultBinArea$RCP26 > 11413),3] <- 9785.5089
resultBinArea[,-1] <- resultBinArea[,-1] / 1000

plot1 <- ggplot() +
        geom_bar( data=resultBinArea, aes(x=bins, y=RCP60, fill="#5D1877"), stat="identity",color="black", alpha=1,size=0.1) +
        geom_bar( data=resultBinArea, aes(x=bins, y=RCP26, fill="#E7626A"), stat="identity",color="black", alpha=1,size=0.1) +
        geom_bar( data=resultBinArea, aes(x=bins, y=Present, fill="#F6F1D4"), stat="identity",color="black", alpha=1,size=0.1) +
        ggtitle("Change in habitat suitability area for Arctic kelp forests") + 
        theme_map +
        ylab("Total habitat area (x1000 km2)") + xlab("Latitude") + 
        scale_fill_identity(name = '', guide = guide_legend(),labels = c('RCP60','RCP26','Present')) + theme(legend.position="None")

plot2 <- ggplot() +
        geom_bar( data=resultBinDiversity, aes(x=bins, y=RCP60, fill="#5D1877"), stat="identity",color="black", alpha=1,size=0.1) +
        geom_bar( data=resultBinDiversity, aes(x=bins, y=RCP26, fill="#E7626A"), stat="identity",color="black", alpha=1,size=0.1) +
        geom_bar( data=resultBinDiversity, aes(x=bins, y=Present, fill="#F6F1D4"), stat="identity",color="black", alpha=1,size=0.1) +
        ggtitle("Change in habitat suitability area for Arctic kelp forests") + 
        theme_map +
        ylab("Average number of species with suitable habitats") + xlab("Latitude") +
        scale_fill_identity(name = '', guide = guide_legend(),labels = c('RCP60','RCP26','Present'))


# Global 
# dfc5ab acd6c9 cabedb

resultBinArea[,-1] <- resultBinArea[,-1] / 1000

plot1 <- ggplot() +
  geom_bar( data=resultBinArea, aes(x=bins, y=Fucoid, fill="#cabedb"), stat="identity",color="black", alpha=1,size=0.1) +
  ggtitle("Habitat suitability of marine forests") + 
  theme_map +
  ylab("Total habitat area (x1000 km2)") + xlab("Latitude") + 
  scale_fill_identity(name = '', guide = guide_legend(),labels = c('')) + theme(legend.position="None")

plot2 <- ggplot() +
  geom_bar( data=resultBinDiversity, aes(x=bins, y=Fucoid, fill="#cabedb"), stat="identity",color="black", alpha=1,size=0.1) +
  ggtitle("Diversity of marine forests") + 
  theme_map +
  ylab("Average number of species with suitable habitats") + xlab("Latitude") +
  scale_fill_identity(name = '', guide = guide_legend(),labels = c('')) + theme(legend.position="None")

plotCombined <- grid.arrange(plot1, plot2, nrow = 1)
plotCombined <- cowplot::ggdraw(plotCombined)
plotCombined

pdf(file=paste0("../../Global biodiversity patterns of Marine Forest Species/Figures/FucoidFig2.pdf"),width=20,height=8,useDingbats=FALSE)
plotCombined
dev.off()

write.csv(resultBinArea,file=paste0("../../Global biodiversity patterns of Marine Forest Species/Results/resultBinArea.csv"))
write.csv(resultBinDiversity,file=paste0("../../Global biodiversity patterns of Marine Forest Species/Results/resultBinDiversity.csv"))

# ---------------

resultDepthArea <- data.frame(depth=0:30,Kelp=NA,Seagrass=NA,Fucoid=NA,FucoidIntertidal=NA,FucoidSubtidal=NA)
resultDepthBiodiv <- data.frame(depth=0:30,Kelp=NA,Seagrass=NA,Fucoid=NA,FucoidIntertidal=NA,FucoidSubtidal=NA)
bathymetry <- raster("/Volumes/Jellyfish/GDrive/Manuscripts/Bio-ORACLE Across Climate Changes/Bioclimatic Layers BO3.0/Spatial Data/Rasters/BathymetryDepthMin.tif")
bathymetry <- bathymetry * (-1)

for(depth in 0:29) {
  
  d.range <- c(depth-1,depth)
  
  cells = Which(bathymetry > d.range[1] & bathymetry <= d.range[2] ,cells=TRUE)

  resultDepthBiodiv[resultDepthBiodiv$depth == d.range[2],2] <- mean(rasterMap1[cells]  , na.rm=T)
  resultDepthBiodiv[resultDepthBiodiv$depth == d.range[2],3] <- mean(rasterMap2[cells]  , na.rm=T)
  resultDepthBiodiv[resultDepthBiodiv$depth == d.range[2],4] <- mean(rasterMap3[cells]  , na.rm=T)
  #resultDepth[resultDepth$bins == bin,5] <- mean(rasterMap4[cells]  , na.rm=T)
  #resultDepth[resultDepth$bins == bin,6] <- mean(rasterMap5[cells]  , na.rm=T)
  
  r1 <- rasterMap1[cells]; r1[!is.na(r1)] <- 1
  r2 <- rasterMap2[cells]; r2[!is.na(r2)] <- 1
  r3 <- rasterMap3[cells]; r3[!is.na(r3)] <- 1
  #r4 <- rasterMap4[cells]; r4[!is.na(r4)] <- 1
  #r5 <- rasterMap5[cells]; r5[!is.na(r5)] <- 1
  
  resultDepthArea[resultDepthArea$depth == d.range[2],2] <- sum(r1 * area[cells] , na.rm=T)
  resultDepthArea[resultDepthArea$depth == d.range[2],3] <- sum(r2 * area[cells] , na.rm=T)
  resultDepthArea[resultDepthArea$depth == d.range[2],4] <- sum(r3 * area[cells] , na.rm=T)
  #resultDepth[resultDepth$bins == bin,5] <- sum(r4 * area[cells] , na.rm=T)
  #resultDepth[resultDepth$bins == bin,6] <- sum(r5 * area[cells] , na.rm=T)
  
}

write.csv(resultDepthBiodiv,file=paste0("../../Global biodiversity patterns of Marine Forest Species/Results/resultDepthBiodiv.csv"))
write.csv(resultDepthArea,file=paste0("../../Global biodiversity patterns of Marine Forest Species/Results/resultDepthArea.csv"))

# dfc5ab acd6c9 cabedb

plot1 <- ggplot() +
  geom_bar( data=resultDepthArea, aes(x=depth, y=Kelp, fill="#dfc5ab"), stat="identity",color="black", alpha=1,size=0.1) +
  ggtitle("Habitat suitability of marine forests") + 
  theme_map +
  ylab("Total habitat area (x1000 km2)") + xlab("Latitude") + 
  scale_fill_identity(name = '', guide = guide_legend(),labels = c('')) + theme(legend.position="None")

plot2 <- ggplot() +
  geom_bar( data=resultDepthBiodiv, aes(x=depth, y=Kelp, fill="#dfc5ab"), stat="identity",color="black", alpha=1,size=0.1) +
  ggtitle("Diversity of marine forests") + 
  theme_map +
  ylab("Average number of species with suitable habitats") + xlab("Latitude") +
  scale_fill_identity(name = '', guide = guide_legend(),labels = c('')) + theme(legend.position="None")

plotCombined <- grid.arrange(plot1, plot2, nrow = 1)
plotCombined <- cowplot::ggdraw(plotCombined)
plotCombined

pdf(file=paste0("../../Global biodiversity patterns of Marine Forest Species/Figures/FucoidFig2.pdf"),width=20,height=8,useDingbats=FALSE)
plotCombined
dev.off()

