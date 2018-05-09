# National Monuments Analysis - Generate zonal statistics for protected areas


library(raster)
library(sp)
library(sf)
library(rgeos)
library(rgdal)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(rvest)
library(maptools)
library(ggplot2)
library(diveRsity)

infolder <- "C:/Users/Tyler/Google Drive/MonumentData/Generated Data"  # location on Tyler's Google Drive
#infolder <- "D:/Data/MonumentData/Generated Data"  # location on Schwartz server (NEED TO ADD NEW PA LAYER AND NATURESERV RICHNESS RASTER)
#rasterOptions(tmpdir = "D:/RastTemp/", progress="text", overwrite=TRUE)  # turn on progress bar for raster operations



###################################################################################################################################################
# specify which geographic subset you want to work with:
subset <- "east"   # options are "lower48", "east", or "west"
###################################################################################################################################################



### READ IN PREPPED SPATIAL DATA
rich.bird <- raster(paste(infolder, "/rich.bird.tif", sep=""))
rich.mammal <- raster(paste(infolder, "/rich.mammal.tif", sep=""))
rich.tree <- raster(paste(infolder, "/rich.tree.tif", sep=""))
rich.reptile <- raster(paste(infolder, "/rich.reptile.tif", sep=""))
rich.fish <- st_read(paste(infolder, "/rich.fish.shp", sep=""), stringsAsFactors=FALSE)
rich.amphib <- st_read(paste(infolder, "/rich.amphib.shp", sep=""), stringsAsFactors=FALSE)
rich.natserv <- raster(paste(infolder, "/natserv.tif", sep=""))  # rarity weighted richness from NatureServ
natlandcover <- raster(paste(infolder, "/natlandcover.tif", sep=""))
climate <- raster(paste(infolder, "/climate.tif", sep=""))
PA <- st_read(paste(infolder, "/post1996_PAs_", subset, ".shp", sep=""), stringsAsFactors=FALSE)
# rename attributes (st_write abbreviates these to match ESRI character limitations)
names(PA) <- c("UnitName", "InReview", "CurDesType","CurDesAuth", "OriDesAuth", "CurDesYear", "AntiqYear", "area_m2", "area_ac", "DesMode", "clipped_area_m2", "clipped_area_ac", "clipped_fraction", "geometry")

PPA <- filter(PA, DesMode=="President")
CPA <- filter(PA, DesMode=="Congress")
RPA <- filter(PA, DesMode=="President then Congress")

PPA.union <- st_union(PPA, by_feature=FALSE)   # convert from 1 feature per PA to a single multipolygon feature per designation mode
CPA.union <- st_union(CPA, by_feature=FALSE)
RPA.union <- st_union(RPA, by_feature=FALSE)

PPA.sp <- as(PPA.union, "Spatial") # convert to a spatial layer first (required for extract function)
CPA.sp <- as(CPA.union, "Spatial")
RPA.sp <- as(RPA.union, "Spatial")

desmode.layers <- c("PPA.sp","CPA.sp","RPA.sp")  # names of unioned PA layers for each DesMode
desmode.names <- c("PPA", "CPA", "RPA")
inputnames <- c("climate", "rich.bird", "rich.mammal", "rich.tree", "rich.reptile", "rich.natserv")  # rasters for which we want to calculate zonal stats
for(h in 1:length(desmode.layers)) {
  for(i in 1:length(inputnames)) {  # calculate zonal stats for each input raster
    start <- Sys.time()
    print(paste0("Processing variable ",i, " of ", length(inputnames), " for designation mode ", h, " of ", length(desmode.names)))
    zonalvals <- raster::extract(x=get(inputnames[i]), y=get(desmode.layers[h]), weights=TRUE, progress="window")  # extract raster values and weights (e.g., cell area proportions) for the multipolygon features
    zonal.df <- as.data.frame(zonalvals[[1]])  # convert to a dataframe
    distvals <- sample(x=zonal.df$value, size=1000000, prob=zonal.df$weight, replace=TRUE)   # draw a large sample of cell values, with sampling probability determined by cell weight
    assign(paste(subset,desmode.names[h],inputnames[i],"distvals",sep="."),distvals)   # write sampled values to output variable
    end <- Sys.time()
    process <- end - start   # calculate processing time
    print(paste0("Complete, processing time = ", process))
  }
}



### PA ZONAL STATISTICS FOR VECTOR INPUTS ###

# not using Velox raster for extraction because it doesn't allow for weighting based on area of intersection between polygon and raster cell

desmode.layers <- c("PPA","CPA","RPA")  # names of PA layers for each DesMode
desmode.names <- c("PPA", "CPA", "RPA")  # output names
inputnames <- c("rich.fish", "rich.amphib")  # richness shapefiles for which we want to calculate zonal stats
for(h in 1:length(desmode.layers)) {
  for(i in 1:length(inputnames)) {  # calculate zonal stats for each input raster
    start <- Sys.time()
    print(paste0("Processing variable ",i, " of ", length(inputnames), " for designation mode ", h, " of ", length(desmode.names)))
    temp.rich <- st_intersection(get(inputnames[i]), get(desmode.layers[h])) %>%  # intersect PAs and richness polygons
      mutate(intersectPolyArea =  as.numeric(st_area(geometry))) %>%  # calculate areas of intersection polygons
      mutate(sumIntersectArea = sum(intersectPolyArea)) %>% # get the sum of all intersect polygons
      mutate(weight = intersectPolyArea/sumIntersectArea) # get the proportion of the total intersection area associated with each intersection polygon (these are the "weights")
    distvals <- sample(x=temp.rich$Join_Count, size=1000000, prob=temp.rich$weight, replace=TRUE)   # draw a large sample of richness values for intersection polygon, with sampling probability determined by area-based weight of the polygon
    assign(paste(subset,desmode.names[h],inputnames[i],"distvals",sep="."),distvals)   # write sampled values to output variable
    end <- Sys.time()
    process <- end - start   # calculate processing time
    print(paste0("Complete, processing time = ", process))
  }
}


### SAVE OUTPUTS

save(lower48.PPA.climate.distvals, lower48.PPA.rich.bird.distvals, lower48.PPA.rich.mammal.distvals, lower48.PPA.rich.tree.distvals, lower48.PPA.rich.natserv.distvals, lower48.PPA.rich.reptile.distvals, lower48.PPA.rich.fish.distvals, lower48.PPA.rich.amphib.distvals, lower48.CPA.climate.distvals, lower48.CPA.rich.bird.distvals, lower48.CPA.rich.mammal.distvals, lower48.CPA.rich.tree.distvals, lower48.CPA.rich.natserv.distvals, lower48.CPA.rich.reptile.distvals, lower48.CPA.rich.fish.distvals, lower48.CPA.rich.amphib.distvals, 
     west.PPA.climate.distvals, west.PPA.rich.bird.distvals, west.PPA.rich.mammal.distvals, west.PPA.rich.tree.distvals, west.PPA.rich.natserv.distvals, west.PPA.rich.reptile.distvals, west.PPA.rich.fish.distvals, west.PPA.rich.amphib.distvals, west.CPA.climate.distvals, west.CPA.rich.bird.distvals, west.CPA.rich.mammal.distvals, west.CPA.rich.tree.distvals, west.CPA.rich.natserv.distvals, west.CPA.rich.reptile.distvals, west.CPA.rich.fish.distvals, west.CPA.rich.amphib.distvals,
     east.PPA.climate.distvals, east.PPA.rich.bird.distvals, east.PPA.rich.mammal.distvals, east.PPA.rich.tree.distvals, east.PPA.rich.natserv.distvals, east.PPA.rich.reptile.distvals, east.PPA.rich.fish.distvals, east.PPA.rich.amphib.distvals, east.CPA.climate.distvals, east.CPA.rich.bird.distvals, east.CPA.rich.mammal.distvals, east.CPA.rich.tree.distvals, east.CPA.rich.natserv.distvals, east.CPA.rich.reptile.distvals, east.CPA.rich.fish.distvals, east.CPA.rich.amphib.distvals,
     file="C:/Users/Tyler/Desktop/per_unit_area_stats_5-8-18.RData")


### DENSITY PLOTS

bandwidth.multiplier <- 7

p1 <- ggplot() +   
  geom_density(aes(x=get(paste0(subset,".CPA.rich.bird.distvals")), col="first", fill="first"), alpha=0.35, size=1, adjust=bandwidth.multiplier) + 
  geom_density(aes(x=get(paste0(subset,".PPA.rich.bird.distvals")), col="second", fill="second"), alpha=0.35, size=1, adjust=bandwidth.multiplier) +
  labs(x="Bird richness", y="Density") +
  ggtitle("Bird richness") +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) + # put legend in top right corner
  theme(legend.text=element_text(size=8), legend.title=element_text(size=8)) +
  scale_fill_manual(name="Designation\nmode", values=c("first"="#F8766D","second"="#619CFF"), labels=c("Congress","President")) +
  scale_color_manual(name="Designation\nmode", values=c("first"="#F8766D","second"="#619CFF"), labels=c("Congress","President"))

p2 <- ggplot() +   
  geom_density(aes(x=get(paste0(subset,".CPA.rich.mammal.distvals")), col="first", fill="first"), alpha=0.35, size=1, adjust=bandwidth.multiplier) + 
  geom_density(aes(x=get(paste0(subset,".PPA.rich.mammal.distvals")), col="second", fill="second"), alpha=0.35, size=1, adjust=bandwidth.multiplier) +
  labs(x="Mammal richness", y="Density") +
  ggtitle("Mammal richness") +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) + # put legend in top right corner
  theme(legend.text=element_text(size=8), legend.title=element_text(size=8)) +
  scale_fill_manual(name="Designation\nmode", values=c("first"="#F8766D","second"="#619CFF"), labels=c("Congress","President")) +
  scale_color_manual(name="Designation\nmode", values=c("first"="#F8766D","second"="#619CFF"), labels=c("Congress","President"))

p3 <- ggplot() +   
  geom_density(aes(x=get(paste0(subset,".CPA.rich.reptile.distvals")), col="first", fill="first"), alpha=0.35, size=1, adjust=bandwidth.multiplier) + 
  geom_density(aes(x=get(paste0(subset,".PPA.rich.reptile.distvals")), col="second", fill="second"), alpha=0.35, size=1, adjust=bandwidth.multiplier) +
  labs(x="Reptile richness", y="Density") +
  ggtitle("Reptile richness") +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) + # put legend in top right corner
  theme(legend.text=element_text(size=8), legend.title=element_text(size=8)) +
  scale_fill_manual(name="Designation\nmode", values=c("first"="#F8766D","second"="#619CFF"), labels=c("Congress","President")) +
  scale_color_manual(name="Designation\nmode", values=c("first"="#F8766D","second"="#619CFF"), labels=c("Congress","President"))

p4 <- ggplot() +   
  geom_density(aes(x=get(paste0(subset,".CPA.rich.fish.distvals")), col="first", fill="first"), alpha=0.35, size=1, adjust=bandwidth.multiplier) + 
  geom_density(aes(x=get(paste0(subset,".PPA.rich.fish.distvals")), col="second", fill="second"), alpha=0.35, size=1, adjust=bandwidth.multiplier) +
  labs(x="Fish richness", y="Density") +
  ggtitle("Fish richness") +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) + # put legend in top right corner
  theme(legend.text=element_text(size=8), legend.title=element_text(size=8)) +
  scale_fill_manual(name="Designation\nmode", values=c("first"="#F8766D","second"="#619CFF"), labels=c("Congress","President")) +
  scale_color_manual(name="Designation\nmode", values=c("first"="#F8766D","second"="#619CFF"), labels=c("Congress","President"))

p5 <- ggplot() +   
  geom_density(aes(x=get(paste0(subset,".CPA.rich.amphib.distvals")), col="first", fill="first"), alpha=0.35, size=1, adjust=bandwidth.multiplier) + 
  geom_density(aes(x=get(paste0(subset,".PPA.rich.amphib.distvals")), col="second", fill="second"), alpha=0.35, size=1, adjust=bandwidth.multiplier) +
  labs(x="Amphibian richness", y="Density") +
  ggtitle("Amphibian richness") +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) + # put legend in top right corner
  theme(legend.text=element_text(size=8), legend.title=element_text(size=8)) +
  scale_fill_manual(name="Designation\nmode", values=c("first"="#F8766D","second"="#619CFF"), labels=c("Congress","President")) +
  scale_color_manual(name="Designation\nmode", values=c("first"="#F8766D","second"="#619CFF"), labels=c("Congress","President"))

p6 <- ggplot() +   
  geom_density(aes(x=get(paste0(subset,".CPA.rich.natserv.distvals")), col="first", fill="first"), alpha=0.35, size=1, adjust=bandwidth.multiplier) + 
  geom_density(aes(x=get(paste0(subset,".PPA.rich.natserv.distvals")), col="second", fill="second"), alpha=0.35, size=1, adjust=bandwidth.multiplier) +
  labs(x="G1 & G2 species richness", y="Density") +
  ggtitle("G1 & G2 species richness") +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) + # put legend in top right corner
  theme(legend.text=element_text(size=8), legend.title=element_text(size=8)) +
  scale_fill_manual(name="Designation\nmode", values=c("first"="#F8766D","second"="#619CFF"), labels=c("Congress","President")) +
  scale_color_manual(name="Designation\nmode", values=c("first"="#F8766D","second"="#619CFF"), labels=c("Congress","President"))

p7 <- ggplot() +   
  geom_density(aes(x=get(paste0(subset,".CPA.climate.distvals")), col="first", fill="first"), alpha=0.35, size=1, adjust=bandwidth.multiplier) + 
  geom_density(aes(x=get(paste0(subset,".PPA.climate.distvals")), col="second", fill="second"), alpha=0.35, size=1, adjust=bandwidth.multiplier) +
  labs(x="Climate refugial potential", y="Density") +
  ggtitle("Climate refugial potential") +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) + # put legend in top right corner
  theme(legend.text=element_text(size=8), legend.title=element_text(size=8)) +
  scale_fill_manual(name="Designation\nmode", values=c("first"="#F8766D","second"="#619CFF"), labels=c("Congress","President")) +
  scale_color_manual(name="Designation\nmode", values=c("first"="#F8766D","second"="#619CFF"), labels=c("Congress","President"))

multiplot(p1, p2, p3, p4, p5, p6, p7, cols=2)



# Can't use this approach for ecological system richness because at location has only 1 ecological system
# Best we could do is count the total number of ecological systems represented on PAs of each designation type,
# then divide that count by the total acreage of PAs of each designation type
# Result is a single number, rather than a distribution of values, for each designation type: a density if ecological systems; higher value means greater diversity of ecological systems captured per unit area designated
# Note that this doesn't distinguish between within-PA and among-PA diversity - it's all pooled within designation mode




desmode.layers <- c("PPA.sp","CPA.sp","RPA.sp")  # names of PA layers for each DesMode
desmode.names <- c("PPA", "CPA", "RPA")  # output names
vx <- velox(natlandcover)
for(h in 1:length(desmode.layers)) {
  start <- Sys.time()
  print(paste0("Processing for designation mode ", h, " of ", length(desmode.names)))
  ecol.systems <- as.vector(raster::extract(natlandcover, get(desmode.layers[h])))  # ecological systems (by ID) within PAs of designation type h

  
  
    end <- Sys.time()
  process <- end - start   # calculate processing time
  print(paste0("Complete, processing time = ", process))
}







### PA ZONAL STATS FOR ECOLOGICAL SYSTEM RICHNESS ###

# use rarefaction method to account for differences in PA area
PA.sp <- as(PA, "Spatial") # convert sf polygon layer to a spatial layer first (required for extract function)


ecol.systems <- raster::extract(natlandcover, PA.sp)  # list of ecological systems (by ID) within each PA
nsamples <- 1000  # number of random samples you want to use for rarefaction
mincells <- min(as.numeric(lapply(ecol.systems, function(x) length(x) - sum(is.na(x)))))  # get smallest number of non-NA cells within a PA
richness.mat <- matrix(nrow=length(ecol.systems), ncol=nsamples) # preallocate matrix to hold means of sample
for(i in 1:nsamples) {  # loop through 1000 random samples
  sample.data <- lapply(ecol.systems, function(x) sample(x[is.na(x)==FALSE], size=mincells, replace=FALSE))  # sample mincells (without replacement) from non-NA values for each PA
  sample.richness <- as.numeric(lapply(sample.data, function(x) length(unique(x)))) # calculate number of unique values in sample (i.e., richness)
  richness.mat[,i] <- sample.richness  # write richness values for sample i to matrix
}
system.richness.rare <- rowMeans(richness.mat)  # calculate mean across samples for each PA

# set richness value to NA for PAs with <90 percent non-NA data
prop.nonNA <- lapply(ecol.systems, function(x) 1 - sum(is.na(x)) / length(x))
system.richness.rare[prop.nonNA<0.9] <- NA


  