# National Monuments Analysis - Generate zonal statistics


library(raster)
library(sp)
library(sf)
library(rgeos)
library(rgdal)
library(dplyr)
library(ggplot2)
library(viridis)
library(tidyverse)
library(rvest)
library(maptools)
library(ggplot2)

infolder <- "C:/Users/Tyler/Google Drive/MonumentData/Generated Data"  # location on Tyler's Google Drive
#infolder <- "D:/Data/MonumentData/Generated Data"  # location on Schwartz server

rasterOptions(progress="text", overwrite=TRUE)  # turn on progress bar for raster operations


### READ IN PREPPED SPATIAL DATA

PA <- st_read(paste(infolder, "/PA.shp", sep=""))
bailey <- st_read(paste(infolder, "/baileycor.shp", sep=""))
rich.bird <- raster(paste(infolder, "/rich.bird.tif", sep=""))
rich.mammal <- raster(paste(infolder, "/rich.mammal.tif", sep=""))
rich.tree <- raster(paste(infolder, "/rich.tree.tif", sep=""))
rich.reptile <- raster(paste(infolder, "/rich.reptile.tif", sep=""))
rich.fish <- st_read(paste(infolder, "/rich.fish.shp", sep=""))
rich.amphib <- st_read(paste(infolder, "/rich.amphib.shp", sep=""))
natlandcover <- raster(paste(infolder, "/natlandcover.tif", sep=""))
climate <- raster(paste(infolder, "/climate.tif", sep=""))
# lcv <- st_read()
# lcv <- st_read()


### PA ZONAL STATISTICS FOR RASTER INPUTS ###

PA.sp <- as(PA, "Spatial") # convert sf polygon layer to a spatial layer first (required for extract function)
fun = "mean"  # choose function for zonal stats (e.g., mean, min, max, sum)
inputnames <- c("climate", "rich.bird", "rich.mammal", "rich.tree", "rich.reptile")  # rasters for which we want to calculate zonal stats
outputnames <- paste(fun,inputnames,sep=".") # vectors that will hold output values
for(i in 1:length(inputnames)) {  # calculate zonal stats for each input raster
  zonalvals <- raster::extract(x=get(inputnames[i]), y=PA.sp, method=simple, fun=get(fun), na.rm=TRUE, weights=TRUE, normalizeWeights=TRUE, df=FALSE)
  assign(outputnames[i],zonalvals)
}


### PA ZONAL STATISTICS FOR VECTOR INPUTS ###

# Mean richness
mean.rich.fish <- st_intersection(rich.fish, PA) %>%  # intersect PAs and richness polygons
  mutate(intersectPolyArea =  as.numeric(st_area(geometry))) %>%  # calculate areas of intersection polygons
  group_by(UnitName) %>%  # group intersection polygons by PA
  mutate(sumIntersectArea = sum(intersectPolyArea)) %>% # get the sum of intersect polygon areas associated with each PA (could be different than PA area because of missing data (e.g., watersheds with no fish richness))
  mutate(overlapProportion = intersectPolyArea/sumIntersectArea) %>% # get the proportion of the summed intersect areas associated with each intersect polygon (these are the "weights")
  mutate(weightedValue = overlapProportion * Join_Count) %>%  # weighted value = weight x richness value
  group_by(UnitName) %>%
  summarise(weightedMean = sum(weightedValue))

mean.rich.amphib <- st_intersection(rich.amphib, sample20) %>%  # intersect PAs and richness polygons
  mutate(intersectPolyArea =  as.numeric(st_area(geometry))) %>%  # calculate areas of intersection polygons
  group_by(UnitName) %>%  # group intersection polygons by PA
  mutate(sumIntersectArea = sum(intersectPolyArea)) %>% # get the sum of intersect polygon areas associated with each PA (could be different than PA area because of missing data (e.g., watersheds with no fish richness))
  mutate(overlapProportion = intersectPolyArea/sumIntersectArea) %>% # get the proportion of the summed intersect areas associated with each intersect polygon (these are the "weights")
  mutate(weightedValue = overlapProportion * Join_Count) %>%  # weighted value = weight x richness value
  group_by(UnitName) %>%
  summarise(weightedMean = sum(weightedValue))

# need to add code here for LCV and sector dominance







### PA ZONAL STATS FOR ECOLOGICAL SYSTEM RICHNESS

PA.sp <- as(PA, "Spatial") # convert sf polygon layer to a spatial layer first (required for extract function)
ecol.systems <- raster::extract(natlandcover, PA.sp, na.rm=TRUE)  # list of ecological systems (by ID) within each PA

# simple richness metric that doesn't account for variation in PA area or rarity of systems represented
system.simple.richness <- lapply(ecol.systems, function(x) length(unique(x)))  # number of different systems within each PA

# rarity-weighted metric: higher values for PAs that include rarer ecological systems
# also controls for PA area (since larger PAs should have greater system richness by virtue of sampling area alone)
lc.vec <- as.vector(natlandcover)   # convert natlandcover to vector of values
lc.table <- as.data.frame(table(lc.vec))  # get counts of each cover type within lower 48
lc.table$rarity <- 1/lc.table$Freq  # calculate rarity score for each cover type as 1 / number of cells of that cover type

system.rw.richness <- rep(NA, length(ecol.systems)) # preallocate vector for rarity weighted richness
for(i in 1:length(ecol.systems)) {
  tempA <- sort(unique(ecol.systems[[i]]))  # get list of cover types in PA
  tempB <- lc.table$rarity[which(lc.table$lc.vec %in% tempA)]  # get rarity scores for these cover types
  system.rw.richness[i] <- sum(tempB)  # sum the rarity scores
}
system.rw.aw.richness <- system.rw.richness/PA$area_ac  # divide by PA area



# LCV Score
# Steps:
# get establishment year for each PA
# get shapefile of congressional districts for congress during the establishment year
# find congressional districts that overlap each PA (or within a certain distance?) (st_intersection? st_extract?)
# calculate mean lcv score (area-weighted?) for congressional districts overlapping the PA during that congressional term

# Sector dominance





### COMBINE OUTPUT VARIABLES IN A SINGLE DATAFRAME

# need to add code that gets state(s) and bailey's division(s) for each PA

PA.df <- tbl_df(PA)[,-ncol(PA)]  # convert to a tbl object (and strip out geometry field)
outputvars <- c(outputnames, "mean.rich.amphib", "mean.rich.fish", "system.simple.richness", "system.rw.richness","system.rw.aw.richness")  # vector of names of all output variables
for(i in 1:length(outputvars)){  # add each output variables as a new column in dataframe
  PA.df <- data.frame(PA.df, get(outputvars[i]))
}
names(PA.df)[(ncol(PA.df)-length(outputvars)+1):ncol(PA.df)] <- outputvars # give names to new output variables in dataframe



