# National Monuments Analysis - Generate summary statistics for federal lands


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

#infolder <- "C:/Users/Tyler/Google Drive/MonumentData/Generated Data"  # location on Tyler's Google Drive
infolder <- "D:/Data/MonumentData/Generated Data"  # location on Schwartz server

rasterOptions(progress="text", overwrite=TRUE)  # turn on progress bar for raster operations


### READ IN PREPPED SPATIAL DATA

bailey <- st_read(paste(infolder, "/baileycor.shp", sep=""), stringsAsFactors=FALSE)
rich.bird <- raster(paste(infolder, "/rich.bird.tif", sep=""))
rich.mammal <- raster(paste(infolder, "/rich.mammal.tif", sep=""))
rich.tree <- raster(paste(infolder, "/rich.tree.tif", sep=""))
rich.reptile <- raster(paste(infolder, "/rich.reptile.tif", sep=""))
rich.fish <- st_read(paste(infolder, "/rich.fish.shp", sep=""), stringsAsFactors=FALSE)
rich.amphib <- st_read(paste(infolder, "/rich.amphib.shp", sep=""), stringsAsFactors=FALSE)
natlandcover <- raster(paste(infolder, "/natlandcover.tif", sep=""))
climate <- raster(paste(infolder, "/climate.tif", sep=""))
fedlands <- st_read(paste(infolder, "/fedlandscrop.shp", sep=""))


### STATS FOR ECOLOGICAL VARIABLES ###
# unit of analysis is all fedlands within each Bailey's division

# First, generate new multipolygon layer, where each feature represents all of the fedlands within a division
fed.bailey <- st_intersection(bailey, fedlands)
fed.bailey<- fed.bailey %>%  # calculate areas of fedlands within each division
  mutate(area_m2 = as.numeric(st_area(geometry))) %>%
  mutate(area_ac = as.numeric(area_m2/4046.86))

fed.bailey.sp <- as(fed.bailey, "Spatial") # convert sf polygon layer to a spatial layer first (required for extract function)
fun = "mean"  # choose function for zonal stats (e.g., mean, min, max, sum)
inputnames <- c("climate", "rich.bird", "rich.mammal", "rich.tree", "rich.reptile")  # rasters for which we want to calculate zonal stats
outputnames <- paste(fun,inputnames,sep=".") # vectors that will hold output values
for(i in 1:length(inputnames)) {  # calculate zonal stats for each input raster
  start <- Sys.time()
  zonalvals <- raster::extract(x=get(inputnames[i]), y=fed.bailey.sp, method=simple, fun=get(fun), na.rm=TRUE, weights=TRUE, normalizeWeights=TRUE, df=FALSE)
  assign(outputnames[i],zonalvals)
  end <- Sys.time()
  process <- end - start
  print(paste0(inputnames[i], "Process=", process))
}

fun = "max"  # choose function for zonal stats (e.g., mean, min, max, sum)
inputnames <- c("climate", "rich.bird", "rich.mammal", "rich.tree", "rich.reptile")  # rasters for which we want to calculate zonal stats
outputnames <- paste(fun,inputnames,sep=".") # vectors that will hold output values
for(i in 1:length(inputnames)) {  # calculate zonal stats for each input raster
  start <- Sys.time()
  zonalvals <- raster::extract(x=get(inputnames[i]), y=fed.bailey.sp, method=simple, fun=get(fun), na.rm=TRUE, df=FALSE)
  assign(outputnames[i],zonalvals)
  end <- Sys.time()
  process <- end - start
  print(paste0(inputnames[i], "Process=", process))
}


### PA ZONAL STATISTICS FOR VECTOR INPUTS ###

# Mean richness
temp.rich.fish <- st_intersection(rich.fish, fed.bailey) %>%  # intersect PAs and richness polygons
  mutate(intersectPolyArea =  as.numeric(st_area(geometry))) %>%  # calculate areas of intersection polygons
  group_by(DIVISION) %>%  # group intersection polygons by PA
  mutate(sumIntersectArea = sum(intersectPolyArea)) %>% # get the sum of intersect polygon areas associated with each PA (could be different than PA area because of missing data (e.g., watersheds with no fish richness))
  mutate(overlapProportion = intersectPolyArea/sumIntersectArea) %>% # get the proportion of the summed intersect areas associated with each intersect polygon (these are the "weights")
  mutate(weightedValue = overlapProportion * Join_Count) %>%  # weighted value = weight x richness value
  group_by(DIVISION) %>%
  summarise(weightedMean = sum(weightedValue), max=max(Join_Count))
mean.rich.fish <- temp.rich.fish$weightedMean
max.rich.fish <- temp.rich.fish$max

temp.rich.amphib <- st_intersection(rich.amphib, fed.bailey) %>%  # intersect PAs and richness polygons
  mutate(intersectPolyArea =  as.numeric(st_area(geometry))) %>%  # calculate areas of intersection polygons
  group_by(DIVISION) %>%  # group intersection polygons by PA
  mutate(sumIntersectArea = sum(intersectPolyArea)) %>% # get the sum of intersect polygon areas associated with each PA (could be different than PA area because of missing data (e.g., watersheds with no fish richness))
  mutate(overlapProportion = intersectPolyArea/sumIntersectArea) %>% # get the proportion of the summed intersect areas associated with each intersect polygon (these are the "weights")
  mutate(weightedValue = overlapProportion * Join_Count) %>%  # weighted value = weight x richness value
  group_by(DIVISION) %>%
  summarise(weightedMean = sum(weightedValue), max=max(Join_Count))
mean.rich.amphib <- temp.rich.amphib$weightedMean
max.rich.amphib <- temp.rich.amphib$max

# need to add code here for LCV and sector dominance



### PA ZONAL STATS FOR ECOLOGICAL SYSTEM RICHNESS

ecol.systems <- raster::extract(natlandcover, fed.bailey.sp, na.rm=TRUE)  # list of ecological systems (by ID) within each fed.bailey multipolygon

# simple richness metric that doesn't account for variation in fed.bailey multipolygon areas or rarity of systems represented
system.richness <- lapply(ecol.systems, function(x) length(unique(x)))  # number of different systems within each fed.bailey multipolygon

# rarity-weighted metric: higher values for fed.bailey multipolygons that include rarer ecological systems
lc.vec <- as.vector(natlandcover)   # convert natlandcover to vector of values
lc.table <- as.data.frame(table(lc.vec))  # get counts of each cover type within lower 48
lc.table$rarity <- 1/lc.table$Freq  # calculate rarity score for each cover type as 1 / number of cells of that cover type

system.rw.richness <- rep(NA, length(ecol.systems)) # preallocate vector for rarity weighted richness
for(i in 1:length(ecol.systems)) {
  tempA <- sort(unique(ecol.systems[[i]]))  # get list of cover types in fed.bailey multipolygon
  tempB <- lc.table$rarity[which(lc.table$lc.vec %in% tempA)]  # get rarity scores for these cover types
  system.rw.richness[i] <- sum(tempB)  # sum the rarity scores
}

# richness metrics that control for PA area (since larger PAs should have greater system richness by virtue of sampling area alone)
system.aw.richness <- system.richness/fed.bailey$area_ac
system.rw.aw.richness <- system.rw.richness/fed.bailey$area_ac  # divide by PA area




### COMBINE OUTPUT VARIABLES IN A SINGLE DATAFRAME

fed.bailey.df <- tbl_df(fed.bailey)[,-ncol(fed.bailey)]  # convert to a tbl object (and strip out geometry field)
outputvars <- c("mean.rich.bird", "max.rich.bird", "mean.rich.mammal", "max.rich.mammal", "mean.rich.tree", "max.rich.tree","mean.rich.reptile", "max.rich.reptile", "mean.rich.amphib", "max.rich.amphib","mean.rich.fish", "max.rich.fish", "system.richness", "system.aw.richness", "system.rw.richness", "system.rw.aw.richness")  # vector of names of all output variables
for(i in 1:length(outputvars)){  # add each output variables as a new column in dataframe
  fed.bailey.df <- data.frame(fed.bailey.df, get(outputvars[i]))
}
names(fed.bailey.df)[(ncol(fed.bailey.df)-length(outputvars)+1):ncol(fed.bailey.df)] <- outputvars # give names to new output variables in dataframe

# output fed.bailey.df to workspace file
save(fed.bailey.df, file=paste(infolder,"/Fedlands_bailey_zonal_stats.RData", sep=""))
