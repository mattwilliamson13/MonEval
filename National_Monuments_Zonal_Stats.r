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

load() # load workspace with prepped input layers


### PA ZONAL STATISTICS FOR RASTER INPUTS ###

PA.sp <- as(PA, "Spatial") # convert sf polygon layer to a spatial layer first (required for extract function)
fun = "mean"  # choose function for zonal stats (e.g., mean, min, max, sum)
inputnames <- c("climate", "rich.bird", "rich.mammal", "rich.tree")  # rasters for which we want to calculate zonal stats
outputnames <- paste(fun,inputnames,sep=".") # vectors that will hold output values
for(i in 1:length(inputnames)) {  # calculate zonal stats for each input raster
  zonalvals <- raster::extract(x=get(inputnames[i]), y=sample.sp, method=simple, fun=get(fun), na.rm=TRUE, weights=TRUE, normalizeWeights=TRUE, df=FALSE)
  assign(outputnames[i],zonalvals)
}

# Calculate number of ecological systems represented within each PA polygon
# note that this only works for sp class, not sf
ecol.systems <- raster::extract(natlandcover, PA.sp, na.rm=TRUE)  # list of ecological systems (by ID) within each PA
system.diversity <- lapply(ecol.systems, function(x) length(unique(x)))  # number of different systems within each PA
# THIS IS TOO SIMPLE - DOES NOT ACCOUNT FOR FACT THAT PA SIZE IS HIGHLY VARIABLE, AND ECOL SYSTEM DIVERSITY DEPENDS ON SIZE
  # new steps needed:
  # 1. Extract all lands within each Bailey's province
  # 2. Calculate area of each ecological system within the province (these will be rarity weights)
  # 3. For each PA, calculate number of ecological systems within the PA
  # 4. Calculate rarity-weighted richness of ecological systems: summation of 1/Ci (where Ci is the area of system i within the province)
  # 5. Standardize rarity-weighted richness by area of PA (to account for fact that larger areas should have greater representation of systems by virtue of size alone)
  # 6. Repeat calculation but use all fed lands within the province, rather than a specific PA
bailey.sp <- as(bailey, "Spatial") # convert sf polygon layer to a spatial layer first (required for extract function)
tempA <- raster::extract(natlandcover, bailey.sp, na.rm=TRUE) # list of ecological systems within each Bailey's province
tempB <- lapply(tempA, function(x), length(x))  # area of province (in # of cells from natlandcover)
tempC <- lapply(tempA, function(x), table(x))  # get number of cells of each ecological system within the province
tempD <- raster::extract(natlandcover, PA.sp, na.rm=TRUE)

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

# LCV Score
  # Steps:
    # get establishment year for each PA
    # get shapefile of congressional districts for congress during the establishment year
    # find congressional districts that overlap each PA (or within a certain distance?) (st_intersection? st_extract?)
    # calculate mean lcv score (area-weighted?) for congressional districts overlapping the PA during that congressional term

# Sector dominance



### BAILEY PROVINCE ZONAL STATISTICS FOR RASTER INPUTS ###

province.fed <- st_intersection(bailey,fedlands)  # extract federal lands within each Bailey province



### BAILEY PROVINCE ZONAL STATISTICS FOR VECTOR INPUTS ###


