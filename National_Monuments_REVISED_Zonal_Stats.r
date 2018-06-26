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
library(matrixStats)

infolder <- "C:/Users/Tyler/Google Drive/MonumentData/Generated Data"  # location on Tyler's Google Drive
#infolder <- "D:/Data/MonumentData/Generated Data"  # location on Schwartz server (NEED TO ADD NEW PA LAYER AND NATURESERV RICHNESS RASTER)
#rasterOptions(tmpdir = "D:/RastTemp/", progress="text", overwrite=TRUE)  # turn on progress bar for raster operations



###################################################################################################################################################
# specify which geographic subset you want to work with:
subset <- "lower48"   # options are "lower48", "east", or "west"
###################################################################################################################################################



### READ IN PREPPED SPATIAL DATA
rich.bird <- raster(paste(infolder, "/rich.bird.tif", sep=""))
rich.mammal <- raster(paste(infolder, "/rich.mammal.tif", sep=""))
rich.tree <- raster(paste(infolder, "/rich.tree.tif", sep=""))
rich.reptile <- raster(paste(infolder, "/rich.reptile.tif", sep=""))
rich.fish <- st_read(paste(infolder, "/rich.fish.shp", sep=""), stringsAsFactors=FALSE)
rich.amphib <- st_read(paste(infolder, "/rich.amphib.shp", sep=""), stringsAsFactors=FALSE)
rich.natserv <- raster(paste(infolder, "/natserv.tif", sep=""))  # rarity weighted richness from NatureServ

# NEED TO: 
  # use Arc to reclassify new GAP ecological systems layer (turn zeros to NoData, and possible turn developed cover types to NoData)
  # upload reclassified layer to Drive
  # replace filepath below
natlandcover <- raster(paste(infolder, "/natlandcover.tif", sep=""))  

impervious <- raster(paste(infolder, "/impervious.tif", sep=""))
climate <- raster(paste(infolder, "/climate.tif", sep=""))
PA <- st_read(paste(infolder, "/post1996_PAs_", subset, ".shp", sep=""), stringsAsFactors=FALSE)
# rename attributes (st_write abbreviates these to match ESRI character limitations)
names(PA) <- c("UnitName", "InReview", "CurDesType","CurDesAuth", "OriDesAuth", "CurDesYear", "AntiqYear", "area_m2", "area_ac", "DesMode", "clipped_area_m2", "clipped_area_ac", "clipped_fraction", "geometry")
st_crs(PA) <- proj4string(natlandcover)
PA.sp <- as(PA, "Spatial") # convert sf polygon layer to a spatial layer first (required for extract function)





### GET MEAN PERCENT IMPERVIOUS SURFACE FOR EACH PA
# this won't run on Tyler's computer - use FARM or Schwartz server
inputnames <- c("impervious")  # rasters for which we want to calculate zonal stats
for(i in 1:length(inputnames)) {  # calculate zonal stats for each input raster
  start <- Sys.time()
  zonalvals <- raster::extract(x=get(inputnames[i]), y=PA.sp, weights=TRUE, progress="window")  # extract raster values and weights (e.g., cell area proportions) within each PA polygon
  prop.nonNA <- sapply(zonalvals, function(x) sum(x[which(is.na(x[,1])==FALSE),2]) / sum(x[,2]))  # get proportion of PA area that is non-NA (i.e., has value in raster layer)
  meanvals <- sapply(zonalvals, function(x) weightedMean(x[,1], x[,2], na.rm=TRUE))   # calculate weighted mean (excluding NA cells)
  meanvals[which(prop.nonNA<0.9)] <- NA   # set mean val to NA for PAs with data available for <90% of their area
  assign(paste("mean",inputnames[i],sep="."),meanvals)  # write to output variable
  end <- Sys.time()
  process <- end - start   # calculate processing time
  print(paste0(i, "Process=", process))
}



#  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# add step here to filter out any PAs with too much impervious surface from PA and PA.sp
#  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



### PA ZONAL STATISTICS FOR RASTER INPUTS ###
inputnames <- c("climate", "rich.bird", "rich.mammal", "rich.tree", "rich.reptile", "rich.natserv")  # rasters for which we want to calculate zonal stats
for(i in 1:length(inputnames)) {  # calculate zonal stats for each input raster
  start <- Sys.time()
  zonalvals <- raster::extract(x=get(inputnames[i]), y=PA.sp, weights=TRUE, progress="window")  # extract raster values and weights (e.g., cell area proportions) within each PA polygon
  prop.nonNA <- sapply(zonalvals, function(x) sum(x[which(is.na(x[,1])==FALSE),2]) / sum(x[,2]))  # get proportion of PA area that is non-NA (i.e., has value in raster layer)
  meanvals <- sapply(zonalvals, function(x) weightedMean(x[,1], x[,2], na.rm=TRUE))   # calculate weighted mean (excluding NA cells)
  meanvals[which(prop.nonNA<0.9)] <- NA   # set mean val to NA for PAs with data available for <90% of their area
  assign(paste("mean",inputnames[i],sep="."),meanvals)  # write to output variable
  medianvals <- sapply(zonalvals, function(x) weightedMedian(x[,1], x[,2], na.rm=TRUE))
  medianvals[which(prop.nonNA<0.9)] <- NA    # set median val to NA for PAs with data available for <90% of their area
  assign(paste("median",inputnames[i],sep="."),maxvals)   # write to output variable
  maxvals <- sapply(zonalvals, function(x) max(x[,1], na.rm=T))   # calculate max
  maxvals[which(prop.nonNA<0.9)] <- NA    # set max val to NA for PAs with data available for <90% of their area
  assign(paste("max",inputnames[i],sep="."),maxvals)   # write to output variable
  minvals <- sapply(zonalvals, function(x) min(x[,1], na.rm=T))   # calculate min
  minvals[which(prop.nonNA<0.9)] <- NA    # set min val to NA for PAs with data available for <90% of their area
  assign(paste("min",inputnames[i],sep="."),minvals)   # write to output variable
  end <- Sys.time()
  process <- end - start   # calculate processing time
  print(paste0(i, "Process=", process))
}



### PA ZONAL STATS FOR ECOLOGICAL SYSTEM RICHNESS ###

# use rarefaction method to account for differences in PA area
ecol.systems <- raster::extract(natlandcover, PA.sp, progress="window")  # list of ecological systems (by ID) within each PA
system.richness <- as.numeric(lapply(ecol.systems, function(x) length(unique(x)))) # calculate raw number of ecological systems (i.e., without controlling for PA area)
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
system.richness[prop.nonNA<0.9] <- NA
system.richness.rare[prop.nonNA<0.9] <- NA






### PA ZONAL STATISTICS FOR VECTOR INPUTS ###

# Fish richness
temp.rich.fish <- st_intersection(rich.fish, PA) %>%  # intersect PAs and richness polygons
  mutate(intersectPolyArea =  as.numeric(st_area(geometry))) %>%  # calculate areas of intersection polygons
  group_by(UnitName) %>%  # group intersection polygons by PA
  mutate(sumIntersectArea = sum(intersectPolyArea)) %>% # get the sum of intersect polygon areas associated with each PA (could be different than PA area because of missing data (e.g., watersheds with no fish richness))
  mutate(overlapProportion = intersectPolyArea/sumIntersectArea) %>% # get the proportion of the summed intersect areas associated with each intersect polygon (these are the "weights")
  group_by(UnitName) %>%
  summarise(weightedMean = weightedMean(Join_Count, overlapProportion, na.rm=TRUE), weightedMedian = weightedMedian(Join_Count, overlapProportion, na.rm=TRUE), max=max(Join_Count), min=min(Join_Count), prop.nonNA=mean(sumIntersectArea)/mean(clipped_area_m2))  # get weighted mean, weighted median, maximum, minimum, and proportion of the total PA area with non-NA values
# for those PAs with less than 90% coverage of non-NA richness data, assign overall NA value
temp.rich.fish$weightedMean[temp.rich.fish$prop.nonNA<0.9] <- NA
temp.rich.fish$weightedMedian[temp.rich.fish$prop.nonNA<0.9] <- NA
temp.rich.fish$max[temp.rich.fish$prop.nonNA<0.9] <- NA
temp.rich.fish$min[temp.rich.fish$prop.nonNA<0.9] <- NA
# deal with PAs that overlap blank spots in richness map, and are therefore not represented in results of above richness calculation
rich.fish.df <- data.frame(UnitName=temp.rich.fish$UnitName, weightedMean=temp.rich.fish$weightedMean, weightedMedian=temp.rich.fish$weightedMedian, max=temp.rich.fish$max, min=temp.rich.fish$min, stringsAsFactors=FALSE) # create dataframe out of temp.rich.fish
fish.PAnames <- rich.fish.df$UnitName  # get list of PA names in the richness output
all.PAnames <- PA$UnitName  # get list of all PA names, including those missing from richness output
'%notin%' <- function(x,y) !(x %in% y)
missing.fish.PAnames <- PA$UnitName[which(PA$UnitName %notin% rich.fish.df$UnitName)]  # find PAs missing from richness output
rich.fish.df.corrected <- data.frame(UnitName=c(rich.fish.df$UnitName, missing.fish.PAnames), mean.rich.fish=c(rich.fish.df$weightedMean, rep(NA, length(missing.fish.PAnames))), median.rich.fish=c(rich.fish.df$weightedMedian, rep(NA, length(missing.fish.PAnames))), max.rich.fish=c(rich.fish.df$max, rep(NA, length(missing.fish.PAnames))), min.rich.fish=c(rich.fish.df$min, rep(NA, length(missing.fish.PAnames))))  # add missing PAs to new dataframe with NA for mean, min, and max richness value

# Amphibian richness
temp.rich.amphib <- st_intersection(rich.amphib, PA) %>%  # intersect PAs and richness polygons
  mutate(intersectPolyArea =  as.numeric(st_area(geometry))) %>%  # calculate areas of intersection polygons
  group_by(UnitName) %>%  # group intersection polygons by PA
  mutate(sumIntersectArea = sum(intersectPolyArea)) %>% # get the sum of intersect polygon areas associated with each PA (could be different than PA area because of missing data (e.g., watersheds with no amphib richness))
  mutate(overlapProportion = intersectPolyArea/sumIntersectArea) %>% # get the proportion of the summed intersect areas associated with each intersect polygon (these are the "weights")
  group_by(UnitName) %>%
  summarise(weightedMean = weightedMean(Join_Count, overlapProportion, na.rm=TRUE), weightedMedian = weightedMedian(Join_Count, overlapProportion, na.rm=TRUE), max=max(Join_Count), min=min(Join_Count), prop.nonNA=mean(sumIntersectArea)/mean(clipped_area_m2))  # get weighted mean, weighted median, maximum, minimum, and proportion of the total PA area with non-NA values
# for those PAs with less than 90% coverage of non-NA richness data, assign overall NA value
temp.rich.amphib$weightedMean[temp.rich.amphib$prop.nonNA<0.9] <- NA
temp.rich.amphib$weightedMedian[temp.rich.amphib$prop.nonNA<0.9] <- NA
temp.rich.amphib$max[temp.rich.amphib$prop.nonNA<0.9] <- NA
temp.rich.amphib$min[temp.rich.amphib$prop.nonNA<0.9] <- NA
# deal with PAs that overlap blank spots in richness map, and are therefore not represented in results of above richness calculation
rich.amphib.df <- data.frame(UnitName=temp.rich.amphib$UnitName, weightedMean=temp.rich.amphib$weightedMean, weightedMedian=temp.rich.amphib$weightedMedian, max=temp.rich.amphib$max, min=temp.rich.amphib$min, stringsAsFactors=FALSE) # create dataframe out of temp.rich.amphib
amphib.PAnames <- rich.amphib.df$UnitName  # get list of PA names in the richness output
all.PAnames <- PA$UnitName  # get list of all PA names, including those missing from richness output
'%notin%' <- function(x,y) !(x %in% y)
missing.amphib.PAnames <- PA$UnitName[which(PA$UnitName %notin% rich.amphib.df$UnitName)]  # find PAs missing from richness output
rich.amphib.df.corrected <- data.frame(UnitName=c(rich.amphib.df$UnitName, missing.amphib.PAnames), mean.rich.amphib=c(rich.amphib.df$weightedMean, rep(NA, length(missing.amphib.PAnames))), median.rich.amphib=c(rich.amphib.df$weightedMedian, rep(NA, length(missing.amphib.PAnames))), max.rich.amphib=c(rich.amphib.df$max, rep(NA, length(missing.amphib.PAnames))), min.rich.amphib=c(rich.amphib.df$min, rep(NA, length(missing.amphib.PAnames))))  # add missing PAs to new dataframe with NA for mean, min, and max richness value

save.image(file="C:/Users/Tyler/Desktop/natMon_zonal_stats_up_to_systems.RData")









### REORDER OUTPUTS FROM RASTER OPERATIONS ALPHABETICALLY ###

# Output variables from raster extract are sorted by FID, but outputs from sp operations were sorted alphabetically because the dplyr function "group_by" was used and it automatically alphabetizes results
names.by.alpha <- sort(PA$UnitName)  # alphabetical vector of unit names
names.by.fid <- PA$UnitName   # vector of unit names by FID
reorder <- match(names.by.alpha, names.by.fid)  # order in which elements from raster extract outputs should appear to be alphabetically ordered
mean.climate <- mean.climate[reorder]
median.climate <- median.climate[reorder]
max.climate <- max.climate[reorder]
min.climate <- min.climate[reorder]
mean.rich.bird <- mean.rich.bird[reorder]
median.rich.bird <- median.rich.bird[reorder]
max.rich.bird <- max.rich.bird[reorder]
min.rich.bird <- min.rich.bird[reorder]
mean.rich.mammal <- mean.rich.mammal[reorder]
median.rich.mammal <- median.rich.mammal[reorder]
max.rich.mammal <- max.rich.mammal[reorder]
min.rich.mammal <- min.rich.mammal[reorder]
mean.rich.tree <- mean.rich.tree[reorder]
median.rich.tree <- median.rich.tree[reorder]
max.rich.tree <- max.rich.tree[reorder]
min.rich.tree <- min.rich.tree[reorder]
mean.rich.reptile <- mean.rich.reptile[reorder]
median.rich.reptile <- median.rich.reptile[reorder]
max.rich.reptile <- max.rich.reptile[reorder]
min.rich.reptile <- min.rich.reptile[reorder]
mean.rich.natserv <- mean.rich.natserv[reorder]
median.rich.natserv <- median.rich.natserv[reorder]
max.rich.natserv <- max.rich.natserv[reorder]
min.rich.natserv <- min.rich.natserv[reorder]
system.richness <- system.richness[reorder]
system.richness.rare <- system.richness.rare[reorder]
mean.impervious <- mean.impervious[reorder]



### COMBINE OUTPUT VARIABLES IN A SINGLE DATAFRAME AND SAVE TO DRIVE ###

PA.df <- tbl_df(PA)[,-ncol(PA)]  # convert to a tbl object (and strip out geometry field)
PA.df <- PA.df[order(PA.df$UnitName),]  # sort original dataframe alphabetically
outputvars <- c("mean.climate","median.climate", "max.climate", "min.climate", "mean.rich.bird", "median.rich.bird", "max.rich.bird", "min.rich.bird", "mean.rich.mammal", "median.rich.mammal", "max.rich.mammal", "min.rich.mammal", "mean.rich.tree", "median.rich.tree", "max.rich.tree", "min.rich.tree", "mean.rich.reptile", "median.rich.reptile", "max.rich.reptile", "min.rich.reptile", "mean.rich.natserv", "median.rich.natserv", "max.rich.natserv", "min.rich.natserv", "system.richness", "system.richness.rare", "mean.impervious")  # vector of names of all output variables
for(i in 1:length(outputvars)){  # add each output variables as a new column in dataframe
  PA.df <- data.frame(PA.df, get(outputvars[i]))
}
names(PA.df)[(ncol(PA.df)-length(outputvars)+1):ncol(PA.df)] <- outputvars # give names to new output variables in dataframe
rich.fish.amphib.df <- merge(rich.fish.df.corrected, rich.amphib.df.corrected, by="UnitName")
PA.df <- merge(PA.df, rich.fish.amphib.df, by="UnitName")
PA_zonal.df <- PA.df
save(PA_zonal.df, file=paste(infolder,"/post1996_", subset, "_PA_zonal_stats_5-10-18.RData", sep=""))  # output to workspace file