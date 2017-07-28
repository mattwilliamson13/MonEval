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

#infolder <- "C:/Users/Tyler/Google Drive/MonumentData/Generated Data"  # location on Tyler's Google Drive
infolder <- "D:/Data/MonumentData/Generated Data"  # location on Schwartz server

rasterOptions(tmpdir = "D:/RastTemp/", progress="text", overwrite=TRUE)  # turn on progress bar for raster operations

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
# lcv <- st_read()
# lcv <- st_read()
PA <- st_read(paste(infolder, "/PA.shp", sep=""), stringsAsFactors=FALSE)
# Fix duplicate PA names for five wilderness areas by adding a 1 or 2 to the PA name
PA <- PA[order(PA$UnitName),]
dupes <- which(PA$UnitName %in% c("Black Canyon Wilderness", "Coyote Mountains Wilderness", "Granite Mountain Wilderness", "Hells Canyon Wilderness", "Red Mountain Wilderness"))
addon <- rep(c(1,2),length(dupes)/2)
for(i in 1:length(dupes)) {
  PA$UnitName[dupes[i]] <- paste(PA$UnitName[dupes[i]],addon[i], sep=" ")
}


### PA ZONAL STATISTICS FOR RASTER INPUTS ###

PA.sp <- as(PA, "Spatial") # convert sf polygon layer to a spatial layer first (required for extract function)
inputnames <- c("climate", "rich.bird", "rich.mammal", "rich.tree", "rich.reptile")  # rasters for which we want to calculate zonal stats
outputnames <- paste(fun,inputnames,sep=".") # vectors that will hold output values
for(i in 1:length(inputnames)) {  # calculate zonal stats for each input raster
  start <- Sys.time()
  zonalvals <- raster::extract(x=get(inputnames[i]), y=PA.sp, weights=TRUE)  # extract raster values and weights (e.g., cell area proportions) within each PA polygon
  meanvals <- sapply(zonalvals, function(x) sum(apply(x, 1, prod),na.rm=T) / sum(x[,2],na.rm=T))   # calculate weighted mean
  assign(paste("mean",inputnames[i],sep="."),meanvals)  # write to output variable
  maxvals <- sapply(zonalvals, function(x) max(x, na.rm=T))   # calculate max
  assign(paste("max",inputnames[i],sep="."),maxvals)   # write to output variable
  end <- Sys.time()
  process <- end - start   # calculate processing time
  print(paste0(i, "Process=", process))
}


### PA ZONAL STATISTICS FOR VECTOR INPUTS ###

# Mean richness
temp.rich.fish <- st_intersection(rich.fish, PA) %>%  # intersect PAs and richness polygons
  mutate(intersectPolyArea =  as.numeric(st_area(geometry))) %>%  # calculate areas of intersection polygons
  group_by(UnitName) %>%  # group intersection polygons by PA
  mutate(sumIntersectArea = sum(intersectPolyArea)) %>% # get the sum of intersect polygon areas associated with each PA (could be different than PA area because of missing data (e.g., watersheds with no fish richness))
  mutate(overlapProportion = intersectPolyArea/sumIntersectArea) %>% # get the proportion of the summed intersect areas associated with each intersect polygon (these are the "weights")
  mutate(weightedValue = overlapProportion * Join_Count) %>%  # weighted value = weight x richness value
  group_by(UnitName) %>%
  summarise(weightedMean = sum(weightedValue), max=max(Join_Count))
# deal with PAs that overlap blank spots in richness map, and are therefore not represented in results of above richness calculation
rich.fish.df <- data.frame(UnitName=temp.rich.fish$UnitName, weightedMean=temp.rich.fish$weightedMean, max=temp.rich.fish$max, stringsAsFactors=FALSE) # create dataframe out of temp.rich.fish
fish.PAnames <- rich.fish.df$UnitName  # get list of PA names in the richness output
all.PAnames <- PA$UnitName  # get list of all PA names, including those missing from richness output
'%notin%' <- function(x,y) !(x %in% y)
missing.fish.PAnames <- PA$UnitName[which(PA$UnitName %notin% rich.fish.df$UnitName)]  # find PAs missing from richness output
rich.fish.df.corrected <- data.frame(UnitName=c(rich.fish.df$UnitName, missing.fish.PAnames), mean.rich.fish=c(rich.fish.df$weightedMean, rep(NA, length(missing.fish.PAnames))), max.rich.fish=c(rich.fish.df$max, rep(NA, length(missing.fish.PAnames))))  # add missing PAs to new dataframe with NA for mean and max richness value


temp.rich.amphib <- st_intersection(rich.amphib, PA) %>%  # intersect PAs and richness polygons
  mutate(intersectPolyArea =  as.numeric(st_area(geometry))) %>%  # calculate areas of intersection polygons
  group_by(UnitName) %>%  # group intersection polygons by PA
  mutate(sumIntersectArea = sum(intersectPolyArea)) %>% # get the sum of intersect polygon areas associated with each PA (could be different than PA area because of missing data (e.g., watersheds with no fish richness))
  mutate(overlapProportion = intersectPolyArea/sumIntersectArea) %>% # get the proportion of the summed intersect areas associated with each intersect polygon (these are the "weights")
  mutate(weightedValue = overlapProportion * Join_Count) %>%  # weighted value = weight x richness value
  group_by(UnitName) %>%
  summarise(weightedMean = sum(weightedValue), max=max(Join_Count))
mean.rich.amphib <- temp.rich.amphib$weightedMean
max.rich.amphib <- temp.rich.amphib$max
# deal with PAs that overlap blank spots in richness map, and are therefore not represented in results of above richness calculation
rich.amphib.df <- data.frame(UnitName=temp.rich.amphib$UnitName, weightedMean=temp.rich.amphib$weightedMean, max=temp.rich.amphib$max, stringsAsFactors=FALSE) # create dataframe out of temp.rich.amphib
amphib.PAnames <- rich.amphib.df$UnitName  # get list of PA names in the richness output
all.PAnames <- PA$UnitName  # get list of all PA names, including those missing from richness output
'%notin%' <- function(x,y) !(x %in% y)
missing.amphib.PAnames <- PA$UnitName[which(PA$UnitName %notin% rich.amphib.df$UnitName)]  # find PAs missing from richness output
rich.amphib.df.corrected <- data.frame(UnitName=c(rich.amphib.df$UnitName, missing.amphib.PAnames), mean.rich.amphib=c(rich.amphib.df$weightedMean, rep(NA, length(missing.amphib.PAnames))), max.rich.amphib=c(rich.amphib.df$max, rep(NA, length(missing.amphib.PAnames))))  # add missing PAs to new dataframe with NA for mean and max richness value



# need to add code here for LCV and sector dominance



### PA ZONAL STATS FOR ECOLOGICAL SYSTEM RICHNESS (USING RAREFACTION METHOD TO ACCOUNT FOR DIFFERENCES IN AREA)

PA.sp <- as(PA, "Spatial") # convert sf polygon layer to a spatial layer first (required for extract function)
ecol.systems <- raster::extract(natlandcover, PA.sp, na.rm=TRUE)  # list of ecological systems (by ID) within each PA
nsamples <- 1000  # number of random samples you want to use for rarefaction
mincells <- min(as.numeric(lapply(ecol.systems, function(x) length(x)))) # get smallest number of cells within a PA
richness.mat <- matrix(nrow=length(ecol.systems), ncol=nsamples) # preallocate matrix to hold means of sample
for(i in 1:nsamples) {  # loop through 1000 random samples
  sample.data <- lapply(ecol.systems, function(x) sample(x, size=mincells, replace=FALSE))  # sample mincells (without replacement) from values for each PA
  sample.richness <- as.numeric(lapply(sample.data, function(x) length(unique(x)))) # calculate number of unique values in sample (i.e., richness)
  richness.mat[,i] <- sample.richness  # write richness values for sample i to matrix
}
system.richness.rare <- rowMeans(richness.mat)  # calculate mean across samples for each PA




### CLASSIFY PAs BY BAILEY DIVISION

# determine which Bailey's ecoregions each PA intersects
intersections <- st_intersects(PA, bailey) # find which bailey features intersect each PA
count.int <- sapply(intersections, length) # get count of how many divisions intersect each polygon
hist(count.int)  # look at histogram

# get majority division for each PA
pi <- st_intersection(bailey, PA)  # get intersections between polygons in PA and bailey layers
piArea <- pi %>%   # get areas of intersections
  mutate(area = st_area(.) %>% as.numeric())
totalArea <- piArea %>%  # calculate total areas of each bailey division for each PA
  as_tibble() %>%
  group_by(UnitName, DIVISION) %>%
  summarize(area = sum(area))
baileyMajority <- totalArea %>%  # for each PA, keep the row with the division that has the largest area
  as_tibble() %>%
  group_by(UnitName) %>%
  top_n(n=1)
bailey.majority <- baileyMajority$DIVISION  # vector of majority division to include in PA dataframe



### CLASSIFY PAs BY STATE

# dissolve states layer (one multipolygon feature per states)
states <- st_read(paste(infolder, "/states2.shp", sep=""), stringsAsFactors=FALSE)
states.sp <- as(states, "Spatial")
states.dissolve <- aggregate(states.sp, list(states.sp$STATE), FUN = function(x) x[1], dissolve = TRUE)
states <- as(states.dissolve, "sf")
states <- select(states, STATE)

# determine which states each PA intersects
intersections <- st_intersects(PA, states) # find which states intersect each PA
count.int <- sapply(intersections, length) # get count of how many divisions intersect each polygon
hist(count.int)  # look at histogram

# get majority state for each PA
pi <- st_intersection(states, PA)  # get intersections between polygons in PA and state layers
piArea <- pi %>%   # get areas of intersections
  mutate(area = st_area(.) %>% as.numeric())
totalArea <- piArea %>%  # calculate total areas of each states within each PA
  as_tibble() %>%
  group_by(UnitName, STATE) %>%
  summarize(area = sum(area))
stateMajority <- totalArea %>%  # for each PA, keep the row with the state that has the largest area
  as_tibble() %>%
  group_by(UnitName) %>%
  top_n(n=1)
state.majority <- stateMajority$STATE  # vector of majority states to include in PA dataframe



### COMBINE OUTPUT VARIABLES IN A SINGLE DATAFRAME

PA.df <- tbl_df(PA)[,-ncol(PA)]  # convert to a tbl object (and strip out geometry field)
outputvars <- c("mean.climate","max.climate","mean.rich.bird", "max.rich.bird", "mean.rich.mammal", "max.rich.mammal", "mean.rich.tree", "max.rich.tree","mean.rich.reptile", "max.rich.reptile", "system.richness.rare", "bailey.majority", "state.majority")  # vector of names of all output variables
for(i in 1:length(outputvars)){  # add each output variables as a new column in dataframe
  PA.df <- data.frame(PA.df, get(outputvars[i]))
}
names(PA.df)[(ncol(PA.df)-length(outputvars)+1):ncol(PA.df)] <- outputvars # give names to new output variables in dataframe
rich.fish.amphib.df <- merge(rich.fish.df.corrected, rich.amphib.df.corrected, by="UnitName")
PA.df <- merge(PA.df, rich.fish.amphib.df, by="UnitName")

# output PA.df to workspace file
save(PA.df, file=paste(infolder,"/PA_zonal_stats4.RData", sep=""))
