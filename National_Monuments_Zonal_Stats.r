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
fun = "mean"  # choose function for zonal stats (e.g., mean, min, max, sum)
inputnames <- c("climate", "rich.bird", "rich.mammal", "rich.tree", "rich.reptile")  # rasters for which we want to calculate zonal stats
outputnames <- paste(fun,inputnames,sep=".") # vectors that will hold output values
for(i in 1:length(inputnames)) {  # calculate zonal stats for each input raster
  start <- Sys.time()
  zonalvals <- raster::extract(x=get(inputnames[i]), y=PA.sp, method=simple, fun=get(fun), na.rm=TRUE, weights=TRUE, normalizeWeights=TRUE, df=FALSE)
  assign(outputnames[i],zonalvals)
  end <- Sys.time()
  process <- end - start
  print(paste0(i, "Process=", process))
}

fun = "max"  # choose function for zonal stats (e.g., mean, min, max, sum)
inputnames <- c("climate", "rich.bird", "rich.mammal", "rich.tree", "rich.reptile")  # rasters for which we want to calculate zonal stats
outputnames <- paste(fun,inputnames,sep=".") # vectors that will hold output values
for(i in 1:length(inputnames)) {  # calculate zonal stats for each input raster
  zonalvals <- raster::extract(x=get(inputnames[i]), y=PA.sp, method=simple, fun=get(fun), na.rm=TRUE, df=FALSE)
  assign(outputnames[i],zonalvals)
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
mean.rich.fish <- temp.rich.fish$weightedMean
max.rich.fish <- temp.rich.fish$max

temp.rich.amphib <- st_intersection(rich.amphib, sample20) %>%  # intersect PAs and richness polygons
  mutate(intersectPolyArea =  as.numeric(st_area(geometry))) %>%  # calculate areas of intersection polygons
  group_by(UnitName) %>%  # group intersection polygons by PA
  mutate(sumIntersectArea = sum(intersectPolyArea)) %>% # get the sum of intersect polygon areas associated with each PA (could be different than PA area because of missing data (e.g., watersheds with no fish richness))
  mutate(overlapProportion = intersectPolyArea/sumIntersectArea) %>% # get the proportion of the summed intersect areas associated with each intersect polygon (these are the "weights")
  mutate(weightedValue = overlapProportion * Join_Count) %>%  # weighted value = weight x richness value
  group_by(UnitName) %>%
  summarise(weightedMean = sum(weightedValue), max=max(Join_Count))
mean.rich.amphib <- temp.rich.amphib$weightedMean
max.rich.amphib <- temp.rich.amphib$max

# need to add code here for LCV and sector dominance



### PA ZONAL STATS FOR ECOLOGICAL SYSTEM RICHNESS

PA.sp <- as(PA, "Spatial") # convert sf polygon layer to a spatial layer first (required for extract function)
ecol.systems <- raster::extract(natlandcover, PA.sp, na.rm=TRUE)  # list of ecological systems (by ID) within each PA

# simple richness metric that doesn't account for variation in PA area or rarity of systems represented
system.richness <- lapply(ecol.systems, function(x) length(unique(x)))  # number of different systems within each PA

# rarity-weighted metric: higher values for PAs that include rarer ecological systems
lc.vec <- as.vector(natlandcover)   # convert natlandcover to vector of values
lc.table <- as.data.frame(table(lc.vec))  # get counts of each cover type within lower 48
lc.table$rarity <- 1/lc.table$Freq  # calculate rarity score for each cover type as 1 / number of cells of that cover type

system.rw.richness <- rep(NA, length(ecol.systems)) # preallocate vector for rarity weighted richness
for(i in 1:length(ecol.systems)) {
  tempA <- sort(unique(ecol.systems[[i]]))  # get list of cover types in PA
  tempB <- lc.table$rarity[which(lc.table$lc.vec %in% tempA)]  # get rarity scores for these cover types
  system.rw.richness[i] <- sum(tempB)  # sum the rarity scores
}

# richness metrics that control for PA area (since larger PAs should have greater system richness by virtue of sampling area alone)
system.aw.richness <- system.richness/PA$area_ac
system.rw.aw.richness <- system.rw.richness/PA$area_ac  # divide by PA area



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
outputvars <- c("mean.rich.bird", "max.rich.bird", "mean.rich.mammal", "max.rich.mammal", "mean.rich.tree", "max.rich.tree","mean.rich.reptile", "max.rich.reptile", "mean.rich.amphib", "max.rich.amphib","mean.rich.fish", "max.rich.fish", "system.richness", "system.aw.richness", "system.rw.richness", "system.rw.aw.richness", "bailey.majority", "state.majority")  # vector of names of all output variables
for(i in 1:length(outputvars)){  # add each output variables as a new column in dataframe
  PA.df <- data.frame(PA.df, get(outputvars[i]))
}
names(PA.df)[(ncol(PA.df)-length(outputvars)+1):ncol(PA.df)] <- outputvars # give names to new output variables in dataframe

# add a new variable for ORIGINAL designating authority (i.e., find current parks and preserves that started out as presidential NMs but were later redesignated by Congress)
PA.df$OrigDesigAuth <- rep(NA, nrow(PA.df))
for(i in 1:nrow(PA.df)) {
  if(is.na(PA.df$OrigAntiq[i])==TRUE) {
    PA.df$OrigDesigAuth[i] <- PA.df$DesigAuth[i]
  } else {
    PA.df$OrigDesigAuth[i] <- "President"
  }
}

# output PA.df to workspace file
save(PA.df, file=paste(infolder,"/PA_zonal_stats.RData", sep=""))
