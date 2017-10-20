library(raster)
library(sp)
library(rgdal)
library(tidyverse)
library(sf)
library(tigris)
library(corrplot)
library(grid)
library(gridExtra)
library(viridis)
library(classInt)
library(fasterize)



infolder <- "D:/Data/MonumentData/Generated Data/"  # location on Schwartz server (NEED TO ADD NEW PA LAYER AND NATURESERV RICHNESS RASTER)
outfolder <- "D:/Data/MonumentData/GeneratedData/"
PA <- st_read(paste(infolder, "PA_revised_9-21-17.shp", sep=""), stringsAsFactors=FALSE)
ind_will_sf <- st_read(paste(infolder,"ind_will_sf.shp", sep=""), stringsAsFactors=FALSE)

buffer_dist <- c(10000, 20000, 50000, 100000, 250000)
var <- c("Farm", "Forestry", "Mine", "RealEst", "Const", "LCV")
val_name <- c("max_Frm", "mx_FrNR", "mx_Mnng", "max_RE","mx_Cnst","LCVMedn")

soc.data.df <- tbl_df(PA)[,-ncol(PA)]
soc.data.df <- soc.data.df[order(soc.data.df$UnitName),]


for (i in 1:length(buffer_dist)){
PA.buf <- st_buffer(PA, dist = buffer_dist[i])
temp.int.df <- st_intersection(ind_will_sf, PA.buf)  # intersect PAs and richness polygons
  for (j in 1: length(val_name)){
    temp.sum.df <- temp.int.df %>% mutate(intersectPolyArea =  as.numeric(st_area(geometry))) %>%  # calculate areas of intersection polygons
    group_by(UnitName) %>%  # group intersection polygons by PA
    mutate(sumIntersectArea = sum(intersectPolyArea)) %>% # get the sum of intersect polygon areas associated with each PA (could be different than PA area because of missing data (e.g., watersheds with no fish richness))
    mutate(overlapProportion = intersectPolyArea/sumIntersectArea) %>% # get the proportion of the summed intersect areas associated with each intersect polygon (these are the "weights")
    mutate(weightedValue = overlapProportion * get(val_name[j])) %>%  # weighted value = weight x richness value
    group_by(UnitName) %>%
    summarise(weightedMean = sum(weightedValue), max=max(get(val_name[j])))

# deal with PAs that overlap blank spots in richness map, and are therefore not represented in results of above richness calculation
  summary.df <- data.frame(UnitName=temp.sum.df$UnitName, weightedMean=temp.sum.df$weightedMean, max=temp.sum.df$max, stringsAsFactors=FALSE) # create dataframe out of temp.rich.fish
  summary.PAnames <- summary.df$UnitName  # get list of PA names in the richness output
  all.PAnames <- PA.buf$UnitName  # get list of all PA names, including those missing from richness output
  '%notin%' <- function(x,y) !(x %in% y)
  missing.summary.PAnames <- PA.buf$UnitName[which(PA.buf$UnitName %notin% summary.df$UnitName)]  # find PAs missing from richness output
  summary.df.corrected <- data.frame(UnitName=c(summary.df$UnitName, missing.summary.PAnames),
                                  mean.val=c(summary.df$weightedMean, rep(NA, length(missing.summary.PAnames))),
                                  max.val=c(summary.df$max, rep(NA, length(missing.summary.PAnames))))  # add missing PAs to new dataframe with NA for mean and max richness value
colnames(summary.df.corrected) <- paste(var[j], colnames(summary.df.corrected),buffer_dist[i], sep =".")
soc.data.df <- cbind(soc.data.df, summary.df.corrected[,2:3])
}
}

write.csv(paste0(outfolder, "socData.csv"), row.names=FALSE)
