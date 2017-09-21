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

infolder <- "C:/Users/Tyler/Google Drive/MonumentData/" # folder where spatial data input layers are stored;
PA <- st_read(paste(infolder,"Revised_Federal_PAs_9-20-17_repaired.shp", sep=""))
PA <- PA %>%  # get rid of PAs smaller than 5,000 acres (minimum for wilderness designation)
  mutate(area_m2 = as.numeric(st_area(geometry))) %>%
  mutate(area_ac = as.numeric(area_m2/4046.86)) %>%
  filter(area_ac >= 5000)

'%notin%' <- function(x,y) !(x %in% y)  #remove islands (HA, PR, VI) and AK, then dissolve
states <- st_read(paste(infolder,"states_albers.shp",sep="")) %>%
  filter(STATE %notin% c("Hawaii","Puerto Rico","U.S. Virgin Islands", "Alaska"))
lower48 <- st_union(states, by_feature=FALSE)# create outline polygon for lower 48 states

landcover <- raster(paste(infolder,"lwr48_v2_2_1", sep=""))

newproj <- proj4string(landcover)
  
shapefiles <- c("PA","lower48")

for(i in 1:length(shapefiles)) {   # reproject shapefiles
  reproj <- st_transform(get(shapefiles[i]), crs=newproj)
  assign(shapefiles[i], reproj)
}

geomlayernames <- c("PA","lower48")  # names of layers you want to check - Need to re-do Fedlands on its own
options(warn=-1)  # temporarily turn off warnings
for(i in 1:length(geomlayernames)) {
  if(length(which(st_is_valid(get(geomlayernames[i]))==FALSE))==0) {
    print(paste("Geometry is valid for layer ",geomlayernames[i], sep=""))
  } else {  # if invalid geometries are found (e.g., Ring Self-intersection), convert to sp and then add zero-width buffer
    print("Invalid geometry found - applying zero-width buffer...")
    temp1 <- as(get(geomlayernames[i]), "Spatial")  # convert to sp
    temp2 <- gBuffer(temp1, byid=TRUE, width=0)  # add zero-width buffer
    temp3 <- as(temp2, "sf")   # convert back to sf
    if(length(which(st_is_valid(temp3)==FALSE))==0) {  # check again for invalid geometries
      assign(geomlayernames[i], temp3)
      print(paste("Geometry corrected for layer ", geomlayernames[i], sep=""))
    } else {
      stop(paste("Unable to correct geometry for layer ",geomlayernames[i],"!!!", sep=""))
    }
    rm(temp1, temp2, temp3)
  }
}
options(warn=0) 


croplayernames <- "PA"  # names of layers you want to crop
lower48.sp <- as(lower48, "Spatial") # convert lower48 sf layer to sp (so extent can be extracted by crop function)
for(k in 1:length(croplayernames)) {
  sp.input <- as(get(croplayernames[k]), "Spatial")
  temp4 <- raster::crop(sp.input, lower48.sp, progress = 'text')
  assign(croplayernames[k], temp4)
}

