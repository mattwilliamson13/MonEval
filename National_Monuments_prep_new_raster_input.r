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
natserv <- raster(paste(infolder,"L48_G1G2_1_Sept2013.tif",sep=""))


'%notin%' <- function(x,y) !(x %in% y)  #remove islands (HA, PR, VI) and AK, then dissolve
states <- st_read(paste(infolder,"states_albers.shp",sep="")) %>%
  filter(STATE %notin% c("Hawaii","Puerto Rico","U.S. Virgin Islands", "Alaska"))
lower48 <- st_union(states, by_feature=FALSE)# create outline polygon for lower 48 states

landcover <- raster(paste(infolder,"lwr48_v2_2_1", sep=""))

newproj <- proj4string(landcover)

shapefiles <- "lower48"
cont.rasters <- "natserv"   # names of continuous rasters to reproject

for(i in 1:length(shapefiles)) {   # reproject shapefiles
  reproj <- st_transform(get(shapefiles[i]), crs=newproj)
  assign(shapefiles[i], reproj)
}

for(k in 1:length(cont.rasters)) {   # reproject categorical rasters
  reproj3 <- projectRaster(get(cont.rasters[k]), crs=newproj, method="bilinear")
  assign(cont.rasters[k], reproj3)
}

geomlayernames <- "lower48"  # names of layers you want to check - Need to re-do Fedlands on its own
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


lower48.sp <- as(lower48, "Spatial") # convert lower48 sf layer to sp (so extent can be extracted by crop function)

croprastnames <- "natserv"
for(k in 1:length(croprastnames)) {
  sp.input <- get(croprastnames[k])
  temp4 <- raster::crop(sp.input, lower48.sp, progress = 'text')
  assign(croprastnames[k], temp4)
}


for(l in 1:length(croprastnames)) {
  temp <- get(croprastnames[l])
  writeRaster(temp, paste0("C:/Users/Tyler/Google Drive/MonumentData/Generated Data/",croprastnames[l],".tif"), format="GTiff", prj=TRUE)
}

