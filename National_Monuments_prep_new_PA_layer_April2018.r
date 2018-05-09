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
library(diveRsity)


infolder <- "C:/Users/Tyler/Google Drive/MonumentData/Generated Data/" # folder where spatial data input layers are stored;
# MUST HAVE FORWARD SLASH AT END OF PATH
#infolder <- "D:/Data/MonumentData/"  # location on Schwartz server

######################################################################################################
### DATA PREP
######################################################################################################


#### LOAD PA DATA AND CALCULATE PA AREAS ###
PA <- st_read(paste0(infolder, "post1996_federal_PAs_merged_4_28_18.shp"), stringsAsFactors = FALSE) %>%
  mutate(area_m2 = as.numeric(st_area(geometry))) %>%
  mutate(area_ac = as.numeric(area_m2/4046.86))


### GENERATE EASTERN US, WESTERN US, AND LOWER 48 POLYGONS FOR
lower48.states <- st_read(paste0(infolder,"states2.shp")) %>%
  filter(!STATE %in% c("Hawaii","Puerto Rico","U.S. Virgin Islands", "Alaska"))
lower48 <- st_union(lower48.states, by_feature=FALSE)# create outline polygon for lower 48 states

west.states <- st_read(paste0(infolder,"states2.shp")) %>%
  filter(STATE %in% c("Washington", "Oregon", "California", "Idaho", "Montana", "Wyoming", "Nevada", "Arizona", "Utah", "Colorado", "New Mexico"))
west <- st_union(west.states, by_feature=FALSE)

east.states <- st_read(paste0(infolder,"states2.shp")) %>%
  filter(!STATE %in% c("Washington", "Oregon", "California", "Idaho", "Montana", "Wyoming", "Nevada", "Arizona", "Utah", "Colorado", "New Mexico", "Hawaii", "Puerto Rico", "U.S. Virgin Islands", "Alaska"))
east <- st_union(east.states, by_feature=FALSE)


### REPROJECT SPATIAL LAYERS TO COMMON PROJECTION ###
# use the Lambert Azimuthal Equal Area projection that the AdaptWest climate data are in (equal area good for zonal statistics)
natlandcover <- raster(paste0(infolder, "natlandcvr.tif"))
newproj <- proj4string(natlandcover)
shapefiles <- c("PA","lower48", "west", "east")  # names of shapefiles to reproject ,"sectordom","lcv"
for(i in 1:length(shapefiles)) {   # reproject shapefiles
  reproj <- st_transform(get(shapefiles[i]), crs=newproj)
  assign(shapefiles[i], reproj)
}


### CHECK FOR INVALID GEOMETRIES IN SF LAYERS ###
geomlayernames <- shapefiles  # names of layers you want to check - Need to re-do Fedlands on its own
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
options(warn=0)  # turn warnings back on


### CREATE NEW "DESIGNATION MODE" VARIABLE (to distinguish PAs that started as presidential NMs but are now congressional designations)
PA$DesMode <- as.character(PA$CurDesAuth)  # have to use class character (instead of default factor) to manually edit values in next step
PA$DesMode[which(PA$CurDesAuth=="Congress" & PA$OriDesAuth=="President")] <- "President then Congress"
PA$DesMode <- as.factor(PA$DesMode)  # convert back to factor


### CLIP PA LAYER TO LOWER 48, WESTERN US, OR EASTERN US ###
PA.lower48 <- st_intersection(PA, lower48)
PA.west <- st_intersection(PA, west)
PA.east <- st_intersection(PA, east)


### ADD NEW AREA ATTRIBUTES FOR CLIPPED LAYERS (i.e., after removing non-terrestrial portions of PAs)
PA.lower48 <- PA.lower48 %>%
  mutate(clipped_area_m2 = as.numeric(st_area(geometry))) %>%
  mutate(clipped_area_ac = as.numeric(clipped_area_m2/4046.86)) %>%
  mutate(clipped_fraction = as.numeric(1-clipped_area_m2/area_m2))
PA.west <- PA.west %>%
  mutate(clipped_area_m2 = as.numeric(st_area(geometry))) %>%
  mutate(clipped_area_ac = as.numeric(clipped_area_m2/4046.86)) %>%
  mutate(clipped_fraction = as.numeric(1-clipped_area_m2/area_m2))
PA.east <- PA.east %>%
  mutate(clipped_area_m2 = as.numeric(st_area(geometry))) %>%
  mutate(clipped_area_ac = as.numeric(clipped_area_m2/4046.86)) %>%
  mutate(clipped_fraction = as.numeric(1-clipped_area_m2/area_m2))


### HISTOGRAMS OF PA size for eastern and western U.S. by current designation type
h1 <- ggplot(PA.east, aes(x=clipped_area_ac, fill=CurDesAuth)) +
  geom_histogram(bins=50, alpha=0.4, position="identity") +
  scale_x_log10() +
  ggtitle("Size distribution for post-1996 eastern PAs") +
  labs(x="Area (acres)", y="Count") +
  scale_fill_discrete(name="Designating\nauthority",
                      breaks=c("Congress","President"),
                      labels=c("Congress", "President")) +
  scale_color_discrete(name="Designation\nmode",
                       breaks=c("Congress","President"),
                       labels=c("Congress", "President")) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) + # put legend in top right corner
  theme(legend.text=element_text(size=8), legend.title=element_text(size=8))
h2 <- ggplot(PA.west, aes(x=clipped_area_ac, fill=CurDesAuth)) +
  geom_histogram(bins=50, alpha=0.4, position="identity") +
  scale_x_log10() +
  ggtitle("Size distribution for post-1996 western PAs") +
  labs(x="Area (acres)", y="Count") +
  scale_fill_discrete(name="Designating\nauthority",
                      breaks=c("Congress","President"),
                      labels=c("Congress", "President")) +
  scale_color_discrete(name="Designation\nmode",
                       breaks=c("Congress","President"),
                       labels=c("Congress", "President")) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) + # put legend in top right corner
  theme(legend.text=element_text(size=8), legend.title=element_text(size=8))
h3 <- ggplot(PA.lower48, aes(x=clipped_area_ac, fill=CurDesAuth)) +
  geom_histogram(bins=50, alpha=0.4, position="identity") +
  scale_x_log10() +
  ggtitle("Size distribution for post-1996 lower 48 PAs")  +
  labs(x="Area (acres)", y="Count") +
  scale_fill_discrete(name="Designating\nauthority",
                      breaks=c("Congress","President"),
                      labels=c("Congress", "President")) +
  scale_color_discrete(name="Designation\nmode",
                       breaks=c("Congress","President"),
                       labels=c("Congress", "President")) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) + # put legend in top right corner
  theme(legend.text=element_text(size=8), legend.title=element_text(size=8))
multiplot(h1, h2, h3, cols=1)



### HISTOGRAMS OF DESIGNATION DATE
ggplot() +
  geom_histogram(data=PA, aes(x = as.numeric(CurDesYear), fill=CurDesAuth), binwidth=1, alpha=0.4, position="identity")




# Error in the next step - st_write uses abbreviated versions (max 7 characters) of attribute names (e.g., InReview become InReviw)

### WRITE PROCESSED LAYERS TO GOOGLE DRIVE
writelayers <- c("PA.lower48", "PA.west", "PA.east")
writelayernames <- c("post1996_PAs_lower48.shp", "post1996_PAs_west.shp", "post1996_PAs_east.shp")
for(l in 1:length(writelayers)) {
  st_write(get(writelayers[l]), paste0(infolder,writelayernames[l]))
}