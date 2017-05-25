# National Monuments Analysis - DATA PREP


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


######################################################################################################
### DATA PREP
######################################################################################################

#### LOAD DATA ###

# PAs (selected categories)
PA <- st_read("C:/Work/SpatialData/NationalMonuments/Federal_PAs_5-12-17.shp")

# Federal lands (regardless of protection status, from PADUS-CBI)
fedlands <- readOGR("C:/Work/SpatialData/NationalMonuments/PADUS_CBIEdition_v2_1_rs_update_2016-09-07/CBI_federal_lands.shp")
fedlands <- as(fedlands, "sf")  # reading in directly as sf produces an error due to null geometries; read in with rgdal, then convert to sf
fedlands <- st_union(fedlands, by_feature=FALSE)

# backwards climate velocity (AdaptWest)
climate <- raster("C:/Work/SpatialData/NationalMonuments/bwvelocityrefugiaindex/bwvelocityrefugiaindex.asc")  # raster of backward climate velocity (refugia value)
crs(climate) <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"  # this layer is missing projection info, but should be in Lambert Azimuthal Equal Area

# species richness (Jenkins et al - note that data are vector format for fish and amphibians, raster for the rest)
rich.amphib <- st_read("C:/Work/SpatialData/NationalMonuments/JenkinsBiodiversityUSA/GeoTIFFs_and_shapefiles/Amphibians_total_richness.shp")
rich.fish <- st_read("C:/Work/SpatialData/NationalMonuments/JenkinsBiodiversityUSA/GeoTIFFs_and_shapefiles/Fish_total_richness.shp")
rich.bird <- raster("C:/Work/SpatialData/NationalMonuments/JenkinsBiodiversityUSA/GeoTIFFs_and_shapefiles/Birds_total_richness.tif") 
rich.mammal <- raster("C:/Work/SpatialData/NationalMonuments/JenkinsBiodiversityUSA/GeoTIFFs_and_shapefiles/Mammals_total_richness.tif")  
rich.reptile <- raster("C:/Work/SpatialData/NationalMonuments/JenkinsBiodiversityUSA/GeoTIFFs_and_shapefiles/Reptiles_total_richness.tif") 
rich.tree <- raster("C:/Work/SpatialData/NationalMonuments/JenkinsBiodiversityUSA/GeoTIFFs_and_shapefiles/Trees_total_richness.tif")

# GAP landcover
landcover <- raster("C:/Work/SpatialData/NationalMonuments/Nat_GAP_LandCover/lwr48_v2_2_1")
# remove open water, developed, ag, disturbed cover types
lc.att <- as.data.frame(levels(landcover))  # get attribute table (and convert to df)
IDs.to.remove <- lc.att$ID[which(lc.att$NVC_CLASS %in% c("Agricultural Vegetation","Developed & Other Human Use", "Open Water", "Recently Disturbed or Modified"))]
rcl <- matrix(nrow=length(IDs.to.remove), ncol=2)
rcl[,1] <- IDs.to.remove
rcl[,2] <- NA
natlandcover <- reclassify(landcover, rcl)

# Bailey's ecoregion provinces (for standardizing ecological values by major ecoregional differences)
# dissolve by province
bailey <- st_read("C:/Work/SpatialData/NationalMonuments/bailey_ecoregions/eco_us.shp")
bailey.sp <- as(bailey, "Spatial")
bailey.dissolve <- aggregate(bailey.sp, list(bailey.sp$PROVINCE), FUN = function(x) x[1], dissolve = TRUE)
bailey <- as(bailey.dissolve, "sf")
bailey <- select(bailey, PROVINCE)

# US states
#remove islands (HA, PR, VI) and AK, then dissolve
'%notin%' <- function(x,y) !(x %in% y)
states <- st_read("C:/Work/SpatialData/Boundaries/States/states_albers.shp") %>%
  filter(STATE %notin% c("Hawaii","Puerto Rico","U.S. Virgin Islands", "Alaska"))
lower48 <- st_union(states, by_feature=FALSE)# create outline polygon for lower 48 states

# LCV scores
lcv <- st_read()

# natural resource sector revenue by county
revenue <- st_read()


### REPROJECT SPATIAL LAYERS TO COMMON PROJECTION ###

# use the Lambert Azimuthal Equal Area projection that the AdaptWest climate data are in (equal area good for zonal statistics)
newproj <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

# reproject rasters (use method="ngb" for categorical variables, method="bilinear" for continuous variables)
rich.mammal <- projectRaster(rich.mammal, crs=newproj, method="bilinear")
rich.bird <- projectRaster(rich.bird, crs=newproj, method="bilinear")
rich.tree <- projectRaster(rich.tree, crs=newproj, method="bilinear")
natlandcover <- projectRaster(natlandcover, crs=newproj, method="ngb")

# reproject shapefiles
PA <- st_transform(PA, crs=newproj)
lower48 <- st_transform(lower48, crs=newproj)
rich.fish <- st_transform(rich.fish, crs=newproj)
rich.amphib <- st_transform(rich.amphib, crs=newproj)
bailey <- st_transform(bailey, crs=newproj)
fedlands <- st_transform(fedlands, crs=newproj)


### CHECK FOR INVALID GEOMETRIES IN SF LAYERS ###

geomlayernames <- c("bailey","rich.amphib")  # names of layers you want to check
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


### CROP INPUT LAYERS TO LOWER 48 ###

croplayernames <- c("PA","climate","fedlands","natlandcover","bailey")
lower48.sp <- as(lower48, "Spatial") # convert lower48 sf layer to sp (so extent can be extracted by crop function)
for(k in 1:length(croplayernames)) {
  sp.input <- as(get(croplayernames[k]), "Spatial")
  temp4 <- crop(sp.input, lower48.sp)
  temp5 <- as(temp4, "sf")
  assign(croplayernames[k], temp5)
}


### FILTER PAs LAYER BY SIZE ###

# get rid of PAs smaller than 5,000 acres (minimum for wilderness designation)
PA <- mutate(PA, area_m2 = as.numeric(st_area(geometry)))
PA <- mutate(PA, area_ac = as.numeric(area_m2/4046.86))
PA <- filter(PA, area_ac >= 5000)



