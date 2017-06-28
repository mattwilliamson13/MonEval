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
library(ggplot2)

infolder <- "C:/Users/Tyler/Google Drive/MonumentData/Generated Data"  # set folder holding input data

PA <- st_read(paste(infolder, "/PA.shp", sep=""))
bailey <- st_read(paste(infolder, "/baileycor.shp", sep=""))
rich.bird <- raster(paste(infolder, "/rich.bird.tif", sep=""))
rich.mammal <- raster(paste(infolder, "/rich.mammal.tif", sep=""))
rich.tree <- raster(paste(infolder, "/rich.tree.tif", sep=""))
rich.reptile <- raster(paste(infolder, "/rich.reptile.tif", sep=""))
rich.fish <- st_read(paste(infolder, "/rich.fish.shp", sep=""))
rich.amphib <- st_read(paste(infolder, "/rich.amphib.shp", sep=""))
natlandcover <- raster(paste(infolder, "/natlandcover.tif", sep=""))
climate <- raster(paste(infolder, "/climate.tif", sep=""))
fedlands <- st_read(paste(infolder, "/fedlandscrop.shp", sep=""))

# remove two NMs that were designated by inter-agency agreement
PA <- filter(PA, DesigAuth %in% c("Congress","President"))

# might also be worth creating a new designating authority category for things that started as presidential NMs but are now congressional designations


####################################################################################
### DATA EXPLORATION

# Histogram of # PAs designated per year by Congress vs. President
ggplot() +
  geom_histogram(data=PA, aes(x = EstabYear, fill=DesigAuth), binwidth=1)
# Frequency plot (like histogram, but with lines instead of bars)
ggplot() +
  geom_freqpoly(data=PA, aes(x = EstabYear, color=DesigAuth), binwidth=1)
# Histogram with faceting
ggplot() +
  geom_histogram(data=PA, aes(x = EstabYear, fill=DesigAuth), binwidth=1) +
  facet_wrap(~DesigAuth, ncol=1)

# boxplots of acreage of CPAs versus PNMs
ggplot() +
  geom_boxplot(data=PA, aes(x=DesigAuth, y=area_ac), fill=c("red","green","blue"), width=0.2) +
  scale_x_discrete(name="Designating authority") +
  scale_y_continuous(name="Protected area acreage") +
  theme_bw()

# violin plot (similar to boxplot)
ggplot() +
  geom_violin(data=PA, aes(x=DesigAuth, y=area_ac)) +
  scale_x_discrete(name="Designating authority") +
  scale_y_continuous(name="Protected area acreage") +
  theme_bw()

# scatterplots of establishment year vs PA area, with faceting by designating authority
ggplot(data=PA, aes(x=EstabYear, y=area_ac)) +
  geom_point() +
  facet_wrap(~DesigAuth)



#################################################################################################
### FIGURE 1: MAP OF PROTECTED AREAS COLOR CODED BY DESIGNATION TYPE

gg1 <- ggplot() +
  geom_sf(lower48, aes())

ggplot() +
  geom_sf(data=bailey) +
  geom_sf(data=PA, aes(fill=DesigAuth, color=NULL)) +
  ggtitle("Federal protected areas of the contigous United States") +
  theme_bw()




##########################################################################################
### FIGURE 2: BOXPLOT OF 