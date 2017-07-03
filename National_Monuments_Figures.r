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
library(RColorBrewer)
library(ggsn)

infolder <- "C:/Users/Tyler/Google Drive/MonumentData/Generated Data"  # set folder holding input data

# load in dataframe with output variables for PAs
load(paste(infolder,"/PA_zonal_stats.RData"))

# load spatial data
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
states <- st_read(paste(infolder, "/states2.shp", sep=""))

# remove two NMs that were designated by inter-agency agreement
PA <- filter(PA, DesigAuth %in% c("Congress","President"))
PA.df <- filter(PA.df, DesigAuth %in% c("Congress","President"))
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

# NOTE: can't map a multipolygon layer using geom_sf, so we'll need to go 
  # back and generate a non-unioned fedlands layer if we want to include in the plot
# the fix for this is fedlands.cast <- st_cast(fedlands, "POLYGON")

# can't map multipolygon layers using geom_sf, so convert fedlands back to polygons layer
fedlands.cast <- st_cast(fedlands, "POLYGON") 

# NOTE: following plot takes a very long time to generate figure in R -
# either run on cluster or comment out fedlands line when tweaking figure
ggplot() +
  geom_sf(data=states, fill="grey85", color=NA) +  # light grey background for lower 48 states
  #geom_sf(data=fedlands.cast, fill="grey70", color=NA) +  # federal lands in darker grey
  geom_sf(data=states, fill=NA, color="white") +  # state outlines in darkest grey
  geom_sf(data=PA, aes(fill=DesigAuth), color=NA) +  # protected areas on top
  ggtitle("Federal protected areas of the contiguous United States") +
  theme_bw() +
  scale_fill_discrete(name="Designating\nauthority")



##########################################################################################
### BOXPLOT OF Congressional versus presidential PAs by Bailey's division

ggplot() +
  geom_bar(data=PA.df, aes(x = bailey.majority, fill=DesigAuth), position="dodge") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1))

# now, just for national monuments
PA.nm <- filter(PA.df, DesigType=="National Monument")
ggplot() +
  geom_bar(data=PA.nm, aes(x = bailey.majority, fill=DesigAuth)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1))


##################################################################
### Map of Bailey's divisions with PAs overlaid

tol21rainbow= c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")
ggplot() +
  geom_sf(data=bailey, aes(fill=DIVISION), color=NA) +
  scale_fill_manual("Bailey Division", values=tol21rainbow) + 
  geom_sf(data=PA, fill=NA, color="black")

# same thing, but only NMs overlaid
PA.nm <- filter(PA, DesigType=="National Monument")
ggplot() +
  geom_sf(data=bailey, aes(fill=DIVISION), color=NA) +
  scale_fill_manual("Bailey Division", values=tol21rainbow) + 
  geom_sf(data=PA.nm, fill=NA, color="black")
  

#############################################################
# Compare mean richness of mammals between PNMs and CPAs
PA.df$mean.rich.mammal <- rnorm(nrow(PA.df))  # create sample data until zonal stats are run
ggplot() +
  geom_boxplot(data=PA.df, aes(x=bailey.majority, y=mean.rich.mammal, fill=DesigAuth)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1))


### test out map option
map1 <- ggplot() +
  geom_sf(data=states) +
  scalebar(data=states, dist=100000, dd2km=FALSE, model="GRS80")
  scalebar(data=bailey, dist=1000, height=0.05, st.dist=0.02, st.bottom=TRUE, st.size=8, dd2km=FALSE, model="GRS80")
north2(map1, x=0.1, y=0.2, scale=0.1, symbol=16)

