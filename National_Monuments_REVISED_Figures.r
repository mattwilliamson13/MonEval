# GRAPHICAL ANALYSIS OF BIOLOGICAL VARIABLES

# IMPORTANT NOTE: I THINK I WANT TO GET RID OF THE Y SCALING ARGUMENT IN DENSITY PLOTS

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
library(rasterVis)
library(gridExtra)
library(diveRsity)
library(grid)
library(gridExtra)
library(legendMap)


####################################################################################################
#### DATA PREP
####################################################################################################

infolder <- "C:/Users/Tyler/Google Drive/MonumentData/Generated Data"  # set folder holding input data
#infolder <- "D:/Data/MonumentData/Generated Data"  # location on Schwartz server

# load in dataframe with biological output variables for PAs (object called PA_zonal.df)
load(paste(infolder,"/post1996_lower48_PA_zonal_stats_5-10-18.RData", sep=""))

# load in sociopolitical output variables 
lcvData <- read.csv(paste(infolder,"/LCVData_12_7_17.csv", sep=""))[,-c(2:9)]  # exclude variables already in PA_zonal.df
lcvData$UnitName <- as.character(lcvData$UnitName)

econData <- read.csv(paste(infolder,"/econData_12_7_17.csv", sep=""))[,-c(2:9)]  # exclude variables already in PA_zonal.df
econData$UnitName <- as.character(econData$UnitName)

socData <- merge(lcvData, econData, by="UnitName")  # merge economic and LCV data

# fix weird character in Rio Grande Del Norte NM (accented i screws up the merge)
socData$UnitName[590] <- "Rio Grande Del Norte National Monument"
PA_zonal.df$UnitName[590] <- "Rio Grande Del Norte National Monument"

# merge social and biological output variables into single dataframe
PA_zonal.df <- merge(PA_zonal.df, socData, by="UnitName")

# load spatial data
PA <- st_read(paste(infolder, "/post1996_PAs_lower48.shp", sep=""))  # use this version of the PA shapefile that has duplicate Unit Names corrected - maps will be incorrect otherwise
# rename PA attributes (st_write abbreviates these to match ESRI character limitations)
names(PA) <- c("UnitName", "InReview", "CurDesType","CurDesAuth", "OriDesAuth", "CurDesYear", "AntiqYear", "area_m2", "area_ac", "DesMode", "clipped_area_m2", "clipped_area_ac", "clipped_fraction", "geometry")
econSpatial <- st_read(paste(infolder, "/econ_shp.shp", sep=""))
LCVspatial <- st_read(paste(infolder, "/LCV_shp.shp", sep=""))
bailey <- st_read(paste(infolder, "/baileycor.shp", sep=""))
rich.bird <- raster(paste(infolder, "/rich.bird.tif", sep=""))
rich.mammal <- raster(paste(infolder, "/rich.mammal.tif", sep=""))
rich.tree <- raster(paste(infolder, "/rich.tree.tif", sep=""))
rich.reptile <- raster(paste(infolder, "/rich.reptile.tif", sep=""))
rich.natserv <- raster(paste(infolder, "/natserv.tif", sep=""))
rich.fish <- st_read(paste(infolder, "/rich.fish.shp", sep=""))
rich.amphib <- st_read(paste(infolder, "/rich.amphib.shp", sep=""))
natlandcover <- raster(paste(infolder, "/natlandcover.tif", sep=""))
climate <- raster(paste(infolder, "/climate_clipped.tif", sep=""))
fedlands <- st_read(paste(infolder, "/fedlandscrop.shp", sep=""))
states <- st_read(paste(infolder, "/states2.shp", sep=""))

# create new dataframe that only includes PNMs under review by Trump administration
inreview.df <- PA_zonal.df %>%  # get values just for those NMs that were part of Trump's review
  filter(InReview=="Yes")



### OPTIONAL FILTERING

# get rid of RPAs (optional)
PA <- PA %>%
  filter(DesMode %in% c("President", "Congress"))
PA_zonal.df <- PA_zonal.df[which(PA_zonal.df$DesMode %in% c("President", "Congress")),]

# subset zonal data to eastern US
westernstates <- c("Washington", "Oregon", "California", "Nevada", "Idaho", "Montana", "Wyoming", "Utah", "Arizona", "Colorado", "New Mexico")
PA_zonal.df <- PA_zonal.df[-which(PA_zonal.df$state.majority %in% westernstates),]

# subset zonal data to western US
westernstates <- c("Washington", "Oregon", "California", "Nevada", "Idaho", "Montana", "Wyoming", "Utah", "Arizona", "Colorado", "New Mexico")
PA_zonal.df <- PA_zonal.df[which(PA_zonal.df$state.majority %in% westernstates),]




###################################################################################################
### FIGURE 1: MAP OF PAs BY DESIGNATION MODE WITH SIZE HISTOGRAM AS INSERT
###################################################################################################


### Map portion showing PAs by designation type
fedlands.raster <- fasterize(sf=fedlands, raster=climate, field=NULL, fun="last")  # rasterize fedlands layer to allow efficient plotting
states.wgs84 <- st_transform(states, "+init=epsg:4326")  # transform CRS to WGS84 (necessary to use legendMap for scale bar)
PA.wgs84 <- st_transform(PA, "+init=epsg:4326")
fedlands.raster.wgs84 <- projectRaster(fedlands.raster, crs="+init=epsg:4326", method="ngb")
fedlands.df <- as.data.frame(as(fedlands.raster.wgs84, "SpatialPixelsDataFrame"))  # convert to dataframe to allow plotting using geom_tile
colnames(fedlands.df) <- c("value", "x", "y")

PAmap <- ggplot() +
  geom_sf(data=states.wgs84, fill="grey85", color=NA) +  # light grey background for lower 48 states
  geom_tile(data=fedlands.df, aes(x=x, y=y, fill="zero")) +  # federal lands in darker grey
  geom_sf(data=states.wgs84, fill=NA, color="white") +  # state outlines in darkest grey
  geom_sf(data=PA.wgs84[which(PA.wgs84$DesMode=="Congress"),], aes(fill="first"), color=NA, alpha=0.5) +  # protected areas on top
  geom_sf(data=PA.wgs84[which(PA.wgs84$DesMode=="President then Congress"),], aes(fill="second"), color=NA, alpha=0.5) +  # protected areas on top
  geom_sf(data=PA.wgs84[which(PA.wgs84$DesMode=="President"),], aes(fill="third"), color=NA, alpha=0.5) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        panel.grid.major = element_line(colour = "white")) +
  scale_fill_manual(name="Designation\nmode", values=c("first"="#F8766D","second"="#619CFF",
                                                       "third"="#00BA38", "zero"="#B3B3B3"), labels=c("CPA","RPA","PPA","federal land")) +
  theme(legend.justification=c(1,0), legend.position="bottom") +   # put legend in top right corner
  scale_bar(lon=-75, lat=22, distance_lon=500, distance_lat=50, 
            distance_legend=150, dist_unit="km", rec_fill="white", 
            rec_colour="black", rec2_fill="black", rec2_colour="black", 
            legend_colour="black", legend_size=3, orientation=TRUE, 
            arrow_length=500, arrow_distance=300, arrow_north_size=4)

# Density plot of PA size distribution
sizedistlog <- ggplot() +
  geom_density(data=PA_zonal.df, aes(x=log(area_ac), fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Log PA size (acres)", y="Density") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  theme(legend.position="none",
        rect = element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_line())


# Put map and density plot in same window
grid.newpage()
large <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
small <- viewport(width = 0.4, height = 0.4, x = 0.22, y = 0.21)  # the inset in upper right
print(PAmap, vp = large)
print(sizedistlog, vp = small)






###################################################################################################
### FIGURE 2: ECOLOGICAL VARIABLES BY DESIGNATION MODE
###################################################################################################


# ADD bandwidth adjustment here
bandwidth.multiplier <- 0.5   # adjust bandwidth (how much smoothing of histogram occurs - 1 is default)
pointsize <- 1.5   # adust size of points in error bars
heightfactor  <- 50  # adjust height of tick marks on end of error bars  (height = yrange of plot / heightfactor )
stat <- "mean"   # choose within-PA summary stat; "mean", "min", or "max"
statname <- "Mean"  # write out statistic name as you want it to appear in plot legend (full name, capitalized first letter:  "Mean", "Minimum", or "Maximum)
attach(PA_zonal.df)

# bird richness
bird.vals <- get(paste0(stat,".rich.bird"))  # get mean and sd of value for each mode
bird.mean.ppa <- mean(bird.vals[which(DesMode=="President")], na.rm=TRUE)
bird.mean.cpa <- mean(bird.vals[which(DesMode=="Congress")], na.rm=TRUE)
bird.sd.ppa <- sd(bird.vals[which(DesMode=="President")], na.rm=TRUE)
bird.sd.cpa <- sd(bird.vals[which(DesMode=="Congress")], na.rm=TRUE)
r1 <- ggplot() +   # make density plot
  geom_density(data=PA_zonal.df, aes(x=get(paste0(stat,".rich.bird")), fill=DesMode, color=DesMode), alpha=0.35, size=1, adjust=bandwidth.multiplier) + 
  labs(x=paste0(statname, " richness"), y="Density") +
  ggtitle("Bird richness") +
  scale_fill_manual(name="Designation\nmode", breaks=c("Congress", "President"), values=c("red", "blue")) +
  scale_color_manual(name="Designation\nmode", breaks=c("Congress", "President"), values=c("red", "blue")) +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  guides(fill=FALSE, color=FALSE) # suppress legend
bird.yrange <- layer_scales(r1)$y$range$range[2]  # get y range of density plot
r1.errorbar <- r1 +   # add error bars to plot
  geom_errorbarh(aes(y=-bird.yrange/10, xmax=bird.mean.ppa + bird.sd.ppa, xmin=bird.mean.ppa - bird.sd.ppa, height=bird.yrange/heightfactor), color="blue", size=1) +
  geom_point(aes(y=-bird.yrange/10, x=bird.mean.ppa), color="blue", size=pointsize) +
  geom_errorbarh(aes(y=-bird.yrange/13, xmax=bird.mean.cpa + bird.sd.ppa, xmin=bird.mean.cpa - bird.sd.cpa, height=bird.yrange/heightfactor), color="red", size=1) +
  geom_point(aes(y=-bird.yrange/13, x=bird.mean.cpa), color="red", size=pointsize)

# mammal richness
mammal.vals <- get(paste0(stat,".rich.mammal"))  # get mean and sd of value for each mode
mammal.mean.ppa <- mean(mammal.vals[which(DesMode=="President")], na.rm=TRUE)
mammal.mean.cpa <- mean(mammal.vals[which(DesMode=="Congress")], na.rm=TRUE)
mammal.sd.ppa <- sd(mammal.vals[which(DesMode=="President")], na.rm=TRUE)
mammal.sd.cpa <- sd(mammal.vals[which(DesMode=="Congress")], na.rm=TRUE)
r2 <- ggplot() +    
  geom_density(data=PA_zonal.df, aes(x=get(paste0(stat,".rich.mammal")), fill=DesMode, color=DesMode), alpha=0.35, size=1, adjust=bandwidth.multiplier) + 
  labs(x=paste0(statname, " richness"), y="Density") +
  ggtitle("Mammal richness") +
  scale_fill_manual(name="Designation\nmode", breaks=c("Congress", "President"), values=c("red", "blue")) +
  scale_color_manual(name="Designation\nmode", breaks=c("Congress", "President"), values=c("red", "blue")) +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  guides(fill=FALSE, color=FALSE) # suppress legend
mammal.yrange <- layer_scales(r2)$y$range$range[2]  # get y range of density plot
r2.errorbar <- r2 +   # add error bars to plot
  geom_errorbarh(aes(y=-mammal.yrange/10, xmax=mammal.mean.ppa + mammal.sd.ppa, xmin=mammal.mean.ppa - mammal.sd.ppa, height=mammal.yrange/heightfactor), color="blue", size=1) +
  geom_point(aes(y=-mammal.yrange/10, x=mammal.mean.ppa), color="blue", size=pointsize) +
  geom_errorbarh(aes(y=-mammal.yrange/13, xmax=mammal.mean.cpa + mammal.sd.ppa, xmin=mammal.mean.cpa - mammal.sd.cpa, height=mammal.yrange/heightfactor), color="red", size=1) +
  geom_point(aes(y=-mammal.yrange/13, x=mammal.mean.cpa), color="red", size=pointsize)

# fish richness
fish.vals <- get(paste0(stat,".rich.fish"))  # get mean and sd of value for each mode
fish.mean.ppa <- mean(fish.vals[which(DesMode=="President")], na.rm=TRUE)
fish.mean.cpa <- mean(fish.vals[which(DesMode=="Congress")], na.rm=TRUE)
fish.sd.ppa <- sd(fish.vals[which(DesMode=="President")], na.rm=TRUE)
fish.sd.cpa <- sd(fish.vals[which(DesMode=="Congress")], na.rm=TRUE)
r3 <- ggplot() +    
  geom_density(data=PA_zonal.df, aes(x=get(paste0(stat,".rich.fish")), fill=DesMode, color=DesMode), alpha=0.35, size=1, adjust=bandwidth.multiplier) + 
  labs(x=paste0(statname, " richness"), y="Density") +
  ggtitle("Fish richness") +
  scale_fill_manual(name="Designation\nmode", breaks=c("Congress", "President"), values=c("red", "blue")) +
  scale_color_manual(name="Designation\nmode", breaks=c("Congress", "President"), values=c("red", "blue")) +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) + # put legend in top right corner
  theme(legend.text=element_text(size=8), legend.title=element_text(size=8))
fish.yrange <- layer_scales(r3)$y$range$range[2]  # get y range of density plot
r3.errorbar <- r3 +   # add error bars to plot
  geom_errorbarh(aes(y=-fish.yrange/10, xmax=fish.mean.ppa + fish.sd.ppa, xmin=fish.mean.ppa - fish.sd.ppa, height=fish.yrange/heightfactor), color="blue", size=1) +
  geom_point(aes(y=-fish.yrange/10, x=fish.mean.ppa), color="blue", size=pointsize) +
  geom_errorbarh(aes(y=-fish.yrange/13, xmax=fish.mean.cpa + fish.sd.ppa, xmin=fish.mean.cpa - fish.sd.cpa, height=fish.yrange/heightfactor), color="red", size=1) +
  geom_point(aes(y=-fish.yrange/13, x=fish.mean.cpa), color="red", size=pointsize)

# amphibian richness
amphib.vals <- get(paste0(stat,".rich.amphib"))  # get mean and sd of value for each mode
amphib.mean.ppa <- mean(amphib.vals[which(DesMode=="President")], na.rm=TRUE)
amphib.mean.cpa <- mean(amphib.vals[which(DesMode=="Congress")], na.rm=TRUE)
amphib.sd.ppa <- sd(amphib.vals[which(DesMode=="President")], na.rm=TRUE)
amphib.sd.cpa <- sd(amphib.vals[which(DesMode=="Congress")], na.rm=TRUE)
r4 <- ggplot() +
  geom_density(data=PA_zonal.df, aes(x=get(paste0(stat,".rich.amphib")), fill=DesMode, color=DesMode), alpha=0.35, size=1, adjust=bandwidth.multiplier) + 
  labs(x=paste0(statname, " richness"), y="Density") +
  ggtitle("Amphibian richness") +
  scale_fill_manual(name="Designation\nmode", breaks=c("Congress", "President"), values=c("red", "blue")) +
  scale_color_manual(name="Designation\nmode", breaks=c("Congress", "President"), values=c("red", "blue")) +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  guides(fill=FALSE, color=FALSE) # suppress legend
amphib.yrange <- layer_scales(r4)$y$range$range[2]  # get y range of density plot
r4.errorbar <- r4 +   # add error bars to plot
  geom_errorbarh(aes(y=-amphib.yrange/10, xmax=amphib.mean.ppa + amphib.sd.ppa, xmin=amphib.mean.ppa - amphib.sd.ppa, height=amphib.yrange/heightfactor), color="blue", size=1) +
  geom_point(aes(y=-amphib.yrange/10, x=amphib.mean.ppa), color="blue", size=pointsize) +
  geom_errorbarh(aes(y=-amphib.yrange/13, xmax=amphib.mean.cpa + amphib.sd.ppa, xmin=amphib.mean.cpa - amphib.sd.cpa, height=amphib.yrange/heightfactor), color="red", size=1) +
  geom_point(aes(y=-amphib.yrange/13, x=amphib.mean.cpa), color="red", size=pointsize)

# reptile richness
reptile.vals <- get(paste0(stat,".rich.reptile"))  # get mean and sd of value for each mode
reptile.mean.ppa <- mean(reptile.vals[which(DesMode=="President")], na.rm=TRUE)
reptile.mean.cpa <- mean(reptile.vals[which(DesMode=="Congress")], na.rm=TRUE)
reptile.sd.ppa <- sd(reptile.vals[which(DesMode=="President")], na.rm=TRUE)
reptile.sd.cpa <- sd(reptile.vals[which(DesMode=="Congress")], na.rm=TRUE)
r5 <- ggplot() +
  geom_density(data=PA_zonal.df, aes(x=get(paste0(stat,".rich.reptile")), fill=DesMode, color=DesMode), alpha=0.35, size=1, adjust=bandwidth.multiplier) + 
  labs(x=paste0(statname, " richness"), y="Density") +
  ggtitle("Reptile richness") +
  scale_fill_manual(name="Designation\nmode", breaks=c("Congress", "President"), values=c("red", "blue")) +
  scale_color_manual(name="Designation\nmode", breaks=c("Congress", "President"), values=c("red", "blue")) +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  guides(fill=FALSE, color=FALSE) # suppress legend
reptile.yrange <- layer_scales(r5)$y$range$range[2]  # get y range of density plot
r5.errorbar <- r5 +   # add error bars to plot
  geom_errorbarh(aes(y=-reptile.yrange/10, xmax=reptile.mean.ppa + reptile.sd.ppa, xmin=reptile.mean.ppa - reptile.sd.ppa, height=reptile.yrange/heightfactor), color="blue", size=1) +
  geom_point(aes(y=-reptile.yrange/10, x=reptile.mean.ppa), color="blue", size=pointsize) +
  geom_errorbarh(aes(y=-reptile.yrange/13, xmax=reptile.mean.cpa + reptile.sd.ppa, xmin=reptile.mean.cpa - reptile.sd.cpa, height=reptile.yrange/heightfactor), color="red", size=1) +
  geom_point(aes(y=-reptile.yrange/13, x=reptile.mean.cpa), color="red", size=pointsize)

# tree richness
tree.vals <- get(paste0(stat,".rich.tree"))  # get mean and sd of value for each mode
tree.mean.ppa <- mean(tree.vals[which(DesMode=="President")], na.rm=TRUE)
tree.mean.cpa <- mean(tree.vals[which(DesMode=="Congress")], na.rm=TRUE)
tree.sd.ppa <- sd(tree.vals[which(DesMode=="President")], na.rm=TRUE)
tree.sd.cpa <- sd(tree.vals[which(DesMode=="Congress")], na.rm=TRUE)
r6 <- ggplot() +
  geom_density(data=PA_zonal.df, aes(x=get(paste0(stat,".rich.tree")), fill=DesMode, color=DesMode), alpha=0.35, size=1, adjust=bandwidth.multiplier) + 
  labs(x=paste0(statname, " richness"), y="Density") +
  ggtitle("Tree richness") +
  scale_fill_manual(name="Designation\nmode", breaks=c("Congress", "President"), values=c("red", "blue")) +
  scale_color_manual(name="Designation\nmode", breaks=c("Congress", "President"), values=c("red", "blue")) +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  guides(fill=FALSE, color=FALSE) # suppress legend
tree.yrange <- layer_scales(r6)$y$range$range[2]  # get y range of density plot
r6.errorbar <- r6 +   # add error bars to plot
  geom_errorbarh(aes(y=-tree.yrange/10, xmax=tree.mean.ppa + tree.sd.ppa, xmin=tree.mean.ppa - tree.sd.ppa, height=tree.yrange/heightfactor), color="blue", size=1) +
  geom_point(aes(y=-tree.yrange/10, x=tree.mean.ppa), color="blue", size=pointsize) +
  geom_errorbarh(aes(y=-tree.yrange/13, xmax=tree.mean.cpa + tree.sd.ppa, xmin=tree.mean.cpa - tree.sd.cpa, height=tree.yrange/heightfactor), color="red", size=1) +
  geom_point(aes(y=-tree.yrange/13, x=tree.mean.cpa), color="red", size=pointsize)

# G1/G2 richness
natserv.vals <- get(paste0(stat,".rich.natserv"))  # get mean and sd of value for each mode
natserv.mean.ppa <- mean(natserv.vals[which(DesMode=="President")], na.rm=TRUE)
natserv.mean.cpa <- mean(natserv.vals[which(DesMode=="Congress")], na.rm=TRUE)
natserv.sd.ppa <- sd(natserv.vals[which(DesMode=="President")], na.rm=TRUE)
natserv.sd.cpa <- sd(natserv.vals[which(DesMode=="Congress")], na.rm=TRUE)
r7 <- ggplot() +
  geom_density(data=PA_zonal.df, aes(x=get(paste0(stat,".rich.natserv")), fill=DesMode, color=DesMode), alpha=0.35, size=1, adjust=bandwidth.multiplier) + 
  labs(x=paste0(statname, " rarity-weighted richness"), y="Density") +
  ggtitle("G1 & G2 species richness") +
  scale_fill_manual(name="Designation\nmode", breaks=c("Congress", "President"), values=c("red", "blue")) +
  scale_color_manual(name="Designation\nmode", breaks=c("Congress", "President"), values=c("red", "blue")) +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  guides(fill=FALSE, color=FALSE) # suppress legend
natserv.yrange <- layer_scales(r7)$y$range$range[2]  # get y range of density plot
r7.errorbar <- r7 +   # add error bars to plot
  geom_errorbarh(aes(y=-natserv.yrange/10, xmax=natserv.mean.ppa + natserv.sd.ppa, xmin=natserv.mean.ppa - natserv.sd.ppa, height=natserv.yrange/heightfactor), color="blue", size=1) +
  geom_point(aes(y=-natserv.yrange/10, x=natserv.mean.ppa), color="blue", size=pointsize) +
  geom_errorbarh(aes(y=-natserv.yrange/13, xmax=natserv.mean.cpa + natserv.sd.ppa, xmin=natserv.mean.cpa - natserv.sd.cpa, height=natserv.yrange/heightfactor), color="red", size=1) +
  geom_point(aes(y=-natserv.yrange/13, x=natserv.mean.cpa), color="red", size=pointsize)

# ecological system richness
system.vals <- system.richness.rare  # get mean and sd of value for each mode
system.mean.ppa <- mean(system.vals[which(DesMode=="President")], na.rm=TRUE)
system.mean.cpa <- mean(system.vals[which(DesMode=="Congress")], na.rm=TRUE)
system.sd.ppa <- sd(system.vals[which(DesMode=="President")], na.rm=TRUE)
system.sd.cpa <- sd(system.vals[which(DesMode=="Congress")], na.rm=TRUE)
r8 <- ggplot() +
  geom_density(data=PA_zonal.df, aes(system.richness.rare, fill=DesMode, color=DesMode), alpha=0.35, size=1, adjust=bandwidth.multiplier) + 
  labs(x="Rarefied richness", y="Density") +
  ggtitle("Ecological system richness") +
  scale_fill_manual(name="Designation\nmode", breaks=c("Congress", "President"), values=c("red", "blue")) +
  scale_color_manual(name="Designation\nmode", breaks=c("Congress", "President"), values=c("red", "blue")) +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  guides(fill=FALSE, color=FALSE) # suppress legend
system.yrange <- layer_scales(r8)$y$range$range[2]  # get y range of density plot
r8.errorbar <- r8 +   # add error bars to plot
  geom_errorbarh(aes(y=-system.yrange/10, xmax=system.mean.ppa + system.sd.ppa, xmin=system.mean.ppa - system.sd.ppa, height=system.yrange/heightfactor), color="blue", size=1) +
  geom_point(aes(y=-system.yrange/10, x=system.mean.ppa), color="blue", size=pointsize) +
  geom_errorbarh(aes(y=-system.yrange/13, xmax=system.mean.cpa + system.sd.ppa, xmin=system.mean.cpa - system.sd.cpa, height=system.yrange/heightfactor), color="red", size=1) +
  geom_point(aes(y=-system.yrange/13, x=system.mean.cpa), color="red", size=pointsize)

# climate refugial potential
climate.vals <- get(paste0(stat,".climate"))  # get mean and sd of value for each mode
climate.mean.ppa <- mean(climate.vals[which(DesMode=="President")], na.rm=TRUE)
climate.mean.cpa <- mean(climate.vals[which(DesMode=="Congress")], na.rm=TRUE)
climate.sd.ppa <- sd(climate.vals[which(DesMode=="President")], na.rm=TRUE)
climate.sd.cpa <- sd(climate.vals[which(DesMode=="Congress")], na.rm=TRUE)
r9 <- ggplot() +
  geom_density(data=PA_zonal.df, aes(x=get(paste0(stat,".climate")), fill=DesMode, color=DesMode), alpha=0.35, size=1, adjust=bandwidth.multiplier) + 
  labs(x=paste0(statname, " refugial potential"), y="Density") +
  ggtitle("Climate refugial potential") +
  scale_fill_manual(name="Designation\nmode", breaks=c("Congress", "President"), values=c("red", "blue")) +
  scale_color_manual(name="Designation\nmode", breaks=c("Congress", "President"), values=c("red", "blue")) +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  guides(fill=FALSE, color=FALSE) # suppress legend
climate.yrange <- layer_scales(r9)$y$range$range[2]  # get y range of density plot
r9.errorbar <- r9 +   # add error bars to plot
  geom_errorbarh(aes(y=-climate.yrange/10, xmax=climate.mean.ppa + climate.sd.ppa, xmin=climate.mean.ppa - climate.sd.ppa, height=climate.yrange/heightfactor), color="blue", size=1) +
  geom_point(aes(y=-climate.yrange/10, x=climate.mean.ppa), color="blue", size=pointsize) +
  geom_errorbarh(aes(y=-climate.yrange/13, xmax=climate.mean.cpa + climate.sd.ppa, xmin=climate.mean.cpa - climate.sd.cpa, height=climate.yrange/heightfactor), color="red", size=1) +
  geom_point(aes(y=-climate.yrange/13, x=climate.mean.cpa), color="red", size=pointsize)

detach(PA_zonal.df)
#multiplot(r1,r2,r3,r4,r5,r6,r7,r8,r9, cols=3)  # density plots only 
multiplot(r1.errorbar, r2.errorbar, r3.errorbar,r4.errorbar,r5.errorbar,r6.errorbar,r7.errorbar,r8.errorbar,r9.errorbar, cols=3)   # desnsity plots with means and SDs





### BARPLOTS
varnames <- c("LCV","Forestry","Farm","Mine")  # choose variable (LCV, Farm, Forestry, Mine)
ylabnames <- c("LCV score", "% off-farm\nagriculture", "% on-farm\nagriculture", "% mineral\nextration")

for(i in 1:length(varnames)){
  mean.varcols <- grep(paste0(varnames[i],".mean"), names(PA_zonal.df))  # column indices for mean values of selected variable
  mean.tidy <- PA_zonal.df %>%   # long-form version of dataframe for mean values
    gather(bufferKm, meanVal, mean.varcols, factor_key=TRUE)
  
  mean.buffer.plot <- ggplot() +  # boxplot of mean values as a function of buffer distance
    geom_boxplot(data=mean.tidy, aes(x=bufferKm, y=meanVal, fill=DesMode), width=0.5) +
    scale_x_discrete(labels=c("10","20","50","100","250")) +
    labs(y=ylabnames[i], x="Buffer distance (km)") +
    scale_color_discrete(name="Designation\nmode") +
    scale_fill_discrete(name="Designation\nmode") 
  assign(paste0("plot",i),mean.buffer.plot)
}


multiplot(plot1, plot2, plot3, plot4, cols=2)   # combine in single plot


b1 <- ggplot() +  # boxplot of mean values as a function of buffer distance
  geom_boxplot(data=PA_zonal.df, aes(x=1, y=mean.rich.bird, fill=DesMode), width=0.1) +
  scale_x_discrete(labels=c("10","20","50","100","250")) +
  scale_color_discrete(name="Designation\nmode") +
  scale_fill_discrete(name="Designation\nmode") 


library(reshape2)


stat <- "mean"   # "mean", "min", or "max"

stat.df <- data.frame(cbind(PA_zonal.df[,grep(stat, names(PA_zonal.df))], PA_zonal.df$DesMode))
df1_long <- melt(stat.df, id.vars="PA_zonal.df.DesMode")

p2 <- ggplot(df1_long, aes(x=PA_zonal.df.DesMode,y=value, fill=PA_zonal.df.DesMode))+
  geom_boxplot() + labs(title="CMP") +facet_wrap(~variable)
p2




###################################################################################################
### FIGURE 3: SOCIOPOLITICAL VARIABLES BY DESIGNATION MODE
###################################################################################################


bufdist1 <- "10000"   # select buffer distance; must be character format, one of these choices: "10000","20000","50000","1e.05","250000")
# LCV score
s1 <- ggplot() +
  geom_density(data=PA_zonal.df, aes(x=get(paste0("LCV.mean.val.",bufdist1)), y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Mean LCV score", y="Scaled density") +
  ggtitle("LCV score") +
  scale_color_discrete(name="Designation\nmode") +
  scale_fill_discrete(name="Designation\nmode") +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  guides(fill=FALSE, color=FALSE) # suppress legend
# Farming sector
s2 <- ggplot() +
  geom_density(data=PA_zonal.df, aes(x=get(paste0("Farm.mean.val.",bufdist1)), y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Percent workforce", y="Scaled density") +
  ggtitle("On-farm agriculture") +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  scale_fill_discrete(name="Designation\nmode",
                      breaks=c("Congress","President", "President then Congress"),
                      labels=c("CPA", "PPA", "RPA")) +
  scale_color_discrete(name="Designation\nmode",
                       breaks=c("Congress","President", "President then Congress"),
                       labels=c("CPA", "PPA", "RPA")) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) + # put legend in top right corner
  theme(legend.text=element_text(size=8), legend.title=element_text(size=8))
# Forestry sector
s3 <- ggplot() +
  geom_density(data=PA_zonal.df, aes(x=get(paste0("Forestry.mean.val.",bufdist1)), y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Percent workforce", y="Scaled density") +
  ggtitle("Off-farm agriculture") +
  scale_color_discrete(name="Designation\nmode") +
  scale_fill_discrete(name="Designation\nmode") +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  guides(fill=FALSE, color=FALSE) # suppress legend
# Mining sector
s4 <- ggplot() +
  geom_density(data=PA_zonal.df, aes(x=get(paste0("Mine.mean.val.",bufdist1)), y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Percent workforce", y="Scaled density") +
  ggtitle("Mineral extraction") +
  scale_color_discrete(name="Designation\nmode") +
  scale_fill_discrete(name="Designation\nmode") +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) + # put legend in top right corner
  guides(fill=FALSE, color=FALSE) # suppress legend

multiplot(s1, s2, s3, s4, cols=2)



################################################################################################
### Plots of variables for supplemental info
################################################################################################



# The socioeconomic variables may not make sense to present anymore, since we are interested in different time periods for areas around different PAs
# Alternatively, could just present mean or max value across the entire time period (1996-present)?



# convert raster layers to dataframes to allow plotting in ggplot
rich.bird.df <- as.data.frame(as(rich.bird, "SpatialPixelsDataFrame"))
colnames(rich.bird.df) <- c("value", "x", "y")
rich.mammal.df <- as.data.frame(as(rich.mammal, "SpatialPixelsDataFrame"))
colnames(rich.mammal.df) <- c("value", "x", "y")
rich.reptile.df <- as.data.frame(as(rich.reptile, "SpatialPixelsDataFrame"))
colnames(rich.reptile.df) <- c("value", "x", "y")
rich.tree.df <- as.data.frame(as(rich.tree, "SpatialPixelsDataFrame"))
colnames(rich.tree.df) <- c("value", "x", "y")
rich.natserv.df <- as.data.frame(as(rich.natserv, "SpatialPixelsDataFrame"))
colnames(rich.natserv.df) <- c("value", "x", "y")
climate.df <- as.data.frame(as(climate, "SpatialPixelsDataFrame"))
colnames(climate.df) <- c("value", "x", "y")

# create individual plots
birdplot <- ggplot() +
  geom_tile(data=rich.bird.df, aes(x=x, y=y, fill=value)) +
  theme(axis.text= element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        plot.title=element_text(hjust=0.5),
        panel.grid.major = element_blank()) +
  scale_fill_gradientn(colors = c("white","red")) +
  ggtitle("Bird richness") +
  theme(plot.title = element_text(size = 20)) +
  labs(fill="")

mammalplot <- ggplot() +
  geom_tile(data=rich.mammal.df, aes(x=x, y=y, fill=value)) +
  theme(axis.text= element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        plot.title=element_text(hjust=0.5),
        panel.grid.major = element_blank()) +
  scale_fill_gradientn(colors = c("burlywood1","burlywood4")) +
  ggtitle("Mammal richness") +
  theme(plot.title = element_text(size = 20)) +
  labs(fill="")

fishplot <- ggplot() +
  geom_sf(data=rich.fish, aes(fill=Join_Count), color=NA) +
  theme(axis.text= element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        plot.title=element_text(hjust=0.5),
        panel.grid.major = element_line(color="white")) +
  scale_fill_gradientn(colors = c("steelblue1","steelblue4")) +
  ggtitle("Fish richness") +
  theme(plot.title = element_text(size = 20)) +
  labs(fill="")

amphibianplot <- ggplot() +
  geom_sf(data=rich.amphib, aes(fill=Join_Count), color=NA) +
  theme(axis.text= element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        plot.title=element_text(hjust=0.5),
        panel.grid.major = element_line(color="white")) +
  scale_fill_gradientn(colors = c("yellow1","yellow4")) +
  ggtitle("Amphibian richness") +
  theme(plot.title = element_text(size = 20)) +
  labs(fill="")

reptileplot <- ggplot() +
  geom_tile(data=rich.reptile.df, aes(x=x, y=y, fill=value)) +
  theme(axis.text= element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        plot.title=element_text(hjust=0.5),
        panel.grid.major = element_blank()) +
  scale_fill_gradientn(colors = c("thistle1","thistle4")) +
  ggtitle("Reptile richness") +
  theme(plot.title = element_text(size = 20)) +
  labs(fill="")

treeplot <- ggplot() +
  geom_tile(data=rich.tree.df, aes(x=x, y=y, fill=value)) +
  theme(axis.text= element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        plot.title=element_text(hjust=0.5),
        panel.grid.major = element_blank()) +
  scale_fill_gradientn(colors = c("darkolivegreen1","darkolivegreen4")) +
  ggtitle("Tree richness") +
  theme(plot.title = element_text(size = 20)) +
  labs(fill="")

G1G2plot <- ggplot() +
  geom_tile(data=rich.natserv.df, aes(x=x, y=y, fill=value)) +
  theme(axis.text= element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        plot.title=element_text(hjust=0.5),
        panel.grid.major = element_blank()) +
  scale_fill_gradientn(colors = c("darkorange1","darkorange4")) +
  ggtitle("G1/G2 species richness") +
  theme(plot.title = element_text(size = 20)) +
  labs(fill="")

climateplot <- ggplot() +
  geom_tile(data=climate.df, aes(x=x, y=y, fill=value)) +
  theme(axis.text= element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        plot.title=element_text(hjust=0.5),
        panel.grid.major = element_blank()) +
  scale_fill_gradientn(colors = c("turquoise1","turquoise4")) +
  ggtitle("Climate refugial potential") +
  theme(plot.title = element_text(size = 20)) +
  labs(fill="")  

LCVplot <- ggplot() +
  geom_sf(data=LCVspatial, aes(fill=LCVMedn), color=NA) +
  theme(axis.text= element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        plot.title=element_text(hjust=0.5),
        panel.grid.major = element_blank()) +
  scale_fill_gradientn(colors = c("red","blue")) +
  ggtitle("LCV score") +
  theme(plot.title = element_text(size = 12)) +
  labs(fill="")

farmplot <- ggplot() +
  geom_sf(data=econSpatial, aes(fill=max_Frm), color=NA) +
  theme(axis.text= element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        plot.title=element_text(hjust=0.5),
        panel.grid.major = element_blank()) +
  scale_fill_gradientn(colors = c("darkgoldenrod","green")) +
  ggtitle("% on-farm agriculture") +
  theme(plot.title = element_text(size = 12)) +
  labs(fill="")

forestryplot <- ggplot() +
  geom_sf(data=econSpatial, aes(fill=mx_FrNR), color=NA) +
  theme(axis.text= element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        plot.title=element_text(hjust=0.5),
        panel.grid.major = element_blank()) +
  scale_fill_gradientn(colors = c("yellow","red")) +
  ggtitle("% off-farm agriculture") +
  theme(plot.title = element_text(size = 12)) +
  labs(fill="")

mineplot <- ggplot() +
  geom_sf(data=econSpatial, aes(fill=mx_Mnng), color=NA) +
  theme(axis.text= element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        plot.title=element_text(hjust=0.5),
        panel.grid.major = element_blank()) +
  scale_fill_gradientn(colors = c("lightblue","darkblue")) +
  ggtitle("% mineral extraction") +
  theme(plot.title = element_text(size = 12)) +
  labs(fill="")  

grid.arrange(birdplot, mammalplot, fishplot, amphibianplot, reptileplot, treeplot, G1G2plot, climateplot, nrow=2)
grid.arrange(LCVplot, farmplot, forestryplot, mineplot, nrow=2)




#########################################################################################
### FIGURE SHOWING PROPORTION NA AS A FUNCTION OF BUFFER SIZE
#########################################################################################

varnames <- c("LCV", "Farm", "Forestry", "Mine")
propNA.mat <- matrix(NA, nrow=length(varnames), ncol=5)  # blank matrix to hold proportion NA for each variable and buffer distance
attach(PA_zonal.df)  
#attach(inreview.df) # if you want to look at how proportion of PNMs under review with NA data changes with buffer distance
for(i in 1:length(varnames)){
  mean.names <- paste0(varnames[i],".mean.val.", c("10000","20000","50000","1e.05","250000"))    # list of variable names for means
  for(j in 1:5){
    propNA.mat[i,j] <- sum(is.na(get(mean.names[j])))/length(get(mean.names[j]))
  }
}
detach(PA_zonal.df)  
#detach(inreview.df)
# convert to dataframe with columns for bufferdist and each soc variable
propNA.df <- data.frame(bufferDist=c(10000,20000,50000,100000,250000), LCVscore=propNA.mat[1,], farming=propNA.mat[2,], forestry=propNA.mat[3,], mining=propNA.mat[4,], stringsAsFactors=FALSE)
propNA.gather <- gather(data=propNA.df, key=socVar, value=propNA, LCVscore:mining)
ggplot(data=propNA.gather, aes(x=bufferDist/1000, y=propNA, color=socVar)) +
  geom_line(linetype=1) +
  geom_point() +
  scale_color_discrete(name="Variable",
                       breaks=c("LCVscore", "farming", "forestry", "mining"),
                       labels=c("LCV score", "% on-farm agriculture", "% off-farm agriculture", "% mineral extraction")) +
  labs(x="Buffer distance (km)", y="Proportion missing data")



### Get proportion NA for ecological variables (no buffer, so a single value per variable)
ecovarnames <- c("mean.rich.bird","mean.rich.mammal", "mean.rich.fish", "mean.rich.amphib","mean.rich.reptile","mean.rich.tree","mean.rich.natserv","system.richness.rare","mean.climate")
attach(PA_zonal.df)
propNA.ecol <- rep(NA, length(ecovarnames))
for(i in 1:length(ecovarnames)){
  propNA.ecol[i] <- sum(is.na(get(ecovarnames[i])))/nrow(PA_zonal.df)
}
propNA.ecol.df <- data.frame(cbind(ecovarnames, round(propNA.ecol,2)), stringsAsFactors = FALSE)




#############################################################################################################################
### SUPPLEMENTAL FIGURE: BOXPLOTS SHOWING SHOWING COMPARISONS AMONG DESIGNATION MODES FOR DIFFERENT BUFFER DISTANCES
#############################################################################################################################

varnames <- c("LCV","Forestry","Farm","Mine")  # choose variable (LCV, Farm, Forestry, Mine)
ylabnames <- c("LCV score", "% off-farm\nagriculture", "% on-farm\nagriculture", "% mineral\nextration")

for(i in 1:length(varnames)){
  mean.varcols <- grep(paste0(varnames[i],".mean"), names(PA_zonal.df))  # column indices for mean values of selected variable
  mean.tidy <- PA_zonal.df %>%   # long-form version of dataframe for mean values
    gather(bufferKm, meanVal, mean.varcols, factor_key=TRUE)
  
  mean.buffer.plot <- ggplot() +  # boxplot of mean values as a function of buffer distance
    geom_boxplot(data=mean.tidy, aes(x=bufferKm, y=meanVal, fill=DesMode), width=0.5) +
    scale_x_discrete(labels=c("10","20","50","100","250")) +
    labs(y=ylabnames[i], x="Buffer distance (km)") +
    scale_color_discrete(name="Designation\nmode") +
    scale_fill_discrete(name="Designation\nmode") 
  assign(paste0("plot",i),mean.buffer.plot)
}


multiplot(plot1, plot2, plot3, plot4, cols=2)   # combine in single plot



#############################################################################################################################
### SUPPLEMENTAL FIGURE: DENSITY PLOTS SHOWING DISTRIBUTION OF VALUES FOR SOCIOPOLITICAL VARIABLES AT DIFFERENT BUFFER WIDTHS
#############################################################################################################################

varnames <- c("LCV", "Farm", "Forestry", "Mine")  # choose variable (LCV, Farm, Forestry, or Mine)
titletext <- c("LCV score","Farming","Forestry","Mining")
xlabtext <- c("Mean score", "Percent workforce", "Percent workforce", "Percent workforce")
attach(PA_zonal.df)
par(mfrow=c(2,2))
for(i in 1:length(varnames)) {
  varname <- varnames[i]
  field.names <- c("UnitName", paste0(varname,".mean.val.", c("10000","20000","50000","1e.05","250000")))    # list of variable names for means
  subdata <- PA_zonal.df[,field.names]
  library(reshape)
  meltdata <- melt(subdata, id="UnitName")
  densplot <- ggplot() +   
    geom_density(data=meltdata, aes(x=value, y=..scaled.., fill=variable, color=variable), alpha=0.25, size=1) + 
    labs(x=xlabtext[i], y="Scaled density") +
    ggtitle(titletext[i]) +
    scale_fill_discrete(name="Buffer\ndistance",
                        breaks=field.names[-1],
                        labels=c("10 km", "20 km", "50 km", "100 km", "250 km")) +
    scale_color_discrete(name="Buffer\ndistance",
                         breaks=field.names[-1],
                         labels=c("10 km", "20 km", "50 km", "100 km", "250 km"))
  assign(paste0("plot",i),densplot)
}
multiplot(plot1, plot2, plot3, plot4, cols=2)



#########################################################################################################################
### SUMMARY STATISTICS FOR EACH VARIABLE AND DESIGNATION CATEGORY
#########################################################################################################################

### Generate table of min, median, and max values of distributions for three designation categories plus NMs under review
var.list <- c("cropped_area_ac","mean.rich.bird","mean.rich.mammal", "mean.rich.fish", "mean.rich.amphib","mean.rich.reptile","mean.rich.tree","mean.rich.natserv","system.richness.rare","mean.climate", "LCV.mean.val.10000","Farm.mean.val.10000","Forestry.mean.val.10000","Mine.mean.val.10000")
min.mat <- matrix(NA, nrow=length(var.list), ncol=4)
median.mat <- matrix(NA, nrow=length(var.list), ncol=4)
max.mat <- matrix(NA, nrow=length(var.list), ncol=4)
#desmode.list <- c("Congress","President","President then Congress")   
attach(PA_zonal.df)
for(i in 1:length(var.list)){
  min.mat[i,1] <- min(get(var.list[i])[which(DesMode=="Congress")], na.rm=TRUE)
  min.mat[i,2] <- min(get(var.list[i])[which(DesMode=="President then Congress")], na.rm=TRUE)
  min.mat[i,3] <- min(get(var.list[i])[which(DesMode=="President")], na.rm=TRUE)
  min.mat[i,4] <- min(get(var.list[i])[which(InReview=="Yes")], na.rm=TRUE)
  median.mat[i,1] <- median(get(var.list[i])[which(DesMode=="Congress")], na.rm=TRUE)
  median.mat[i,2] <- median(get(var.list[i])[which(DesMode=="President then Congress")], na.rm=TRUE)
  median.mat[i,3] <- median(get(var.list[i])[which(DesMode=="President")], na.rm=TRUE)
  median.mat[i,4] <- median(get(var.list[i])[which(InReview=="Yes")], na.rm=TRUE)
  max.mat[i,1] <- max(get(var.list[i])[which(DesMode=="Congress")], na.rm=TRUE)
  max.mat[i,2] <- max(get(var.list[i])[which(DesMode=="President then Congress")], na.rm=TRUE)
  max.mat[i,3] <- max(get(var.list[i])[which(DesMode=="President")], na.rm=TRUE)
  max.mat[i,4] <- max(get(var.list[i])[which(InReview=="Yes")], na.rm=TRUE)  
}
detach(PA_zonal.df)

output.df <- data.frame()
for(i in 1:nrow(min.mat)){
  output.df <- rbind(output.df,min.mat[i,],median.mat[i,],max.mat[i,])
}
colnames(output.df) <- c("Congress","President then Congress","President","Monument under review")
row.names <- c()
for(j in 1:length(var.list)){
  row.names <- c(row.names, paste0(c("min.","median.","max."),var.list[j]))
}
rownames(output.df) <- row.names
output.df.round <- round(output.df, 2)
write.csv(output.df.round, "C:/Users/Tyler/Google Drive/MonumentData/Generated Data/summary_stats.csv")


# generate summary table for monuments under review
subdater <- PA_zonal.df[which(PA_zonal.df$InReview=="Yes"),c("UnitName",var.list)]















####################################################################################
####################################################################################
####################################################################################



####################################################################################
### EXTRAS
####################################################################################

# Histogram of # PAs designated per year by Congress vs. President
ggplot() +
  geom_histogram(data=PA, aes(x = CurDesYear, fill=CurDesAuth), binwidth=1)
# Frequency plot (like histogram, but with lines instead of bars)
ggplot() +
  geom_freqpoly(data=PA, aes(x = CurDesYear, color=CurDesAuth), binwidth=1)
# Histogram with faceting
ggplot() +
  geom_histogram(data=PA, aes(x = CurDesYear, fill=CurDesAuth), binwidth=1) +
  facet_wrap(~CurDesAuth, ncol=1)

# boxplots of acreage of CPAs versus PNMs
ggplot() +
  geom_boxplot(data=PA, aes(x=CurDesAuth, y=area_ac), fill=c("red","green"), width=0.2) +
  scale_x_discrete(name="Designating authority") +
  scale_y_continuous(name="Protected area acreage") +
  theme_bw()
# violin plot (similar to boxplot)
ggplot() +
  geom_violin(data=PA, aes(x=CurDesAuth, y=area_ac)) +
  scale_x_discrete(name="Designating authority") +
  scale_y_continuous(name="Protected area acreage") +
  theme_bw()
# scatterplots of establishment year vs PA area, with faceting by designating authority
ggplot(data=PA, aes(x=CurDesYear, y=area_ac)) +
  geom_point() +
  facet_wrap(~CurDesAuth)


### Map of Bailey's divisions with PAs overlaid

tol21rainbow= c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")
ggplot() +
  geom_sf(data=bailey, aes(fill=DIVISION), color=NA, alpha=0.75) +
  scale_fill_manual("Bailey Division", values=tol21rainbow) + 
  geom_sf(data=PA, fill="black", color=NA)
# same thing, but only NMs overlaid
PA.nm <- filter(PA, CurDesType=="National Monument")
ggplot() +
  geom_sf(data=bailey, aes(fill=DIVISION), color=NA, alpha=0.75) +
  scale_fill_manual("Bailey Division", values=tol21rainbow) + 
  geom_sf(data=PA.nm, fill="black", color=NA)
# distinguish between PNMs and CPAs
PA.pnm <- filter(PA, CurDesAuth=="President")
PA.other <- filter(PA, CurDesAuth!="President")
ggplot() +
  geom_sf(data=bailey, aes(fill=DIVISION), color=NA, alpha=0.75) +
  scale_fill_manual("Bailey Division", values=tol21rainbow) + 
  geom_sf(data=PA.pnm, fill="black", color=NA) +
  geom_sf(data=PA.other, fill="grey40", color=NA)


# Congressional versus presidential PAs by Bailey's division
ggplot() +
  geom_bar(data=PA_zonal.df, aes(x = bailey.majority, fill=CurDesAuth), position="dodge") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1))
# now, just for national monuments
PA.nm <- filter(PA_zonal.df, CurDesType=="National Monument")
ggplot() +
  geom_bar(data=PA.nm, aes(x = bailey.majority, fill=CurDesAuth)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1))


### Boxplots of ecological variables

# mean species richness plots
p1 <- ggplot() +
  geom_boxplot(data=PA_zonal.df, aes(x=DesMode, y=mean.rich.mammal, fill=DesMode), width=0.5) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  ggtitle("Mammals") +
  labs(y="Mean species richness", x=NULL)
p2 <- ggplot() +
  geom_boxplot(data=PA_zonal.df, aes(x=DesMode, y=mean.rich.bird, fill=DesMode), width=0.5) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  ggtitle("Birds") +
  labs(y=NULL, x=NULL)
p3 <- ggplot() +
  geom_boxplot(data=PA_zonal.df, aes(x=DesMode, y=mean.rich.reptile, fill=DesMode), width=0.5) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  ggtitle("Reptiles") +
  labs(y=NULL, x=NULL) 
p4 <- ggplot() +
  geom_boxplot(data=PA_zonal.df, aes(x=DesMode, y=mean.rich.amphib, fill=DesMode), width=0.5) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  ggtitle("Amphibians") +
  labs(y=NULL, x=NULL)
p5 <- ggplot() +
  geom_boxplot(data=PA_zonal.df, aes(x=DesMode, y=mean.rich.fish, fill=DesMode), width=0.5) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  ggtitle("Fish") +
  labs(y=NULL, x=NULL)
p6 <- ggplot() +
  geom_boxplot(data=PA_zonal.df, aes(x=DesMode, y=mean.rich.tree, fill=DesMode), width=0.5) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  ggtitle("Trees") +
  labs(y=NULL, x=NULL)
# maximum species richness plots
p7 <- ggplot() +
  geom_boxplot(data=PA_zonal.df, aes(x=DesMode, y=max.rich.mammal, fill=DesMode), width=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1), legend.position="none", 
        axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  labs(y="Maximum species richness")
p8 <- ggplot() +
  geom_boxplot(data=PA_zonal.df, aes(x=DesMode, y=max.rich.bird, fill=DesMode), width=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1), legend.position="none", 
        axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  labs(y=NULL)
p9 <- ggplot() +
  geom_boxplot(data=PA_zonal.df, aes(x=DesMode, y=max.rich.reptile, fill=DesMode), width=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1), legend.position="none", 
        axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  labs(y=NULL) 
p10 <- ggplot() +
  geom_boxplot(data=PA_zonal.df, aes(x=DesMode, y=max.rich.amphib, fill=DesMode), width=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1), legend.position="none", 
        axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  labs(y=NULL)
p11 <- ggplot() +
  geom_boxplot(data=PA_zonal.df, aes(x=DesMode, y=max.rich.fish, fill=DesMode), width=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1), legend.position="none", 
        axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  labs(y=NULL)
p12 <- ggplot() +
  geom_boxplot(data=PA_zonal.df, aes(x=DesMode, y=max.rich.tree, fill=DesMode), width=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1), legend.position="none", 
        axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  labs(y=NULL)
multiplot(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, cols=6)   # combine in single plot

### Comparing RARITY WEIGHTED SPECIES RICHNESS (NatureServe) between Presidential NMs, Congressional PAs, and PAs that started as PNMs but were later redesignated by Congress
p13 <- ggplot() +
  geom_boxplot(data=PA_zonal.df, aes(x=DesMode, y=mean.rich.natserv, fill=DesMode), width=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1), legend.position="none", 
        axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  labs(y="Mean rarity-weighted species richness") + 
  ggtitle("Imperiled species richness")

p14 <- ggplot() +
  geom_boxplot(data=PA_zonal.df, aes(x=DesMode, y=max.rich.natserv, fill=DesMode), width=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1), legend.position="none", 
        axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  labs(y="Maximum rarity-weighted species richness") + 
  ggtitle("Imperiled species richness")

### Comparing ECOLOGICAL SYSTEM RICHNESS between Presidential NMs, Congressional PAs, and PAs that started as PNMs but were later redesignated by Congress
p15 <- ggplot() +
  geom_boxplot(data=PA_zonal.df, aes(x=DesMode, y=system.richness.rare, fill=DesMode), width=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1), legend.position="none", 
        axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  labs(y="System richness") + 
  ggtitle("Ecological system richness")


### Comparing BACKWARD CLIMATE VELOCITY between Presidential NMs, Congressional PAs, and PAs that started as PNMs but were later redesignated by Congress
# mean BCV plot
p16 <- ggplot() +
  geom_boxplot(data=PA_zonal.df, aes(x=DesMode, y=mean.climate, fill=DesMode), width=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1), legend.position="none", 
        axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  labs(y="Mean climate refugial potential") +
  ggtitle("Climate refugial potential")
# max BCV plot
p17 <- ggplot() +
  geom_boxplot(data=PA_zonal.df, aes(x=DesMode, y=max.climate, fill=DesMode), width=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1), legend.position="none", 
        axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  labs(y="Maximum climate refugial potential") +
  ggtitle("Climate refugial potential")
multiplot(p16,p17, cols=2)   # combine in single plot

### Plot comparing mean values for ALL ecological variables
multiplot(p1,p2,p3,p4,p5,p6,p13,p15,p16, cols=3)

### Plot comparing max values for ALL ecological variables
multiplot(p7,p8,p9,p10,p11,p12,p14,p15,p17, cols=3)


### Correlation matrices for different buffer distances, with correlations of means above diagonal and correlations of maxes below diagonal

varname <- "Forestry"  # choose variable (LCV, Farm, Forestry, or Mine)

attach(PA_zonal.df)
mean.names <- paste0(varname,".mean.val.", c("10000","20000","50000","1e.05","250000"))    # list of variable names for means
max.names <- paste0(varname,".max.val.", c("10000","20000","50000","1e.05","250000"))   # list of variable names for maxes
cor.mat <- matrix(NA, nrow=5, ncol=5)
for(i in 1:5) {
  for(j in 1:5) {
    if(i>j) {
      cor.mat[i,j] <- cor(get(mean.names[i]), get(mean.names[j]), use="pairwise.complete.obs")
    } else {
      if(i<j) {
        cor.mat[i,j] <- cor(get(max.names[i]), get(max.names[j]), use="pairwise.complete.obs")
      } else {
        cor.mat[i,j] <- sum(is.na(get(mean.names[i])))/nrow(PA_zonal.df)
      }
    }
  }
}
cor.mat <- round(cor.mat, 2)
rownames(cor.mat) <- colnames(cor.mat) <- c("10km","20km","50km","100km","250km")
print(cor.mat)
detach(PA_zonal.df)


### Plots showing how percentage of NA values changes with buffer size
inreview.df <- PA_zonal.df %>%  # get values just for those NMs that were part of Trump's review
  filter(InReview=="Yes")

varnames <- c("LCV", "Farm", "Forestry", "Mine")
propNA.mat <- matrix(NA, nrow=length(varnames), ncol=5)  # blank matrix to hold proportion NA for each variable and buffer distance
attach(PA_zonal.df)  
#attach(inreview.df) # if you want to look at how proportion of PNMs under review with NA data changes with buffer distance
for(i in 1:length(varnames)){
  mean.names <- paste0(varnames[i],".mean.val.", c("10000","20000","50000","1e.05","250000"))    # list of variable names for means
  for(j in 1:5){
    propNA.mat[i,j] <- sum(is.na(get(mean.names[j])))/length(get(mean.names[j]))
  }
}
detach(PA_zonal.df)  
#detach(inreview.df)
# convert to dataframe with columns for bufferdist and each soc variable
propNA.df <- data.frame(bufferDist=c(10000,20000,50000,100000,250000), LCVscore=propNA.mat[1,], farming=propNA.mat[2,], forestry=propNA.mat[3,], mining=propNA.mat[4,], stringsAsFactors=FALSE)
propNA.gather <- gather(data=propNA.df, key=socVar, value=propNA, LCVscore:mining)
ggplot(data=propNA.gather, aes(x=bufferDist/1000, y=propNA, color=socVar)) +
  geom_line() +
  geom_point() +
  scale_color_discrete(name="Social variable",
                       breaks=c("LCVscore", "farming", "forestry", "mining"),
                       labels=c("LCV score", "% farming", "% forestry", "% mining")) +
  labs(x="Buffer distance (km)", y="Proportion missing data")



### Plots showing how distributions of means and maxes changes as a function of buffer distance

varname <- "LCV"  # choose variable (LCV, Farm, Forestry, Mine)

mean.varcols <- grep(paste0(varname,".mean"), names(PA_zonal.df))  # column indices for mean values of selected variable
max.varcols <- grep(paste0(varname,".max"), names(PA_zonal.df))  # # column indices for mean values of selected variable
mean.tidy <- PA_zonal.df %>%   # long-form version of dataframe for mean values
  gather(bufferKm, meanVal, mean.varcols, factor_key=TRUE)
max.tidy <- PA_zonal.df %>%   # long-form version of dataframe for mean values
  gather(bufferKm, maxVal, max.varcols, factor_key=TRUE)

mean.buffer.plot <- ggplot() +  # boxplot of mean values as a function of buffer distance
  geom_boxplot(data=mean.tidy, aes(x=bufferKm, y=meanVal, fill=DesMode), width=0.5) +
  scale_x_discrete(labels=c("10","20","50","100","250")) +
  labs(y=paste0("Mean ", varname), x="")

max.buffer.plot <- ggplot() +    # boxplot of maximum values as a function of buffer distance
  geom_boxplot(data=max.tidy, aes(x=bufferKm, y=maxVal, fill=DesMode), width=0.5) +
  scale_x_discrete(labels=c("10","20","50","100","250")) +
  labs(y=paste0("Maximum ", varname), x="Buffer distance (km)")

multiplot(mean.buffer.plot, max.buffer.plot, cols=1)   # combine in single plot


# TABULAR DATA SUMMARIES

### Summary table of PA size and count by designation type
DesType.summary <- PA_zonal.df %>%
  group_by(CurDesType) %>%
  summarise(total.acres = sum(area_ac), 
            mean.acres = mean(area_ac),
            mean.acres = mean(area_ac),
            median.acres = median(area_ac),
            max.acres = max(area_ac),
            count = length(CurDesType))

### Summary table of PA size and count by designation mode
DesMode.summary <- PA_zonal.df %>%
  group_by(DesMode) %>%
  summarise(total.acres = sum(area_ac), 
            mean.acres = mean(area_ac),
            mean.acres = mean(area_ac),
            median.acres = median(area_ac),
            max.acres = max(area_ac),
            count = length(DesMode))
# Note that it doesn't make much sense to include minimum PA size, since we artificially capped at 5,000 acres


### Calculate the quantile value for each PA with respect to each variable
attach(PA_zonal.df)
qtl.vars <- names(PA_zonal.df)[13:71]   # get names of quantitative variables for which we want to calculate quantile values
for(i in 1:length(qtl.vars)) {  # loop through quantitative variables
  qtls <- rank(get(qtl.vars[i]), na.last="keep")/sum(!is.na(get(qtl.vars[i])))
  assign(paste0(qtl.vars[i],".qtl"), qtls)
}
detach(PA_zonal.df)

# Look at percentile values for NMs under review
var.name <- "Forestry.mean.val.10000.qtl"
reviewNM <- which(PA_zonal.df$InReview=="Yes")
NM.qtls <- data.frame(UnitName = PA_zonal.df$UnitName[reviewNM], Quantile = get(var.name)[reviewNM], stringsAsFactors=FALSE)
NM.qtls[order(NM.qtls$Quantile, decreasing=TRUE),]
PA.qtls <- data.frame(UnitName = PA_zonal.df$UnitName, Quantile = get(var.name), stringsAsFactors=FALSE)
head(PA.qtls[order(PA.qtls$Quantile, decreasing=TRUE),])





