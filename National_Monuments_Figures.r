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
load(paste(infolder,"/PA_zonal_stats_10-22-17.RData", sep=""))

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
PA <- st_read(paste(infolder, "/PA_revised_9-21-17.shp", sep=""))  # use this version of the PA shapefile that has duplicate Unit Names corrected - maps will be incorrect otherwise
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

# Create a new Designation Mode variable to distinguish PAs that started as presidential NMs but are now congressional designations (call this variable DesMode)
PA_zonal.df$DesMode <- PA_zonal.df$CurDesAuth
PA_zonal.df$DesMode[which(PA_zonal.df$CurDesAuth=="Congress" & PA_zonal.df$OriDesAuth=="President")] <- "President then Congress"
PA$DesMode <- as.character(PA$CurDesAuth)  # have to use class character (instead of default factor) to manually edit values in next step
PA$DesMode[which(PA$CurDesAuth=="Congress" & PA$OriDesAuth=="President")] <- "President then Congress"
PA$DesMode <- as.factor(PA$DesMode)  # convert back to factor

# From SPATIAL layer, remove two NMs that were designated by inter-agency agreement
  # and six coastal PAs that are <5000 acres after cropping out non-mainland portion
PA <- PA %>%  # get rid of PAs smaller than 5,000 acres (minimum for wilderness designation)
  mutate(cropped_area_m2 = as.numeric(st_area(geometry))) %>%
  mutate(cropped_area_ac = as.numeric(cropped_area_m2/4046.86)) %>%
  filter(CurDesAuth %in% c("Congress","President")) %>%
  filter(cropped_area_ac >= 5000)


# From NONSPATIAL zonal stats layer, remove two NMs that were designated by inter-agency agreement
PA_zonal.df <- filter(PA_zonal.df, CurDesAuth %in% c("Congress","President"))
PA_zonal.df <- filter(PA_zonal.df, cropped_area_ac >= 5000)

# calculate percent NA for each field in the dataset
#sapply(PA_zonal.df, function(x) round(sum(is.na(x))/length(x)*100,1))

# create new dataframe that only includes PNMs under review by Trump administration
inreview.df <- PA_zonal.df %>%  # get values just for those NMs that were part of Trump's review
  filter(InReview=="Yes")



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
  geom_density(data=PA_zonal.df, aes(x=log(area_ac), y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Log PA size (acres)", y="Scaled density") +
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

ecol.means.mat <- matrix(NA, nrow=3, ncol=9)  # get mean values for each variable for each designation class
desmode.list <- c("Congress","President","President then Congress")   
var.list <- c("mean.rich.bird","mean.rich.mammal", "mean.rich.fish", "mean.rich.amphib","mean.rich.reptile","mean.rich.tree","mean.rich.natserv","system.richness.rare","mean.climate")
attach(PA_zonal.df)
for(i in 1:3){
  for(j in 1:9){
    ecol.means.mat[i,j] <- mean(get(var.list[j])[which(DesMode==desmode.list[i])], na.rm=TRUE)
  }
}
detach(PA_zonal.df)
# bird richness
r1 <- ggplot() +   
  geom_density(data=PA_zonal.df, aes(x=mean.rich.bird, y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Mean richness", y="Scaled density") +
  ggtitle("Bird richness") +
  scale_color_discrete(name="Designation\nmode") +
  scale_fill_discrete(name="Designation\nmode") +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  guides(fill=FALSE, color=FALSE) # suppress legend
# mammal richness
r2 <- ggplot() +    
  geom_density(data=PA_zonal.df, aes(x=mean.rich.mammal, y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Mean richness", y="Scaled density") +
  ggtitle("Mammal richness") +
  scale_color_discrete(name="Designation\nmode") +
  scale_fill_discrete(name="Designation\nmode") +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  guides(fill=FALSE, color=FALSE) # suppress legend
# fish richness
r3 <- ggplot() +    
  geom_density(data=PA_zonal.df, aes(x=mean.rich.fish, y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Mean richness", y="Scaled density") +
  ggtitle("Fish richness") +
  #scale_color_discrete(name="Designation\nmode") +
  #scale_fill_discrete(name="Designation\nmode") +
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
# amphibian richness
r4 <- ggplot() +
  geom_density(data=PA_zonal.df, aes(x=mean.rich.amphib, y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Mean richness", y="Scaled density") +
  ggtitle("Amphibian richness") +
  scale_color_discrete(name="Designation\nmode") +
  scale_fill_discrete(name="Designation\nmode") +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  guides(fill=FALSE, color=FALSE) # suppress legend
# reptile richness
r5 <- ggplot() +
  geom_density(data=PA_zonal.df, aes(x=mean.rich.reptile, y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Mean richness", y="Scaled density") +
  ggtitle("Reptile richness") +
  scale_color_discrete(name="Designation\nmode") +
  scale_fill_discrete(name="Designation\nmode") +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  guides(fill=FALSE, color=FALSE) # suppress legend
# tree richness
r6 <- ggplot() +
  geom_density(data=PA_zonal.df, aes(mean.rich.tree, y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Mean richness", y="Scaled density") +
  ggtitle("Tree richness") +
  scale_color_discrete(name="Designation\nmode") +
  scale_fill_discrete(name="Designation\nmode") +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  guides(fill=FALSE, color=FALSE) # suppress legend
# G1/G2 richness
r7 <- ggplot() +
  geom_density(data=PA_zonal.df, aes(mean.rich.natserv, y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Mean rarity-weighted richness", y="Scaled density") +
  ggtitle("G1 & G2 species richness") +
  scale_color_discrete(name="Designation\nmode") +
  scale_fill_discrete(name="Designation\nmode") +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  guides(fill=FALSE, color=FALSE) # suppress legend
# ecological system richness
r8 <- ggplot() +
  geom_density(data=PA_zonal.df, aes(system.richness.rare, y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Rarefied richness", y="Scaled density") +
  ggtitle("Ecological system richness") +
  scale_color_discrete(name="Designation\nmode") +
  scale_fill_discrete(name="Designation\nmode") +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  guides(fill=FALSE, color=FALSE) # suppress legend
# climate refugial potential
r9 <- ggplot() +
  geom_density(data=PA_zonal.df, aes(mean.climate, y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Mean refugial potential", y="Scaled density") +
  ggtitle("Climate refugial potential") +
  scale_color_discrete(name="Designation\nmode") +
  scale_fill_discrete(name="Designation\nmode") +
  theme(plot.title = element_text(size=11, face="bold")) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  guides(fill=FALSE, color=FALSE) # suppress legend
  

multiplot(r1,r2,r3,r4,r5,r6,r7,r8,r9, cols=3)



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





