# GRAPHICAL ANALYSIS OF BIOLOGICAL VARIABLES


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
library(gridExtra)
library(diveRsity)

infolder <- "C:/Users/Tyler/Google Drive/MonumentData/Generated Data"  # set folder holding input data

# load in dataframe with output variables for PAs
load(paste(infolder,"/PA_zonal_stats3.RData", sep=""))

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

# drop variables that are no longer needed or inaccurate
drops <- c("OrigAntiq", "AntiqYear", "GapStatus", "IucnCat", "OrigDesigAuth")
PA.df <- PA.df[ , !(names(PA.df) %in% drops)]

# Create a new Designation Mode variable to distinguish PAs that started as presidential NMs but are now congressional designations (call this variable DesigMode)
select.units <- c("Petrified Forest National Park", "Lassen National Park", "Grand Canyon National Park", "Pinnacles National Park", "Olympic National Park", 
                  "Zion National Park", "Acadia National Park", "Great Basin National Park", "Bryce Canyon National Park", "Carlsbad Caverns National Park", 
                  "Arches National Park", "Great Sand Dunes National Park", "Death Valley National Park", "Saguaro National Park", 
                  "Black Canyon of the Gunnison National Park", "Dry Tortugas National Park", "Joshua Tree National Park", "Capitol Reef National Park", 
                  "Channel Islands National Park", "Gulf Islands National Seashore", "Grand Teton National Park")
PA.df$DesigMode <- PA.df$DesigAuth
PA.df$DesigMode[which(PA.df$UnitName %in% select.units)] <- "President then Congress"




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
### MAP OF PROTECTED AREAS COLOR CODED BY DESIGNATION TYPE

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
  geom_sf(data=bailey, aes(fill=DIVISION), color=NA, alpha=0.75) +
  scale_fill_manual("Bailey Division", values=tol21rainbow) + 
  geom_sf(data=PA, fill="black", color=NA)

# same thing, but only NMs overlaid
PA.nm <- filter(PA, DesigType=="National Monument")

ggplot() +
  geom_sf(data=bailey, aes(fill=DIVISION), color=NA, alpha=0.75) +
  scale_fill_manual("Bailey Division", values=tol21rainbow) + 
  geom_sf(data=PA.nm, fill="black", color=NA)
  
# distinguish between PNMs and CPAs
PA.pnm <- filter(PA, DesigAuth=="President")
PA.other <- filter(PA, DesigAuth!="President")
ggplot() +
  geom_sf(data=bailey, aes(fill=DIVISION), color=NA, alpha=0.75) +
  scale_fill_manual("Bailey Division", values=tol21rainbow) + 
  geom_sf(data=PA.pnm, fill="black", color=NA) +
  geom_sf(data=PA.other, fill="grey40", color=NA)



#############################################################
# Boxplots comparing SPECIES RICHNESS between Presidential NMs, Congressional PAs, and PAs that started as PNMs but were later redesignated by Congress

# mean species richness plots
p1 <- ggplot() +
  geom_boxplot(data=PA.df, aes(x=DesigMode, y=mean.rich.mammal, fill=DesigMode), width=0.5) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  ggtitle("Mammals") +
  labs(y="Mean species richness", x=NULL)
p2 <- ggplot() +
  geom_boxplot(data=PA.df, aes(x=DesigMode, y=mean.rich.bird, fill=DesigMode), width=0.5) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  ggtitle("Birds") +
  labs(y=NULL, x=NULL)
p3 <- ggplot() +
  geom_boxplot(data=PA.df, aes(x=DesigMode, y=mean.rich.reptile, fill=DesigMode), width=0.5) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  ggtitle("Reptiles") +
  labs(y=NULL, x=NULL) 
p4 <- ggplot() +
  geom_boxplot(data=PA.df, aes(x=DesigMode, y=mean.rich.amphib, fill=DesigMode), width=0.5) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  ggtitle("Amphibians") +
  labs(y=NULL, x=NULL)
p5 <- ggplot() +
  geom_boxplot(data=PA.df, aes(x=DesigMode, y=mean.rich.fish, fill=DesigMode), width=0.5) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  ggtitle("Fish") +
  labs(y=NULL, x=NULL)
p6 <- ggplot() +
  geom_boxplot(data=PA.df, aes(x=DesigMode, y=mean.rich.tree, fill=DesigMode), width=0.5) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  ggtitle("Trees") +
  labs(y=NULL, x=NULL)
# maximum species richness plots
p7 <- ggplot() +
  geom_boxplot(data=PA.df, aes(x=DesigMode, y=max.rich.mammal, fill=DesigMode), width=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1), legend.position="none", 
        axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  labs(y="Maximum species richness")
p8 <- ggplot() +
  geom_boxplot(data=PA.df, aes(x=DesigMode, y=max.rich.bird, fill=DesigMode), width=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1), legend.position="none", 
        axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  labs(y=NULL)
p9 <- ggplot() +
  geom_boxplot(data=PA.df, aes(x=DesigMode, y=max.rich.reptile, fill=DesigMode), width=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1), legend.position="none", 
        axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  labs(y=NULL) 
p10 <- ggplot() +
  geom_boxplot(data=PA.df, aes(x=DesigMode, y=max.rich.amphib, fill=DesigMode), width=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1), legend.position="none", 
        axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  labs(y=NULL)
p11 <- ggplot() +
  geom_boxplot(data=PA.df, aes(x=DesigMode, y=max.rich.fish, fill=DesigMode), width=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1), legend.position="none", 
        axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  labs(y=NULL)
p12 <- ggplot() +
  geom_boxplot(data=PA.df, aes(x=DesigMode, y=max.rich.tree, fill=DesigMode), width=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1), legend.position="none", 
        axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  labs(y=NULL)
multiplot(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, cols=6)   # combine in single plot



#############################################################
# Boxplots comparing ECOLOGICAL SYSTEM RICHNESS between Presidential NMs, Congressional PAs, and PAs that started as PNMs but were later redesignated by Congress

# Probably none of these metrics is really an appropriate measure. 
# Raw richness does not account for differences in area (i.e., sampling effort) between PAs
# Area weighted overcorrects because species-area curve is asymptotic, not linear
# Rarity weighted might be OK, but still needs to adjust for area affect correctly
# One option might be to use rarefaction approach:
  # 1. For each PA, get system value for each cell within boundaries
  # 2. Randomly sample n cells (where n= # cells within smallest PA)
  # 3. Calculate richness in random sample (i.e., # of unique system values)
  # 4. Resample 1000 times and calculate average richness across samples for each PA

# raw system richness
p13 <- ggplot() +
  geom_boxplot(data=PA.df, aes(x=DesigMode, y=system.richness, fill=DesigMode), width=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1), legend.position="none", 
        axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  labs(y="System richness") + 
  ggtitle("Raw")
# area-weighted system richness
p14 <- ggplot() +
  geom_boxplot(data=PA.df, aes(x=DesigMode, y=system.aw.richness, fill=DesigMode), width=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1), legend.position="none", 
        axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  labs(y=NULL) + 
  ggtitle("Area-weighted")
# rarity-weighted system richness
p15 <- ggplot() +
  geom_boxplot(data=PA.df, aes(x=DesigMode, y=system.rw.richness, fill=DesigMode), width=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1), legend.position="none", 
        axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  labs(y=NULL) + 
  ggtitle("Rarity-weighted")
# area- and rarity-weighted system richness
p16 <- ggplot() +
  geom_boxplot(data=PA.df, aes(x=DesigMode, y=system.rw.aw.richness, fill=DesigMode), width=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1), legend.position="none", 
        axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  labs(y=NULL) + 
  ggtitle("Area- and rarity-weighted")
multiplot(p13,p14,p15,p16, cols=4)   # combine in single plot



#############################################################
# Boxplots comparing BACKWARD CLIMATE VELOCITY between Presidential NMs, Congressional PAs, and PAs that started as PNMs but were later redesignated by Congress

# mean BCV plot
p17 <- ggplot() +
  geom_boxplot(data=PA.df, aes(x=DesigMode, y=mean.climate, fill=DesigMode), width=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1), legend.position="none", 
        axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  labs(y="Mean backward climate velocity")
# max BCV plot
p18 <- ggplot() +
  geom_boxplot(data=PA.df, aes(x=DesigMode, y=max.climate, fill=DesigMode), width=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1), legend.position="none", 
        axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  labs(y="Maximum backward climate velocity")
multiplot(p17,p18, cols=2)   # combine in single plot



###########################################################
# 2-way ANOVAs test whether envi. response variable depends on Designating Authority (president vs. congress) and if there is an interaction with Bailey's division

response <- "mean.rich.amphib"   # name of response variable you want to test
lm1 <- aov(get(response) ~ as.factor(DesigAuth) * as.factor(bailey.majority), data=PA.df)   # run two-way ANOVA
res <- lm1$residuals  # extract residuals to check for normality
hist(res,main="Histogram of residuals",xlab="Residuals")
library(car)
leveneTest(get(response) ~ as.factor(DesigAuth) * as.factor(bailey.majority), data=PA.df)  # test for equal variances
summary(lm1)

# NOTE THAT ANOVA IS NOT APPROPRIATE FOR MOST OF THESE TESTS BECAUSE OF VIOLATION OF EQUAL VARIANCES ASSUMPTION



