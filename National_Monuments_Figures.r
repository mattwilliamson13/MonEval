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
#infolder <- "D:/Data/MonumentData/Generated Data"  # location on Schwartz server

# load in dataframe with biological output variables for PAs (object called PA_zonal.df)
load(paste(infolder,"/PA_zonal_stats_10-4-17.RData", sep=""))

# load in csv of social output variables 
socData <- read.csv(paste(infolder,"/socData_10_20_17.csv", sep=""))[,-c(2:9)]  # exclude variables already in PA_zonal.df
socData$UnitName <- as.character(socData$UnitName)

# fix weird character in Rio Grande Del Norte NM (accented i screws up the merge)
socData$UnitName[590] <- "Rio Grande Del Norte National Monument"
PA_zonal.df$UnitName[590] <- "Rio Grande Del Norte National Monument"

# merge social and biological output variables into single dataframe
PA_zonal.df <- merge(PA_zonal.df, socData, by="UnitName")

# load spatial data
PA <- st_read(paste(infolder, "/PA_revised_9-21-17.shp", sep=""))  # use this version of the PA shapefile that has duplicate Unit Names corrected - maps will be incorrect otherwise
bailey <- st_read(paste(infolder, "/baileycor.shp", sep=""))
rich.bird <- raster(paste(infolder, "/rich.bird.tif", sep=""))
rich.mammal <- raster(paste(infolder, "/rich.mammal.tif", sep=""))
rich.tree <- raster(paste(infolder, "/rich.tree.tif", sep=""))
rich.reptile <- raster(paste(infolder, "/rich.reptile.tif", sep=""))
rich.natserv <- raster(paste(infolder, "/natserv.tif", sep=""))
rich.fish <- st_read(paste(infolder, "/rich.fish.shp", sep=""))
rich.amphib <- st_read(paste(infolder, "/rich.amphib.shp", sep=""))
natlandcover <- raster(paste(infolder, "/natlandcover.tif", sep=""))
climate <- raster(paste(infolder, "/climate.tif", sep=""))
fedlands <- st_read(paste(infolder, "/fedlandscrop.shp", sep=""))
states <- st_read(paste(infolder, "/states2.shp", sep=""))

# Create a new Designation Mode variable to distinguish PAs that started as presidential NMs but are now congressional designations (call this variable DesMode)
PA_zonal.df$DesMode <- PA_zonal.df$CurDesAuth
PA_zonal.df$DesMode[which(PA_zonal.df$CurDesAuth=="Congress" & PA_zonal.df$OriDesAuth=="President")] <- "President then Congress"
PA$DesMode <- as.character(PA$CurDesAuth)  # have to use class character (instead of default factor) to manually edit values in next step
PA$DesMode[which(PA$CurDesAuth=="Congress" & PA$OriDesAuth=="President")] <- "President then Congress"
PA$DesMode <- as.factor(PA$DesMode)  # convert back to factor

# From SPATIAL layer, remove two NMs that were designated by inter-agency agreement
PA <- filter(PA, CurDesAuth %in% c("Congress","President"))

# From NONSPATIAL zonal stats layer, remove two NMs that were designated by inter-agency agreement
PA_zonal.df <- filter(PA_zonal.df, CurDesAuth %in% c("Congress","President"))




####################################################################################
### DATA EXPLORATION
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
# density plots of 
ggplot(data=PA, aes(x=area_ac, fill=DesMode)) +
  geom_density(alpha=0.25)



####################################################################################
### MAPS 
####################################################################################

### Map of protected areas coded by designation type

# NOTE: can't map a multipolygon layer using geom_sf, so we'll need to go 
  # back and generate a non-unioned fedlands layer if we want to include in the plot
  # the fix for this is fedlands.cast <- st_cast(fedlands, "POLYGON")

fedlands.cast <- st_cast(fedlands, "POLYGON")  # can't map multipolygon layers using geom_sf, so convert fedlands back to polygons layer
  # NOTE: following plot takes a very long time to generate figure in R -
  # either run on cluster or comment out fedlands line when tweaking figure
ggplot() +
  geom_sf(data=states, fill="grey85", color=NA) +  # light grey background for lower 48 states
  geom_sf(data=fedlands.cast, fill="grey70", color=NA) +  # federal lands in darker grey
  geom_sf(data=states, fill=NA, color="white") +  # state outlines in darkest grey
  geom_sf(data=PA, aes(fill=CurDesAuth), color=NA, alpha=0.5) +  # protected areas on top
  ggtitle("Federal protected areas of the contiguous United States") +
  theme_bw() +
  #theme_map() +     # this option removes lat/lon lines
  scale_fill_discrete(name="Designating\nauthority")


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




##########################################################################################
### ECOLOGICAL VARIABLE BOXPLOTS 
##########################################################################################

# Congressional versus presidential PAs by Bailey's division
ggplot() +
  geom_bar(data=PA_zonal.df, aes(x = bailey.majority, fill=CurDesAuth), position="dodge") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1))
# now, just for national monuments
PA.nm <- filter(PA_zonal.df, CurDesType=="National Monument")
ggplot() +
  geom_bar(data=PA.nm, aes(x = bailey.majority, fill=CurDesAuth)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1))


### Comparing SPECIES RICHNESS between Presidential NMs, Congressional PAs, and PAs that started as PNMs but were later redesignated by Congress

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




### Density plot for species richness
inreview.df <- PA_zonal.df %>%  # get values just for those NMs that were part of Trump's review
  filter(InReview=="Yes")

# Note: in geom_segment, will need to manually adjust height of tick marks (yend argument) showing values for NMs under review
# because y-axis range differs among density plots for different variables


r1 <- ggplot() +
  geom_density(data=PA_zonal.df, aes(x=mean.rich.bird, y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Mean richness", y="Scaled density") +
  ggtitle("Birds") +
  geom_segment(data=inreview.df, mapping=aes(x=mean.rich.bird, y=0, xend=mean.rich.bird, yend=0.05), size=0.7, color="black") +
  scale_color_discrete(name="Designating\nauthority") +
  scale_fill_discrete(name="Designating\nauthority") +
  guides(fill=FALSE, color=FALSE) # suppress legend

r2 <- ggplot() +
  geom_density(data=PA_zonal.df, aes(x=mean.rich.mammal, y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Mean richness", y="Scaled density") +
  ggtitle("Mammals") +
  geom_segment(data=inreview.df, mapping=aes(x=mean.rich.mammal, y=0, xend=mean.rich.mammal, yend=0.05), size=0.7, color="black") +
  scale_color_discrete(name="Designating\nauthority") +
  scale_fill_discrete(name="Designating\nauthority") +
  guides(fill=FALSE, color=FALSE) # suppress legend

r3 <- ggplot() +
  geom_density(data=PA_zonal.df, aes(x=mean.rich.fish, y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Mean richness", y="Scaled density") +
  ggtitle("Fish") +
  geom_segment(data=inreview.df, mapping=aes(x=mean.rich.fish, y=0, xend=mean.rich.fish, yend=0.05), size=0.7, color="black") +
  scale_color_discrete(name="Designating\nauthority") +
  scale_fill_discrete(name="Designating\nauthority") +
  theme(legend.justification=c(1,1), legend.position=c(1,1))  # put legend in top right corner

r4 <- ggplot() +
  geom_density(data=PA_zonal.df, aes(x=mean.rich.amphib, y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Mean richness", y="Scaled density") +
  ggtitle("Amphibians") +
  geom_segment(data=inreview.df, mapping=aes(x=mean.rich.amphib, y=0, xend=mean.rich.amphib, yend=0.05), size=0.7, color="black") +
  scale_color_discrete(name="Designating\nauthority") +
  scale_fill_discrete(name="Designating\nauthority") +
  guides(fill=FALSE, color=FALSE) # suppress legend

r5 <- ggplot() +
  geom_density(data=PA_zonal.df, aes(x=mean.rich.reptile, y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Mean richness", y="Scaled density") +
  ggtitle("Reptiles") +
  geom_segment(data=inreview.df, mapping=aes(x=mean.rich.reptile, y=0, xend=mean.rich.reptile, yend=0.05), size=0.7, color="black") +
  scale_color_discrete(name="Designating\nauthority") +
  scale_fill_discrete(name="Designating\nauthority") +
  guides(fill=FALSE, color=FALSE) # suppress legend

r6 <- ggplot() +
  geom_density(data=PA_zonal.df, aes(mean.rich.tree, y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Mean richness", y="Scaled density") +
  ggtitle("Trees") +
  geom_segment(data=inreview.df, mapping=aes(x=mean.rich.tree, y=0, xend=mean.rich.tree, yend=0.05), size=0.7, color="black") +
  scale_color_discrete(name="Designating authority") +
  scale_fill_discrete(name="Designating authority") +
  guides(fill=FALSE, color=FALSE) # suppress legend

multiplot(r1,r2,r3,r4,r5,r6, cols=3)



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

# density plots
rw <- ggplot() +
  geom_density(data=PA_zonal.df, aes(mean.rich.natserv, y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Mean rarity-weighted species richness", y="Scaled density") +
  ggtitle("Imperiled and Critically Imperiled species richness") +
  geom_segment(data=inreview.df, mapping=aes(x=mean.rich.natserv, y=0, xend=mean.rich.natserv, yend=0.05), size=0.7, color="black") +
  scale_color_discrete(name="Designating authority") +
  scale_fill_discrete(name="Designating authority") +
  theme(legend.justification=c(1,1), legend.position=c(1,1))  # put legend in top right corner
rw

### Comparing ECOLOGICAL SYSTEM RICHNESS between Presidential NMs, Congressional PAs, and PAs that started as PNMs but were later redesignated by Congress
p15 <- ggplot() +
  geom_boxplot(data=PA_zonal.df, aes(x=DesMode, y=system.richness.rare, fill=DesMode), width=0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1), legend.position="none", 
        axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill='grey85', colour='black')) +
  scale_fill_grey(start=0, end=1) +
  labs(y="System richness") + 
  ggtitle("Ecological system richness")

# density plot
es <- ggplot() +
  geom_density(data=PA_zonal.df, aes(system.richness.rare, y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Rarefied richness", y="Scaled density") +
  ggtitle("Ecological system richness") +
  geom_segment(data=inreview.df, mapping=aes(x=system.richness.rare, y=0, xend=system.richness.rare, yend=0.05), size=0.7, color="black") +
  scale_color_discrete(name="Designating authority") +
  scale_fill_discrete(name="Designating authority") +
  theme(legend.justification=c(1,1), legend.position=c(1,1))  # put legend in top right corner
es

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

cv <- ggplot() +
  geom_density(data=PA_zonal.df, aes(mean.climate, y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Mean refugial potential", y="Scaled density") +
  ggtitle("Climate refugial potential") +
  geom_segment(data=inreview.df, mapping=aes(x=mean.climate, y=0, xend=mean.climate, yend=0.05), size=0.7, color="black") +
  scale_color_discrete(name="Designating authority") +
  scale_fill_discrete(name="Designating authority") +
  theme(legend.justification=c(1,1), legend.position=c(1,1))  # put legend in top right corner
cv



### Plot comparing mean values for ALL ecological variables
multiplot(p1,p2,p3,p4,p5,p6,p13,p15,p16, cols=3)

### Plot comparing max values for ALL ecological variables
multiplot(p7,p8,p9,p10,p11,p12,p14,p15,p17, cols=3)




##########################################################################################
### SOCIAL VARIABLE DATA EXPLORATION
##########################################################################################


### Correlation matrices for different buffer distances, with correlations of means above diagonal and correlations of maxes below diagonal

varname <- "Farm"  # choose variable (LCV, Farm, Forestry, or Mine)

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



### Density plots of mean values for social variables

inreview.df <- PA_zonal.df %>%  # get values just for those NMs that were part of Trump's review
  filter(InReview=="Yes")

# Note: in geom_segment, will need to manually adjust height of tick marks (yend argument) showing values for NMs under review
  # because y-axis range differs among density plots for different variables

bufdist <- "10000"   # select buffer distance; must be character format, one of these choices: "10000","20000","50000","1e.05","250000")

s1 <- ggplot() +
  geom_density(data=PA_zonal.df, aes(x=get(paste0("LCV.mean.val.",bufdist)), y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Mean LCV score", y="Scaled density") +
  ggtitle("LCV score") +
  geom_segment(data=inreview.df, mapping=aes(x=get(paste0("LCV.mean.val.",bufdist)), y=0, xend=get(paste0("LCV.mean.val.",bufdist)), yend=0.05), size=1, color="black") +
  scale_color_discrete(name="Designating\nauthority") +
  scale_fill_discrete(name="Designating\nauthority") +
  guides(fill=FALSE, color=FALSE) # suppress legend

s2 <- ggplot() +
  geom_density(data=PA_zonal.df, aes(x=get(paste0("Farm.mean.val.",bufdist)), y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Percent workforce", y="Scaled density") +
  ggtitle("Farming sector") +
  geom_segment(data=inreview.df, mapping=aes(x=get(paste0("Farm.mean.val.",bufdist)), y=0, xend=get(paste0("Farm.mean.val.",bufdist)), yend=0.05), size=1, color="black") +
  scale_color_discrete(name="Designating\nauthority") +
  scale_fill_discrete(name="Designating\nauthority") +
  guides(fill=FALSE, color=FALSE) # suppress legend

s3 <- ggplot() +
  geom_density(data=PA_zonal.df, aes(x=get(paste0("Forestry.mean.val.",bufdist)), y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Percent workforce", y="Scaled density") +
  ggtitle("Forestry sector") +
  geom_segment(data=inreview.df, mapping=aes(x=get(paste0("Forestry.mean.val.",bufdist)), y=0, xend=get(paste0("Forestry.mean.val.",bufdist)), yend=0.05), size=1, color="black") +
  scale_color_discrete(name="Designating\nauthority") +
  scale_fill_discrete(name="Designating\nauthority") +
  guides(fill=FALSE, color=FALSE) # suppress legend

s4 <- ggplot() +
  geom_density(data=PA_zonal.df, aes(x=get(paste0("Mine.mean.val.",bufdist)), y=..scaled.., fill=DesMode, color=DesMode), alpha=0.35, size=1) + 
  labs(x="Percent workforce", y="Scaled density") +
  ggtitle("Mining sector") +
  geom_segment(data=inreview.df, mapping=aes(x=get(paste0("Mine.mean.val.",bufdist)), y=0, xend=get(paste0("Mine.mean.val.",bufdist)), yend=0.05), size=1, color="black") +
  scale_color_discrete(name="Designating authority") +
  scale_fill_discrete(name="Designating authority") +
  theme(legend.justification=c(1,1), legend.position=c(1,1))  # put legend in top right corner

multiplot(s1, s2, s3, s4, cols=2)







##########################################################################
# TABULAR DATA SUMMARIES
##########################################################################

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















##############################################################
### STATISTICAL ANALYSES
##############################################################

### 1-way ANOVA to test whether envi. response variable depends on Designating Authority (president vs. congress)
response <- "mean.rich.tree"   # name of response variable you want to test
lm1 <- aov(get(response) ~ as.factor(DesMode), data=PA_zonal.df)   # run ANOVA
res <- lm1$residuals  # extract residuals to check for normality
hist(res, main="Histogram of residuals", xlab="Residuals")   # check for normality
library(car)
leveneTest(get(response) ~ as.factor(DesMode), data=PA_zonal.df)  # test for equal variances (P < 0.05 is evidence of UNequal variances and invalidity of ANOVA)
summary(lm1)
TukeyHSD(lm1, conf.level=0.95)   # Tukey's Honest Significance Test to see which groups are significantly different (in terms of group means)



### 2-way ANOVA to test whether envi. response variable depends on Designating Authority (president vs. congress) 
  # AND if there is an interaction with Bailey's division
  # NOTE THAT STANDARD TWO-WAY ANOVA IS NOT APPROPRIATE FOR MOST OF THESE TESTS BECAUSE OF VIOLATION OF EQUAL VARIANCES ASSUMPTION
response <- "mean.rich.mammal"   # name of response variable you want to test
lm1 <- aov(get(response) ~ as.factor(DesMode) * as.factor(bailey.majority), data=PA_zonal.df)   # run two-way ANOVA
res <- lm1$residuals  # extract residuals to check for normality
hist(res,main="Histogram of residuals",xlab="Residuals")
library(car)
leveneTest(get(response) ~ as.factor(DesMode) * as.factor(bailey.majority), data=PA_zonal.df)  # test for equal variances
summary(lm1)


### Non-parametric one-way ANOVA alternative
# Kruskal Wallis does not assume normality, but does assume that groups have same distribution (except possibly different medians), so doesn't get around the issue of inequality of variances
kruskal.test(mean.climate ~ as.factor(DesMode), data=PA_zonal.df)  # kruskal wallis test (null hypothesis: the groups are from the same distribution; i.e., no difference in response variable among groups)
bartlett.test(mean.rich.bird ~ as.factor(DesMode), data=PA_zonal.df)  # bartlett test of equal variance (null hypothesis: equal variances)





