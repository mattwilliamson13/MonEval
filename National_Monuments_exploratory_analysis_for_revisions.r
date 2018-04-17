# EXPLORATORY ANALYSIS FOR REVISED MANUSCRIPT


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

data <- PA_zonal.df
NMdata <- data[which(data$CurDesType=="National Monument"),]
post2000data <- data[which(data$CurDesYear>=2000),]
