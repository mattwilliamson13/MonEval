require(sf)
require(dplyr)
require(stringr)

infolder <- "C:/Users/Tyler/Google Drive/MonumentData/"  # set folder holding input data
PA <- st_read(paste(infolder,"Revised_Federal_PAs_9-20-17_repaired.shp", sep=""), stringsAsFactors=FALSE)
PA$DesMode <- as.character(PA$CurDesAuth)  # have to use class character (instead of default factor) to manually edit values in next step
PA$DesMode[which(PA$CurDesAuth=="Congress" & PA$OriDesAuth=="President")] <- "President then Congress"
PA$DesMode <- as.factor(PA$DesMode)  # convert back to factor
PA <- PA %>%  # get rid of PAs smaller than 5,000 acres (minimum for wilderness designation)
  mutate(area_m2 = as.numeric(st_area(geometry))) %>%
  mutate(area_ac = as.numeric(area_m2/4046.86))


NM <- PA %>%
  filter(DesMode=="President" | DesMode=="President then Congress")

NM$AntiqYear[7] <- 2016

NM <- NM %>%
  mutate(year = pmin(as.numeric(AntiqYear), as.numeric(CurDesYear), na.rm=TRUE))

NM <- NM %>%   # get rid of marine monuments if desired
  filter(str_detect(UnitName, "MARINE")==FALSE)

years <- c(min(NM$year):max(NM$year))
cum.acres <- c(NA, length(years))
annual.acres <- c(NA, length(years))
mean.acres <- c(NA, length(years))
for(i in 1:length(years)){
  cum.acres[i] <- sum(NM$area_m2[which(NM$year<=years[i])])
  annual.acres[i] <- sum(NM$area_m2[which(NM$year==years[i])])
  mean.acres[i] <- mean(NM$area_m2[which(NM$year==years[i])])
}

# plot cumulative acreage of PPAs through time
plot(cum.acres ~ years, type="l", xlab="Year", ylab="Cumulative acreage of PPAs")

# plot number of PPAs designated by year
truehist(NM$year, h=1, prob=FALSE, xlab="year", ylab="Number of PPAs designated per year")


# plot total acreage of PPAs designated by year
plot(annual.acres ~ years, xlab="year", ylab="Acres of PPAs designated")

# plot mean acreage of PPAs designated in each year
plot(mean.acres ~ years, xlab="year", ylab="Average PPA size")


