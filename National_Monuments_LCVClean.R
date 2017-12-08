library(tidycensus)
library(forcats)
library(purrr)
library(sf)
library(tidyverse)

options(tigris_use_cache = TRUE)
census_api_key('baaaffb5ed3accd8dfa53c6f827659d43fcdfa21') #get this from the census api webpage see help(census_api_key) for details

us.states <- unique(fips_codes$state)[1:51] 
continental.states <- us.states[!us.states %in% c("AK", "HI")]

pop_cty <- reduce(
  map(continental.states, function(x) {
    get_decennial(geography = "county", variables = "P0010001", 
                  state = x, summary_var = "P0030001", geometry=TRUE)
  }), rbind)


LCV_sum <- read.csv("D:/Data/ConsOppMap/LCV/LCV_summary.csv", colClasses = "character", stringsAsFactors = FALSE)


LCV_sum[,3:8] <- apply(LCV_sum[,3:8], 2, function(x) as.numeric(x))



recl_tbl <- read.csv("D:/ConsOppData/AncillaryData/LCV_recl.csv", colClasses = "character", stringsAsFactors = FALSE)

pop_cty$GeoFIPS <-"0"
for (i in 1:nrow(pop_cty)){
  if(pop_cty$GEOID[i] %in% recl_tbl$GEOID){
    temp <- which(recl_tbl$GEOID == pop_cty$GEOID[i])
    pop_cty$GeoFIPS[i] <- recl_tbl$LCVmatch[temp] } else {
      pop_cty$GeoFIPS[i] <- pop_cty$GEOID[i]
    }
}


pop_cty$GeoFIPS <- ifelse(pop_cty$GeoFIPS == "51515", "51019", pop_cty$GeoFIPS) #change for Bedford City, VA
pop_cty$GeoFIPS <- ifelse(pop_cty$GeoFIPS == "46113", "46102", pop_cty$GeoFIPS) #change for Ogalala, SD

pop_geo <- as.character(pop_cty$GeoFIPS)
LCV_geo <- as.character(LCV_sum$GEOID)

colnames(LCV_sum)[12] <- "GeoFIPS"

LCV_shp <- left_join(pop_cty, LCV_sum, by="GeoFIPS")

st_write(LCV_shp,"D:/Data/MonumentData/Generated Data/LCV_shp.shp" )

