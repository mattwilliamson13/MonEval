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


count_econ <- read.csv("C:/Users/mwilliam/ConsOppBatch/EconData/CA25N_2001_2015__ALL_AREAS.csv", stringsAsFactors = FALSE, strip.white = TRUE)

count_econ$Description <- gsub(" ", "", count_econ$Description, fixed = TRUE)
all_us <- count_econ[is.na(count_econ$Region),]
count_econ <- count_econ[!is.na(count_econ$Region),] #remove US level summaries
state_sums <- count_econ[grepl("000$",count_econ$GeoFIPS),] #find all state summary levels
count_econ2 <- count_econ[!grepl("000$",count_econ$GeoFIPS),] #remove all state summary levels
count_econ3 <- count_econ2[!grepl("^02",count_econ2$GeoFIPS),] #remove AK
count_econ4 <- count_econ3[!grepl("^15",count_econ3$GeoFIPS),] #remove AK

econ_sort <- count_econ4[count_econ4$LineCode == "10" | count_econ4$LineCode == "70" |count_econ4$LineCode == "200" | count_econ4$LineCode == "100" | count_econ4$LineCode == "1100" | count_econ4$LineCode == "400",]
econ_gather <- gather(econ_sort, key="Year", value="Emp", 8:22 )
econ_gather$Year <- gsub("X", "",econ_gather$Year)
econ_gather$Emp <- as.numeric(econ_gather$Emp)

econ_gather_t <- econ_gather %>% group_by(GeoFIPS, Year) %>% mutate(Total = max(Emp, na.rm=TRUE))
econ <- econ_gather_t %>% group_by(GeoFIPS, Year, LineCode) %>% mutate(PercEmp = Emp/Total * 100)

dy <- econ %>% group_by(GeoFIPS, LineCode) %>% summarise(base_year = min(Year[is.na(Emp)==FALSE]),
                                                         last_year = max(Year[is.na(Emp)==FALSE]),
                                                         numNA = sum(is.na(Emp)))
dy$base_year <- ifelse(is.na(dy$base_year), 2001, dy$base_year) #set base year for records that are never reported to keep them in the subsequent data
dy$last_year <- ifelse(is.na(dy$last_year), 2002, dy$last_year) 

econ_sum <- merge(dy, econ, by.x = c("GeoFIPS", "LineCode", "base_year"),by.y=c("GeoFIPS","LineCode","Year"), all.x=TRUE)

colnames(econ_sum)[11:13] <- paste0("Init",colnames(econ_sum)[11:13])

econ_sum2 <-merge(econ_sum, econ[,c(1,5,8:11)], by.x = c("GeoFIPS", "LineCode", "last_year"), by.y=c("GeoFIPS","LineCode","Year"), all.x=TRUE) 

econ_sum2 <-merge(econ_sum, econ[,c(1,5,8:11)], by.x = c("GeoFIPS", "LineCode", "last_year"), by.y=c("GeoFIPS","LineCode","Year"), all.x=TRUE) 
colnames(econ_sum2)[14:16] <- paste0("Last",colnames(econ_sum2)[14:16])
econ_sum2$delEmp <- ifelse(econ_sum2$numNA <= 13, econ_sum2$LastEmp - econ_sum2$InitEmp,NA)
econ_sum2$delPercEmp <- ifelse(econ_sum2$numNA <= 13, econ_sum2$LastPercEmp - econ_sum2$InitPercEmp,NA)
econ_sum2$delEmpRt <- econ_sum2$delEmp/(as.numeric(econ_sum2$last_year) - as.numeric(econ_sum2$base_year))
econ_sum2$delPercEmpRt <- econ_sum2$delPercEmp/(as.numeric(econ_sum2$last_year) - as.numeric(econ_sum2$base_year))

econ_summary <- econ %>%  group_by(GeoFIPS, LineCode) %>% summarise(med = median(PercEmp, na.rm=TRUE),
                                                                    max = max(PercEmp, na.rm=TRUE))
econ_full <- merge(econ_sum2, econ_summary, by=c("GeoFIPS","LineCode"), all.x=TRUE)

econ_full$Description <- gsub("Total.+","Total", econ_full$Description)
econ_full$Description <- gsub("Farm.+","Farm", econ_full$Description)
econ_full$Description <- gsub("Forestry.+","ForestryNR", econ_full$Description)
econ_full$Description <- gsub("Mining.+","Mining", econ_full$Description)
econ_full$Description <- gsub("Real.+","RE", econ_full$Description)

econ_cst <- econ_full[,c(1,10:22)]

econ_wide <- reshape(econ_cst, idvar="GeoFIPS", timevar = "Description", direction = "wide")
ew <- do.call(data.frame,lapply(econ_wide, function(x) replace(x, is.infinite(x),NA))) #remove infite values due to NAs

#rejoin other data to wide df
econ_sprd <- merge(ew, unique(econ_full[,c(1,6)]),by="GeoFIPS")


#BEA data collapse GEO codes for a number of counties; tigris data also uses old code for Ogala

recl_tbl <- read.csv("D:/ConsOppData/AncillaryData/BEAFIPSModifications.csv", colClasses = "character", stringsAsFactors = FALSE)
recl_tbl <- recl_tbl[-c(54:55),] #eliminate collapse in WI for census before 1989
pop_cty$GeoFIPS <-"0"
for (i in 1:nrow(pop_cty)){
  if(pop_cty$GEOID[i] %in% recl_tbl$FIPS){
    temp <- which(recl_tbl$FIPS == pop_cty$GEOID[i])
    pop_cty$GeoFIPS[i] <- recl_tbl$BEA.FIPS[temp] } else {
    pop_cty$GeoFIPS[i] <- pop_cty$GEOID[i]
  }
}

econ_geo <- as.character(econ_sprd$GeoFIPS)
pop_geo <- pop_cty$GeoFIPS
pop_geo[!(pop_geo %in% econ_geo)]

pop_cty$GeoFIPS <- ifelse(pop_cty$GeoFIPS == "51515", "51019", pop_cty$GeoFIPS) #change for Bedford City, VA
pop_cty$GeoFIPS <- ifelse(pop_cty$GeoFIPS == "46113", "46102", pop_cty$GeoFIPS) #change for Ogalala, SD


econ_shp <- left_join(pop_cty, econ_sprd, by="GeoFIPS")

st_write(econ_shp,"D:/Data/MonumentData/Generated Data/econ_shp.shp" )

