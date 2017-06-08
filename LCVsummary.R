setwd("C:/ConsOppData/InstitutionalData/LCVScores/")
#Load all of the house files downloaded using LCVdownload.R
house.files <- list.files(getwd(), pattern="house.csv")

LCVHouseScore <- function(fname){
df <- read.csv(fname, skip=6)
df$state <- substr(df$District, 1,2)
df$year <- substr(colnames(df)[4],2,5)
NameList <- c("state", "year", paste0("X",substr(fname,1,4),".Score")) #Takes only the year score, not the lifetime score of the rep
idx <- match(NameList, names(df))
df2 <- df[,idx]
colnames(df2)[3] <- "LCVScore"
return(df2)
}

hse <- lapply(house.files, LCVHouseScore)
LCVhse <- do.call(rbind, hse)
LCVhse$body <- "House"

#Load all of the Senate files downloaded using LCVdownload.R

senate.files <- list.files(getwd(), pattern="senate.csv")

LCVSenateScore <- function(fname){
df <- read.csv(fname, skip=6)
df$year <- substr(colnames(df)[4],2,5)
NameList <- c("State", "year", paste0("X",substr(fname,1,4),".Score")) #Takes only the year score, not the lifetime score of the rep
idx <- match(NameList, names(df))
df2 <- df[,idx]
colnames(df2)[3] <- "LCVScore"
return(df2)
}

sen <- lapply(senate.files, LCVSenateScore)
LCVsen <- do.call(rbind, sen)

#generate state level summaries for the House
library(dplyr)
LCVhse$LCVScore <- as.numeric(as.character(LCVhse$LCVScore)) #There are 53 n/a
hse.sum <- LCVhse %>%
      group_by(state) %>%
      summarise(
        meanLCV = mean(LCVScore, na.rm=TRUE),
        maxLCV = max(LCVScore, na.rm=TRUE),
        minLCV = min(LCVScore,na.rm=TRUE),
        medianLCV = median(LCVScore,na.rm=TRUE),
        upper90LCV = quantile(LCVScore, probs=0.9, na.rm=TRUE),
        lower10LCV = quantile(LCVScore, probs=0.1, na.rm=TRUE),
        upper75LCV = quantile(LCVScore, probs=0.75, na.rm=TRUE),
        lower25LCV = quantile(LCVScore, probs=0.25, na.rm=TRUE))

  #generate state level summaries for the senate
LCVsen$LCVScore <- as.numeric(as.character(LCVsen$LCVScore))
sen.sum <- LCVsen %>%
      group_by(State) %>%
      summarise(
        meanLCV = mean(LCVScore, na.rm=TRUE),
        maxLCV = max(LCVScore, na.rm=TRUE),
        minLCV = min(LCVScore,na.rm=TRUE),
        medianLCV = median(LCVScore,na.rm=TRUE),
        upper90LCV = quantile(LCVScore, probs=0.9, na.rm=TRUE),
        lower10LCV = quantile(LCVScore, probs=0.1, na.rm=TRUE),
        upper75LCV = quantile(LCVScore, probs=0.75, na.rm=TRUE),
        lower25LCV = quantile(LCVScore, probs=0.25, na.rm=TRUE))
