## NM DESIGNATIONS THROUGH TIME

library(dplyr)
library(tidyr)

data <- read.csv("C:/Work/SharedDocs/CLLC/NationalMonuments/AA_designations_spreadsheet_formatted.csv", header=TRUE, stringsAsFactors=FALSE)  # load database
tbl_df(data)  # convert to tbl class
View(data)
data %>% group_by(ActionType) %>% summarise(TotalAcres=sum(AcresAffect, na.rm=TRUE))





sort(unique(data$Action))
hist(data$Year[which(data$Action=="Established")])
