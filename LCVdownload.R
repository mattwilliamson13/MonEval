setwd("/Users/matthewwilliamson/Downloads/LCVHouse/") #for Matt's Mac
setwd("C:/ConsOppData/InstitutionalData/LCVScores/")
rt <- "http://scorecard.lcv.org/exports/"
yr <- seq(from=1972,to=2016,by=1)
hs <- "-house"
fl <- "-scorecard-grid-export.csv"

for(i in 1:length(yr)){
  y <- yr[i]
  link <- paste0(rt,y,hs,fl)
  fname <- paste0(y,hs,".csv")
  download.file(url=link, destfile=fname)
}


setwd("/Users/matthewwilliamson/Downloads/LCVSenate/")
hs <- "-senate"
