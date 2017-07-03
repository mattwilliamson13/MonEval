setwd("C:/ConsOppData/SpatialData/CongressionalDistricts/")
rt <- "http://cdmaps.polisci.ucla.edu/shp/districts"
cn <- seq(from=92, to=114, by = 1)
cnp <- ifelse(cn < 100, paste0("0",cn), cn)
ext <- ".zip"

for(i in 1:length(cnp)){
  y <- cnp[i]
  link <- paste0(rt,y,ext)
  fname <- paste0("districts",y,ext)
  download.file(url=link, destfile=fname)
}

dist.zip <- list.files(getwd(), pattern=".zip")
lapply(dist.zip,unzip, junkpaths=TRUE)
