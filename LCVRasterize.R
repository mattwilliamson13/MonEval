library(tigris)
library(raster)
library(sf)

##Get Continental US list
us.abbr <- unique(fips_codes$state)[1:51]
us.name <- unique(fips_codes$state_name)[1:51]

us.states <- as.data.frame(cbind(us.abbr, us.name))
colnames(us.states) <- c("state","STATENAME")
us.states$state <- as.character(us.states$state)
us.states$STATENAME <- as.character(us.states$STATENAME)
continental.states <- us.states[us.states$state != "AK" & us.states$state != "HI" & us.states$state != "DC",] #only CONUS

##Load Cong District
setwd("D:/Data/MonumentData/LCV/Outputs/")

dist.files <- list.files(getwd(),pattern=".shp")
prj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" #projection for NatLandCover in Monument Dataset

options(warn=-1)  # temporarily turn off warnings
for(i in 1:length(dist.files)) {
  temp1 <- readOGR(dist.files[i])
  temp1 <- spTransform(temp1, prj)
  if(length(which(gIsValid(temp1)==FALSE))==0) {
    print(paste("Geometry is valid for layer ",dist.files[i], sep=""))
    st_write(as(temp1,"sf"),paste0("D:/Data/MonumentData/LCV/Outputs/GeomClean/",dist.files[i]))
  } else {  # if invalid geometries are found (e.g., Ring Self-intersection), convert to sp and then add zero-width buffer
    print("Invalid geometry found - applying zero-width buffer...")
    temp2 <- gBuffer(temp1, byid=TRUE, width=0)  # add zero-width buffer
    if(length(which(gIsValid(temp2)==FALSE))==0) {  # check again for invalid geometries
      print(paste("Geometry corrected for layer ", dist.files[i], sep=""))
      temp3 <- as(temp2, "sf")
      st_write(temp3,paste0("D:/Data/MonumentData/LCV/Outputs/GeomClean/",dist.files[i]))
    } else {
      stop(paste("Unable to correct geometry for layer ",dist.files[i],"!!!", sep=""))
    }
    rm(temp1, temp2, temp3)
  }
}
options(warn=0)  # turn warnings back on


##Select all districts within a state
rasterOptions(tmpdir="D:/RastTemp", progress="text", maxmemory=1e+09, chunksize=1e+08)
setwd("D:/Data/MonumentData/LCV/Outputs/GeomClean/")
i <- 1
proj.dist.files <- list.files(getwd(),pattern=".shp")
for(s in 2:nrow(continental.states)){
st <- continental.states[s,2]
  for(i in 1:length(proj.dist.files)){
  yr <- substr(proj.dist.files[i], 1,4)
  a <- st_read(proj.dist.files[i])
  st.cd <- filter(a, STATENAME == st)
  a.box <- st_bbox(st.cd)
  print(paste0("Rasterizing",continental.states[s,2], yr, sep=" "))
  r <- raster(extent(a.box[1], a.box[3], a.box[2], a.box[4]), crs=st_crs(st.cd))
  res(r) <- c(90,90)
  r[] <- NA
  st.cd.sp <- as(st.cd, "Spatial")
  cdr <- rasterize(st.cd.sp,r,field= "LCVScore")
  writeRaster(cdr,filename=paste0("D:/Data/MonumentData/LCV/Outputs/GeomClean/RasterOuts/",st, yr),format="GTiff", options= c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"))
  }
}
