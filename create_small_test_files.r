# make categorical raster (integer values)
rcat <- raster(ncol=10, nrow=10)
values(rcat) <- 1:ncell(rcat)   
projection(rcat) <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
plot(rcat)

# make continuous raster (decimal values)
rcont <- raster(ncol=10, nrow=10)
values(rcont) <-  runif(ncell(rcont)) 
projection(rcat) <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
plot(rcont)

# make sample set of polygons
square <- t(replicate(5, {
  o <- runif(2,-75,75)
  c(o, o + c(0, 20), o + 20, o + c(20, 0), o)
}))
ID <- paste0('sq', seq_len(nrow(square)))

# Create SP
polys <- SpatialPolygons(mapply(function(poly, id) {
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=id)
}, split(square, row(square)), ID))

# Create SPDF
polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))
polys <- st_as_sf(polys.df)
st_crs(polys) <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

plot(rcont)
plot(polys, add=TRUE)

# save in new workspace
save(list=c("rcat","rcont","polys"),file="C:/Users/Tyler/Desktop/smallSampleSpatialData.RData")
