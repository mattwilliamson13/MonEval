# Generate PA cost distance matrix


# set working directory
setwd()

# Read in human modification layer
modification <- raster("C:/Work/SpatialData/LCLU/TheobaldHumanModificationNorthAmerica/data/commondata/raster_data/gHM_NA_270.tif")

# Convert to cost surface (what function to apply? Assume linear? See what Travis did.)
cost <- modification

# specify location of Circuitscape program
CS_exe <- 'C:/"Program Files"/Circuitscape/cs_run.exe' # note that quotations are "Program File" are intentional to deal with space in path

# read in shapefile of patches as SpatialPolygons object
sites <- 

# reproject to common projection and check for invalid geometries if using polygons  

# plot it
plot(cost)
plot(sites,pch=19,col=2, add=TRUE)

# rasterize points using the cost extent
sites <- rasterize(x = sites,y = cost)

# Write rasters to your working directory
writeRaster(sites,"sites_rast.asc",overwrite=TRUE)
writeRaster(cost,"cost_rast.asc",overwrite=TRUE)

# Make an .ini file
CS_ini <- c("[circuitscape options]",            
            "data_type = raster",
            "scenario = pairwise",
            paste(c("point_file =",
                    "habitat_file =",
                    "output_file ="),
                  paste(getwd(),c("sites_rast.asc",
                                  "cost_rast.asc",
                                  "CS.out"),
                        sep="/")))

# Write it to your working directory
writeLines(CS_ini,"myini.ini")

# Make the CS run cmd
CS_run <- paste(CS_exe, paste(getwd(),"myini.ini",sep="/")) # Make the cmd

# Run the command
system(CS_run)

# Import the effective resistance
rdist <- as.dist(read.csv("CS_resistances.out",sep=" ",row.names=1,header=1))