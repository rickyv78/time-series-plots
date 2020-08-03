# load the sp and rgdal packages

library(sp)
library(rgdal)
library(sf)

# set working directory to data folder
wd <- "Z:\\RS_SUPPORT\\MargaretByrne_TransitionZones_Oct2019\\Wandoo\\"
id_field <- "name"
shapename <- "wandoo_pts_to_ck.shp"
mga50 <- "+proj=utm +zone=50 +south +ellps=GRS80 +units=m +no_defs "

# set the radius for the plots
radius <- 45 # radius in meters

###########################################
shape <- st_read(paste0(wd,shapename), stringsAsFactors = FALSE)
shape <- st_transform(shape, mga50)
coords <- as.data.frame(st_coordinates(shape))

#coords <- as.data.frame(shape@coords)
coords$number <- rownames(coords)

# define the plot edges based upon the plot radius. 
yPlus <- coords$Y+radius
xPlus <- coords$X+radius
yMinus <- coords$Y-radius
xMinus <- coords$X-radius

# calculate polygon coordinates for each plot centroid. 
square=cbind(xMinus,yPlus,  # NW corner
             xPlus, yPlus,  # NE corner
             xPlus,yMinus,  # SE corner
             xMinus,yMinus, # SW corner
             xMinus,yPlus)  # NW corner again - close ploygon

# Extract the plot ID information
ID <- shape[,which(colnames(shape) == id_field), drop = TRUE]

a <- vector('list', length(2))
# loop through each centroid value and create a polygon
# this is where we match the ID to the new plot coordinates
for (i in 1:nrow(coords)) {  # for each for in object centroids
  a[[i]]<-Polygons(list(Polygon(matrix(square[i, ], ncol=2, byrow=TRUE))), ID[i]) 
  # make it an Polygon object with the Plot_ID from object ID
}

# convert a to SpatialPolygon and assign CRS
polysB<-SpatialPolygons(a,proj4string=CRS(as.character(st_crs(shape))))

polys.df <- SpatialPolygonsDataFrame(polysB, data.frame(id=ID, row.names=ID))

setwd(wd)
writeOGR(polys.df, ".", layer = paste0(shapename, "_", radius*2, "m") ,driver="ESRI Shapefile", overwrite_layer = TRUE)

