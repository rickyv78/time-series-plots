library(devtools)
install.packages("RSPaW/RSSApkg")
library(RSSApkg)
library(sf)
library(tidyverse)
library(snow)
library(doParallel)

wdir <- "Z:\\RS_SUPPORT\\MargaretByrne_TransitionZones_Oct2019\\Wandoo\\plots"
imdir <- "w:\\usgs\\112082"
layer <-   "wandoo_pts_to_ck_90m" ##No .shp suffix
attrb <- "name"
combo <- c(5,4,2)


jpegR(wdir, imdir, layer, attrb, start = NA, stop = NA, combo,
      buffer = 2000)

################### modified function code ###################
buffer = 2000
start = NA
stop = NA
  
  shp <- st_read(paste0(wdir, "\\", layer, ".shp"), stringsAsFactors = FALSE)
  pr <- unique(shp$pathrow)
  p <- 2
for (p in 1:length(pr)){
  imdir <- paste0("w:\\usgs\\", pr[p])
  
  shp.p <- filter(shp, pathrow == pr[p])
  
  shpnames <- shp.p[,which(colnames(shp.p) == attrb), drop = TRUE]
 
  alldo <- u_dateR(path = imdir, archive = TRUE)
  suppressWarnings(start <- lubridate::dmy(start))
  suppressWarnings(stop <- lubridate::dmy(stop))
  todo <- if (!is.na(start) & !is.na(stop)) {
    sub <- subset(alldo, dates >= start & dates <= stop)
    sub
  } else if (is.na(start) & !is.na(stop)) {
    sub <- subset(alldo, dates <= stop)
    sub
  }else if (!is.na(start) & is.na(stop)) {
    sub <- subset(alldo, dates >= start)
    sub
  }else {
    alldo
  }
  proj <- raster::crs(raster::raster(todo[1, 1]))
  i <- 1
  
  #Define how many cores (memory is limiting factor here)
  UseCores <- 12
  #Register CoreCluster
  cl <- makeCluster(UseCores)
  registerDoParallel(cl)
  i <- 1
  foreach(i = seq_along(shpnames)) %dopar% {
    library(sf)
    library(rgdal)
    library(raster)
    library(tidyverse) 
  
 # for (i in seq_along(shpnames)) {
    shp.i <- as(shp.p[i,], 'Spatial')
    shp_t <- sp::spTransform(shp.i, proj)
    ext <- raster::extent(shp_t) + buffer
    beg <- todo[1, 2]
    end <- todo[length(todo[, 2]), 2]
    folder <- paste0(wdir, "/jpegs_site_", shpnames[i], "_", 
                     beg, "-", end, "_", pr[p])
    if (!file.exists(folder)) {
      dir.create(folder)
    }
    for (j in seq_along(todo[, 1])) {
      date <- todo[j, "dates"]
      jname <- paste0(date, "-", paste(combo, collapse = ""), 
                      ".jpg")
      fname <- paste0(folder, "/", jname)
      img <- todo[j, "path"]
      rstack <- raster::stack(img)
      jpeg(filename = fname, width = 842, height = 870)
      tryCatch({
        expr = raster::plotRGB(rstack, r = combo[1], 
                               g = combo[2], b = combo[3], ext = ext, stretch = "lin")
      }, error = function(i) {
        raster::plotRGB(rstack, r = combo[1], g = combo[2], 
                        b = combo[3], ext = ext)
        message("Very cloudy at this extent")
      })
      raster::plot(shp_t, add = TRUE, lwd = 2, border = "magenta")
      dev.off()
    }
  }
  stopCluster(cl)
}  
