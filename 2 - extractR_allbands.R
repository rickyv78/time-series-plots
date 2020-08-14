  ## libraries #################################################################
  library(raster)
  library(rgdal)
  library(maptools)
  library(tidyverse)
  library(stringr)
  library(lubridate)
  library(sf)
  library(snow)
  library(doParallel)
  

  wdir <- "Z:\\RS_SUPPORT\\MargaretByrne_TransitionZones_Oct2019\\Wandoo\\plots"
  imdir <- "w:\\usgs\\112082"
  layer <-   "wandoo_pts_to_ck_90m"
  attrb <- "name"
#blah
  
  
shp <- st_read(paste0(wdir, "\\", layer, ".shp"), quiet = TRUE, stringsAsFactors = FALSE)

pr <- unique(shp$pathrow)
p <- 1

#Define how many cores (memory is limiting factor here)
UseCores <- length(pr)
#Register CoreCluster
cl <- makeCluster(UseCores)
registerDoParallel(cl)
i <- 1
foreach(p = 1:length(pr)) %dopar% {
  library(sf)
  library(rgdal)
  library(raster)
  library(tidyverse) 
  library(lubridate)
#for (p in 1:length(pr)){

imdir <- paste0("w:\\usgs\\", pr[p])

shp.p <- filter(shp, pathrow == pr[p])
sitenames <- shp.p[,which(colnames(shp.p) == attrb), drop = TRUE]
    
imfolders <- list.files(path = imdir, pattern = "pre.ers$", recursive = TRUE, full.names = TRUE)
characters <- sapply(imfolders, nchar ) == 58
imfolders <- as.data.frame(imfolders[characters], stringsAsFactors = FALSE)
colnames(imfolders) <- "name"
imfolders <- mutate(imfolders, date = ymd(paste0(str_sub(name, 16, 19), "-",
                                          str_sub(name, 20, 21), "-",
                                          str_sub(name, 22, 23))), 
                    sat = str_sub(name, 25, 26))


siteFolders <- list.dirs(path = wdir, recursive = FALSE)

alljpg <- as.data.frame(list.files(path = wdir, pattern = "\\d{4}-\\d{2}-\\d{2}-\\d{3}.jpg" ,recursive = TRUE),
                             stringsAsFactors = FALSE)
colnames(alljpg) <- "file"
alljpg <- mutate(alljpg, site = str_sub(file, start = 12, end = -45),
                 date = ymd(paste0(str_sub(file, start = -18, end = -15), "-", 
                                    str_sub(file, start = -13, end = -12), "-", 
                                    str_sub(file, start = -10, end = -9))), 
                 pathrow = str_sub(file, start = -25, end = -20))

alljpg <- filter(alljpg, pathrow == pr[p])

qadates <- unique(alljpg$date)
df <- data.frame()
i <- 1
for (i in 1:length(qadates)){
  landsatImName <- filter(imfolders, date == qadates[i])
  
  landsat <- brick(landsatImName$name[1])  
  names(landsat) <- c("b1", "b2", "b3", "b4", "b5", "b6" )
  ex <- raster::extract(landsat, shp.p)

  j <- 2
  
  for (j in 1:length(ex)){
    plyj <- as.data.frame(ex[j])
    plyj$site <- sitenames[j]
    plyj$date <- qadates[i]
    plyj$sat <- landsatImName$sat[1]
    df <- bind_rows(df, plyj)
  
  }

}
  dfg <- df %>% group_by(site, date) %>%
    summarise(b1 = mean(b1), 
              b2 = mean(b2),
              b3 = mean(b3),
              b4 = mean(b4),
              b5 = mean(b5),
              b6 = mean(b6))

write_csv(dfg, paste0(wdir, "//all_bands_", min(dfg$date), "_to_", 
                      max(dfg$date), "_", pr[p],".csv"))


}
stopCluster(cl)
