  ## libraries #################################################################
  library(raster)
  library(rgdal)
  library(maptools)
  library(tidyverse)
  library(stringr)
  library(lubridate)
  library(sf)
  

  wkdir <- "Z:\\DEC\\Wheatbelt_Fire_and_Biodiversity_program_SP2018-072\\DATA\\Working\\TS_PLOTS\\wongan_hills"
  imdir <- "w:\\usgs\\112081"
  shpName <- "wongan_ck_pt_mga50_90m.shp" ## .shp suffix
  attrb <- "name"
#blah
  


shp <- st_read(paste0(wkdir, "\\", shpName), quiet = TRUE)

sitenames <- shp$name 
  
imfolders <- list.files(path = imdir, pattern = "pre.ers$", recursive = TRUE, full.names = TRUE)
characters <- sapply(imfolders, nchar ) == 58
imfolders <- as.data.frame(imfolders[characters], stringsAsFactors = FALSE)
colnames(imfolders) <- "name"
imfolders <- mutate(imfolders, date = ymd(paste0(str_sub(name, 16, 19), "-",
                                          str_sub(name, 20, 21), "-",
                                          str_sub(name, 22, 23))), 
                    sat = str_sub(name, 25, 26))


siteFolders <- list.dirs(path = wkdir, recursive = FALSE)

alljpg <- as.data.frame(list.files(path = wkdir, pattern = "\\d{4}-\\d{2}-\\d{2}-\\d{3}.jpg" ,recursive = TRUE),
                             stringsAsFactors = FALSE)
colnames(alljpg) <- "file"
alljpg <- mutate(alljpg, site = str_sub(file, start = 12, end = -38),
                 date = ymd(paste0(str_sub(file, start = -18, end = -15), "-", 
                                    str_sub(file, start = -13, end = -12), "-", 
                                    str_sub(file, start = -10, end = -9))))

qadates <- unique(alljpg$date)
df <- data.frame()
i <- 1
for (i in 1:length(qadates)){
  landsatImName <- filter(imfolders, date == qadates[i])
  
  landsat <- brick(landsatImName$name[1])  
  names(landsat) <- c("b1", "b2", "b3", "b4", "b5", "b6" )
  ex <- raster::extract(landsat, shp)

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

write_csv(dfg, paste0(wkdir, "//all_bands_", min(dfg$date), "_to_", max(dfg$date), ".csv"))



