library(ggplot2)
library(tidyverse)
library(lubridate)
library(sf)
library(stringr)
library(tools)

csvname <- "\\all_bands_1988-01-03_to_2020-05-02.csv"
wdir <- "Z:\\RS_SUPPORT\\MargaretByrne_TransitionZones_Oct2019\\Wandoo\\plots"

csv.lst <- list.files(path = wdir, pattern = ".csv", full.names = TRUE)

csv <- bind_rows(read_csv(csv.lst[1]), 
                 read_csv(csv.lst[2]), 
                 read_csv(csv.lst[3]))
 
  jpgs <- as.data.frame(list.files(path = wdir, pattern = ".jpg", recursive = TRUE), stringsAsFactors = FALSE)
  colnames(jpgs) <- "names"
  
  jpgs <- mutate(jpgs, site = str_sub(str_split(names, pattern = "/", 2, simplify = TRUE)[,1], start = 12, end = -26), 
                 date = ymd(str_sub(str_split(names, pattern = "/", 2, simplify = TRUE)[,2], end = -9)))
  
  
  clean <- jpgs %>% left_join(csv, by = c("date", "site")) %>%
    dplyr::select(-names)
  
  write_csv(clean, paste0(wdir,"\\all_bands_all_sites_",Sys.Date(),  ".csv"))
  