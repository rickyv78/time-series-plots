library(ggplot2)
library(tidyverse)
library(lubridate)
library(sf)
library(stringr)
library(tools)

csvname <- "\\all_bands_1988-01-03_to_2020-05-02.csv"
wd <- "Z:\\DEC\\Wheatbelt_Fire_and_Biodiversity_program_SP2018-072\\DATA\\Working\\TS_PLOTS\\wongan_hills_transects"

 
  jpgs <- as.data.frame(list.files(path = wd, pattern = ".jpg", recursive = TRUE), stringsAsFactors = FALSE)
  colnames(jpgs) <- "names"
  
  
  
  jpgs <- mutate(jpgs, site = str_sub(str_split(names, pattern = "/", 2, simplify = TRUE)[,1], start = 12, end = -19), 
                 date = ymd(str_sub(str_split(names, pattern = "/", 2, simplify = TRUE)[,2], end = -9)))
  
  csv <- read_csv(paste0(wd,csvname))
  clean <- jpgs %>% left_join(csv, by = c("date", "site")) %>%
    dplyr::select(-names)
  
  write_csv(clean, paste0(wd,csvname))
  