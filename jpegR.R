library(devtools)
install.packages("RSPaW/RSSApkg")
library(RSSApkg)
library(sf)
library(tidyverse)

wdir <- "Z:\\DEC\\Wheatbelt_Fire_and_Biodiversity_program_SP2018-072\\DATA\\Working\\TS_PLOTS\\wongan_hills_transects"
imdir <- "w:\\usgs\\112081"
layer <-   "Senecence_transects_2019_WH_30m" ##No .shp suffix
attrb <- "Trans_ID"
combo <- c(5,4,2)



jpegR(wdir, imdir, layer, attrb, start = NA, stop = NA, combo,
      buffer = 2000)

