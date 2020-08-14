library(ggplot2)
library(tidyverse)
library(lubridate)
library(sf)
library(stringr)
library(ggrepel)
library(broman)

wdir <- "Z:\\RS_SUPPORT\\MargaretByrne_TransitionZones_Oct2019\\Wandoo\\plots"
#caldir <- "Z:\\DEC\\Wheatbelt_Fire_and_Biodiversity_program_SP2018-072\\DATA\\Working\\TS_PLOTS"
#cal <- read.csv(paste0(caldir, "\\calibration_coefficients_lm.csv"))
mga50 <- "+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs"

cal <- read_csv("Z:\\DEC\\ForestManagementPlan_2014_2023\\DATA\\Working\\calibration\\Jarrah Forest\\coefficients\\i35_linear.csv")

csv <- read_csv(paste0(wdir, "\\all_bands_all_sites_2020-08-05.csv"))
csv.lst <- list.files(path = wdir, pattern = ".csv", full.names = TRUE)

layer <-   "wandoo_pts_to_ck" ##No .shp suffix
plys <- st_read(paste0(wdir, "\\", layer, ".shp"), stringsAsFactors = FALSE)
plys <- st_transform(plys, mga50)

# Districts:  MER GLD PIL ESP SWC MRA MOR DON BWD PHL WEL FRK ALB GTN EXM SHK WTN GSN PHS CWB
fireAll <- st_read(dsn="V:\\GIS1-Corporate\\data\\GDB\\Fire\\Burn_data\\Fire_Data_Burns.gdb", layer = "CPT_FIRE_HISTORY",  
               query = "SELECT * FROM \"CPT_FIRE_HISTORY\" WHERE FIH_DISTRICT IN ('PHS', 'PHL', 'CWB')", 
               quiet = TRUE)
fireAll <- st_transform(fireAll, mga50)

sites <- unique(csv$site)

dir.create(path = paste0(wdir, "\\graphs"), showWarnings = FALSE)

i <- 10
for(i in 1:length(sites)){
  
ply <- filter(plys, name == sites[i] )
pti <- st_intersection(ply, fireAll)
pti <- pti %>% arrange(desc(FIH_YEAR1)) %>%
  filter(FIH_DATE1 > ymd("1985-01-01")) 
pti$FIH_DATE1 <- as.Date(pti$FIH_DATE1)

csv01 <- filter(csv, site == sites[i])
csv01 <- na.omit(csv01)

    csv01 <- mutate(csv01, index =  ((b5 + b3)/2)*cal[2,2, drop = TRUE]+ cal[1,2, drop = TRUE] )
    
    mean.i <- mean(csv01$index)
    csv01$med <- runningmean(1:nrow(csv01), csv01$index, window = 20, what = "median")
    csv01 <- mutate(csv01, diff = index - med)
    
    indexName <- "i35"   
    #trend plot
    ggplot(csv01, aes(x = as.Date(date), y = index)) +
      geom_line() +
      geom_point() +
      geom_line(aes(x = as.Date(date), y = med), color = "red")+
      geom_vline(xintercept = pti$FIH_DATE1[1], colour = "red", linetype = 2)+  
      annotate("text", x= (pti$FIH_DATE1[1] + 200 ), y= 35, label = paste0(pti$FIH_POLY_T[1]," : ",pti$FIH_DATE1[1] ), 
               colour = "red", angle  = 90, size = 3.5)+
      geom_vline(xintercept = pti$FIH_DATE1[2], colour = "red", linetype = 2)+
      annotate("text", x= (pti$FIH_DATE1[2] + 200 ), y= 35, label = paste0(pti$FIH_POLY_T[2]," : ",pti$FIH_DATE1[2] ), 
               colour = "red", angle  = 90, size = 3.5)+
      geom_vline(xintercept = pti$FIH_DATE1[3], colour = "red", linetype = 2)+  
      annotate("text", x= (pti$FIH_DATE1[3] + 200 ), y= 35, label = paste0(pti$FIH_POLY_T[3]," : ",pti$FIH_DATE1[3] ), 
               colour = "red", angle  = 90, size = 3.5)+
      geom_vline(xintercept = pti$FIH_DATE1[4], colour = "red", linetype = 2)+
      annotate("text", x= (pti$FIH_DATE1[4] + 200 ), y= 35, label = paste0(pti$FIH_POLY_T[4]," : ",pti$FIH_DATE1[4] ), 
               colour = "red", angle  = 90, size = 3.5)+
      geom_vline(xintercept = ymd("2011/04/01"), color = "blue", linetype = "dashed")+
      annotate("text", label = "Drought/heatwave", x = ymd("2011/9/01"), y = 30, color = "blue", angle  = 90)+
      #geom_text_repel(aes(label=ifelse((diff < -10), as.character(date),  "")))+
      coord_cartesian(ylim = c(-10, 70)) +
      labs(
        title = paste0("Potential TERN site: ", sites[i]),
        x = "Date",
        y = "Vegetation Cover %", 
        caption = paste0("Index = ", indexName)
      ) +
      #geom_text(aes(label = ifelse(i35 < 17, as.character(date), ""))) +
      theme_bw() 
    
    ggsave(paste0(wdir, "\\graphs\\",sites[i], "_i35.png"), width = 7, height = 5)
}


