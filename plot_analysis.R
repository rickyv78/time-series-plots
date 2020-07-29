library(ggplot2)
library(dplyr)
library(lubridate)
library(sf)
library(stringr)
library(ggrepel)

wd <- "Z:\\DEC\\Wheatbelt_Fire_and_Biodiversity_program_SP2018-072\\DATA\\Working\\TS_PLOTS\\wongan_hills_transects"
#caldir <- "Z:\\DEC\\Wheatbelt_Fire_and_Biodiversity_program_SP2018-072\\DATA\\Working\\TS_PLOTS"
#cal <- read.csv(paste0(caldir, "\\calibration_coefficients_lm.csv"))

cal <- read_csv("Z:\\DEC\\ForestManagementPlan_2014_2023\\DATA\\Working\\calibration\\Jarrah Forest\\coefficients\\i35_linear.csv")

csv <- read_csv(paste0(wd, "\\all_bands_1988-01-03_to_2020-05-02.csv"))

sites <- unique(csv$site)

#offset <- cal[1,4] # get offset value
#gain <- cal[1,5] # get gain value
#gain2 <- cal[1,6]

dir.create(path = paste0(wd, "\\graphs"), showWarnings = FALSE)


i <- 3
for(i in 1:length(sites)){
  
csv01 <- filter(csv, site == sites[i])
csv01 <- na.omit(csv01)
#satvi <- mutate(csv01, satvi = ((b5 - b3)/(b5 + b3 +0.5)) * 1.5 - (b6 / 2))
#csv01 <- mutate(satvi, senescence = (gain * satvi^2) + (gain2 * satvi) + offset )
csv01 <- mutate(csv01, index = ((b5 - b3)/(b5 + b3 +0.5)) * 1.5 - (b6 / 2))
indexName <- "Satvi"
#trend plot
ggplot(csv01, aes(x = as.Date(date), y = index)) +
    geom_line() +
    geom_point() +
    coord_cartesian(ylim = c(10, -40)) +
    #geom_text_repel(aes(label=ifelse(( index > -11), as.character(date), "")))+
    #geom_text_repel(aes(label=ifelse(( index < -20), as.character(date), "")))+
    labs(
      title = paste0("Potential senecence site: ", sites[i]),
      x = "Date",
      y = indexName
    ) +
    theme_bw() 
  
    ggsave(paste0(wd, "\\graphs\\",sites[i], "_", indexName,".png"), width = 7, height = 5)
    
    csv01 <- mutate(csv01, index =  ((b5 + b3)/2)*cal[2,2, drop = TRUE]+ cal[1,2, drop = TRUE] )
    
    indexName <- "i35"   
    #trend plot
    ggplot(csv01, aes(x = as.Date(date), y = index)) +
      geom_line() +
      geom_point() +
      coord_cartesian(ylim = c(-10, 70)) +
      labs(
        title = paste0("Potential senecence site: ", sites[i]),
        x = "Date",
        y = "Vegetation Cover %", 
        caption = paste0("Index = ", indexName)
      ) +
      #geom_text(aes(label = ifelse(i35 < 17, as.character(date), ""))) +
      theme_bw() 
    
    ggsave(paste0(wd, "\\graphs\\",sites[i], "_i35.png"), width = 7, height = 5)
}


