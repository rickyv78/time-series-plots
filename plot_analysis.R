library(ggplot2)
library(dplyr)
library(lubridate)
library(sf)
library(stringr)

wd <- "Z:\\DEC\\Wheatbelt_Fire_and_Biodiversity_program_SP2018-072\\DATA\\Working\\TS_PLOTS\\wongan_hills"
#caldir <- "Z:\\DEC\\Wheatbelt_Fire_and_Biodiversity_program_SP2018-072\\DATA\\Working\\TS_PLOTS"
#cal <- read.csv(paste0(caldir, "\\calibration_coefficients_lm.csv"))

cal <- read_csv("Z:\\DEC\\ForestManagementPlan_2014_2023\\DATA\\Working\\calibration\\Jarrah Forest\\coefficients\\i35_linear.csv")

csv <- read_csv(paste0(wd, "\\all_bands_1988-01-03_to_2020-05-18.csv"))

sites <- unique(csv$site)

#offset <- cal[1,4] # get offset value
#gain <- cal[1,5] # get gain value
#gain2 <- cal[1,6]

i <- 10
for(i in 1:length(sites)){
  
csv01 <- filter(csv, site == sites[i])
csv01 <- na.omit(csv01)
#satvi <- mutate(csv01, satvi = ((b5 - b3)/(b5 + b3 +0.5)) * 1.5 - (b6 / 2))
#csv01 <- mutate(satvi, senescence = (gain * satvi^2) + (gain2 * satvi) + offset )
csv01 <- mutate(csv01, satvi = ((b5 - b3)/(b5 + b3 +0.5)) * 1.5 - (b6 / 2))

#trend plot
ggplot(csv01, aes(x = as.Date(date), y = satvi)) +
    geom_line() +
    geom_point() +
    coord_cartesian(ylim = c(10, -40)) +
    labs(
      title = paste0("Potential senecence site: ", sites[i]),
      x = "Date",
      y = "Satvi"
    ) +
    theme_bw() 
  
    ggsave(paste0(wd, "\\graphs\\",sites[i], "_satvi.png"))
    
    csv01 <- mutate(csv01, i35 =  ((b5 + b3)/2)*cal[2,2, drop = TRUE]+ cal[1,2, drop = TRUE] )
    
    
    #trend plot
    ggplot(csv01, aes(x = as.Date(date), y = i35)) +
      geom_line() +
      geom_point() +
      coord_cartesian(ylim = c(-10, 70)) +
      labs(
        title = paste0("Potential senecence site: ", sites[i]),
        x = "Date",
        y = "Vegetation Cover %", 
        caption = "Index = i35"
      ) +
      #geom_text(aes(label = ifelse(i35 < 17, as.character(date), ""))) +
      theme_bw() 
    
    ggsave(paste0(wd, "\\graphs\\",sites[i], "_i35.png"))
}


