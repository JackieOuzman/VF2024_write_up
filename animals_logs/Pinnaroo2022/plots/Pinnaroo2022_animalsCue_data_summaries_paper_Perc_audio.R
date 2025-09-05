#Animal cue data for the paper 2024

library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)




VF_all <-  readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/Pinnaroo2022_all_data.rds") 
VF_no_control <- VF_all %>% filter(VF != "VFControl")

#This anaimal seems a bit suss I will exclude from results
#VF_no_control <- VF_no_control %>% filter(animal != 9380384) %>% 

### mean of audio and cue data for all VF animals
names(VF_no_control)

#Lots of negative values - lets recode these to zero
VF_no_control <- VF_no_control %>%
  mutate(
    Audio_values=  case_when(
      Audio_values < 0 ~ 0,
      .default = Audio_values))
VF_no_control <- VF_no_control %>%
  mutate(
    Shock_values=  case_when(
      Shock_values < 0 ~ 0,
      .default = Shock_values))


################################################################################
## make a new variable #############

str(VF_no_control)

VF_no_control <- VF_no_control %>% 
  mutate(percet_audio = ((Audio_values/ (Audio_values + Shock_values)*100)),)




################################################################################
### for each trial site what is the percentage audio and the SE


Pin2022_VF_no_control_summary_percet_audio <- VF_no_control %>%
  group_by(VF) %>%
  summarise(
    mean_percet_audio = mean(percet_audio, na.rm = TRUE),
    
    sd_percet_audio = sd(percet_audio, na.rm = TRUE)
  )

Pin2022_VF_no_control_summary_percet_audio


Pin2022_no_control_summary_percet_audio <- VF_no_control %>%
  group_by() %>%
  summarise(
    mean_percet_audio = mean(percet_audio, na.rm = TRUE),
    
    sd_percet_audio = sd(percet_audio, na.rm = TRUE)
  )

Pin2022_no_control_summary_percet_audio


test_V2 <- VF_no_control %>% filter(VF == "VF2" )


################################################################################
str(VF_no_control)


obs_per_day <- VF_no_control %>%
  group_by(date ) %>%
  summarize(
        count = n() # Count of observations in each group
  )

obs_per_day_stats <- obs_per_day %>%
  summarize(
    mean_value = mean(count, na.rm = TRUE),
    median_value = median(count, na.rm = TRUE))

#############################################################################



obs_per_day_animal <- VF_no_control %>%
  group_by(date , animal) %>%
  summarize(
    count = n()) 
    

obs_per_day_animal_stats <- obs_per_day_animal %>%
  summarize(
    
    mean_value = mean(count, na.rm = TRUE),
    median_value = median(count, na.rm = TRUE), 
    min = min(count, na.rm = TRUE),
    max = max(count, na.rm = TRUE))
obs_per_day_animal_stats


dim(VF_no_control)
