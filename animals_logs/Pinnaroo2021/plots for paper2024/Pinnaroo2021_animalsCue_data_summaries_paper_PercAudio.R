#Animal cue data for the paper 2024

library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)




VF_all <-  readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2021/data_for_plots/Pinnaroo2021_all_data.rds") 
VF_no_control <- VF_all %>% filter(VF != "VFControl")

#This anaimal seems a bit suss I will exclude from results
VF_no_control <- VF_no_control %>% filter(animal != 9380384) %>% 

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


animal_list <- VF_no_control %>% distinct(animal)

################################################################################
### for each trial site what is the percentage audio and the SE


Pin2021_VF_no_control_summary_percet_audio <- VF_no_control %>%
  group_by(VF) %>%
  summarise(
    mean_percet_audio = mean(percet_audio, na.rm = TRUE),
    
    sd_percet_audio = sd(percet_audio, na.rm = TRUE)
  )

Pin2021_VF_no_control_summary_percet_audio


Pin2021_no_control_summary_percet_audio <- VF_no_control %>%
  group_by() %>%
  summarise(
    mean_percet_audio = mean(percet_audio, na.rm = TRUE),
    
    sd_percet_audio = sd(percet_audio, na.rm = TRUE)
  )

Pin2021_no_control_summary_percet_audio





################################################################################

################################################################################
str(VF_no_control)
dim(VF_no_control)

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

