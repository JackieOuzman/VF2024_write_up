
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)




VF_all <-  readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2021/data_for_plots/Pinnaroo2021_all_data.rds") 
str(VF_all)
unique(VF_all$VF)
unique(VF_all$DOY)
unique(VF_all$date)

VF_no_control <- VF_all %>% filter(VF != "VFControl")


#### summaries the data - cummlative data
cue_max_per_animal_day <- VF_no_control %>% group_by( date, animal) %>% 
  summarise(max_audio = max(cumulativeAudioCount),
            max_pulse = max(cumulativeShockCount))

print(cue_max_per_animal_day)

cue_max_per_day_allVF_animals <- VF_no_control %>% group_by( date) %>% 
  summarise(max_audio = max(cumulativeAudioCount),
            max_pulse = max(cumulativeShockCount))

cue_max_per_day_allVF_animals

### make this df long
str(cue_max_per_day_allVF_animals)

cue_max_per_day_allVF_animals_long <- cue_max_per_day_allVF_animals %>% 
  pivot_longer(
    cols = c( "max_audio",  "max_pulse"),
    names_to = "cue",
    values_to = "cumulative_max")

### Plots




VF_dates=data.frame(date=as.Date(c("2020-10-21", "2020-10-25", "2020-10-30", "2020-11-03")), 
                    event=c("VF1", "VF2", "VF3", "VF Deactivated"))

cue_max_per_day_allVF_animals_long %>% 
#filter( count <50) %>% 
  ggplot( aes(x = date, y = cumulative_max, fill = cue))+
  #geom_boxplot(alpha = 0.2)+
  geom_col()+
  #geom_point()+
  theme_bw()+
  #facet_wrap(. ~ animal_ID)+
  geom_vline(data=VF_dates, mapping=aes(xintercept=date), color="blue", alpha = 0.2) +
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  #,legend.position = "none")+
  labs(title= "Pinnaroo2021 for the mob cumulative audio and pulse",
       x= "Date",
       y = "Cummulative audio cues")


### not super keen on this

#### summaries the data - Non cumlative data
str(VF_no_control)

cue_max_per_animal_day_not_cum <- VF_no_control %>% group_by( date, animal) %>% 
  summarise(sum_audio = sum(Audio_values, na.rm = TRUE),
            sum_pulse = sum(Shock_values, na.rm = TRUE))

print(cue_max_per_animal_day_not_cum)

cue_max_per_animal_day_not_cum_all_animals <- VF_no_control %>% group_by( date) %>% 
  summarise(sum_audio = sum(Audio_values, na.rm = TRUE),
            sum_pulse = sum(Shock_values, na.rm = TRUE))

cue_max_per_animal_day_not_cum_all_animals

### make this df long
str(cue_max_per_animal_day_not_cum_all_animals)

cue_max_per_animal_day_not_cum_all_animals_long <- cue_max_per_animal_day_not_cum_all_animals %>% 
  pivot_longer(
    cols = c( "sum_audio",  "sum_pulse"),
    names_to = "cue",
    values_to = "sum_cue")

cue_max_per_animal_day_not_cum_all_animals_long

cue_max_per_animal_day_not_cum_all_animals_long %>% 
  #filter( count <50) %>% 
  ggplot( aes(x = date, y = sum_cue, fill = cue))+
  #geom_boxplot(alpha = 0.2)+
  geom_col()+
  #geom_point()+
  theme_bw()+
  #facet_wrap(. ~ animal_ID)+
  geom_vline(data=VF_dates, mapping=aes(xintercept=date), color="blue", alpha = 0.2) +
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  #,legend.position = "none")+
  labs(title= "Pinnaroo2021 for the mob sum audio and pulse per day",
       x= "Date",
       y = "sum of cues per day for all animals")


## by animal

cue_max_per_animal_day_not_cum_long <- cue_max_per_animal_day_not_cum %>% 
  pivot_longer(cols = c( "sum_audio",  "sum_pulse"),
               names_to = "cue",
               values_to = "sum_cue")

cue_max_per_animal_day_not_cum_long

cue_max_per_animal_day_not_cum_long %>% 
  #filter(  <50) %>% 
  ggplot( aes(x = date , y = sum_cue, fill = cue))+
  #geom_boxplot(alpha = 0.2)+
  geom_col()+
  #geom_point()+
  theme_bw()+
  facet_wrap(. ~ animal)+
  geom_vline(data=VF_dates, mapping=aes(xintercept=date), color="blue", alpha = 0.2) +
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  #,legend.position = "none")+
  labs(title= "Pinnaroo2021 for the mob sum audio and pulse per day",
       x= "Date",
       y = "sum of cues per day for all animals")


cue_max_per_animal_day_not_cum_long$date1 <- as.character(cue_max_per_animal_day_not_cum_long$date)
unique(cue_max_per_animal_day_not_cum_long$date)

cue_max_per_animal_day_not_cum_long %>% 
  #filter( date< "2020-11-04" ) %>% 
  ggplot( aes(x = as.factor(animal) , y = sum_cue, fill = cue))+
  geom_col()+
  theme_bw()+
  facet_wrap(. ~ date1)+
  #geom_vline(data=VF_dates, mapping=aes(xintercept=date), color="blue", alpha = 0.2) +
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  #,legend.position = "none")+
  labs(title= "Pinnaroo2021 sum audio and pulse per day per animal",
       x= "animals",
       y = "sum of cues per day for all animals")



cue_max_per_animal_day_not_cum_long %>% 
  #filter( date< "2020-11-04" ) %>% 
  filter(animal == "9380142") %>% 
  ggplot( aes(x = date , y = sum_cue, fill = cue))+
  geom_col()+
  theme_bw()+
  facet_wrap(. ~ as.factor(animal))+
  geom_vline(data=VF_dates, mapping=aes(xintercept=date), color="blue", alpha = 0.2, size=1.5) +
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  #,legend.position = "none")+
  labs(title= "Pinnaroo2021 sum audio and pulse per day per animal",
       x= "animals",
       y = "sum of cues per day for all animals")


##Try this again...


#### summaries the data - cumulative data
str(VF_no_control)

##what is the min time for each date and VF
min_time_step_Vf <- VF_no_control %>% group_by( date, VF) %>% 
  summarise(min_time = min(time_step))

min_time_step_Vf
            
            cue_max_min_per_animal_day_cum <- VF_no_control %>% group_by( date, animal) %>% 
  summarise(
            min_time = min(time_step)
    
            # max_audio = max(cumulativeAudioCount),
            # min_audio = min(cumulativeAudioCount),
            # max_pulse = max(cumulativeShockCount),
            # min_pulse = min(cumulativeShockCount)
            )

print(cue_max_min_per_animal_day_cum)

