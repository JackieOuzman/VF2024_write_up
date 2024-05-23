library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)




VF_all <-  readRDS("W:/VF/2024/animal behaviour data/Long Plain/data_for_plots/LP_all_data.rds") 
VF_no_control <- VF_all %>% filter(VF != "VFControl")



###audio (best option)
str(VF_no_control)

summary_of_cues_per_animal_use_cum <-  VF_no_control %>% 
  group_by(animal, date, DOY) %>% 
  summarise(max_aduio = max( cumulativeAudioCount, na.rm = TRUE),
            max_pulse = max( cumulativeShockCount, na.rm = TRUE),
            
            percet_pulse_to_audio = ((max_pulse / max_aduio)*100),
            percet_audio_to_totalCues = ((max_aduio/ (max_aduio + max_pulse)*100))
            )


summary_of_cues_per_animal_use_cum <- ungroup(summary_of_cues_per_animal_use_cum)
summary_of_cues_per_animal_use_cum <- summary_of_cues_per_animal_use_cum %>% filter( date< "2020-11-04" )

VF_dates=data.frame(date=as.Date(c("2020-10-21", "2020-10-25", "2020-10-30", "2020-11-03")), 
                    event=c("VF1", "VF2", "VF3", "VF Deactivated"))


#what are the cow with high values?


high_cows_audio <- summary_of_cues_per_animal_use_cum %>% 
  filter(max_aduio> 1000) %>%   distinct(animal)

high_cows_audio_summary <-  summary_of_cues_per_animal_use_cum %>% 
  filter(animal %in% c(9380142, 9380268, 9380384, 9380455)) %>% 
  filter( date< "2020-11-04" )

### Plot with audio (best option)

Audio_LP <- high_cows_audio_summary %>% 
  ggplot( aes(x = date, y = max_aduio, group = as.factor(animal)))+
  geom_line(data= summary_of_cues_per_animal_use_cum , 
            aes(x = date, 
                y = max_aduio, 
                group = as.factor(animal)),
            #size = 2,
            colour = "black",alpha = 0.2,
            #linetype = "dotted"
            )+
  geom_line(aes(color=as.factor(animal)))+
  #geom_line()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_date(date_breaks = "1 day", date_labels = "%d/%m")+
  geom_vline(data=VF_dates, mapping=aes(xintercept=date), color="Black",  size=1) +
  theme(axis.text.x=element_text(angle=90,hjust=1))+ 
  labs(title= "",
       x= "",
       y = "Cummulative audio cues",col="Animal ID",)










###Pulse 
str(VF_no_control)

VF_dates=data.frame(date=as.Date(c("2020-10-21", "2020-10-25", "2020-10-30", "2020-11-03")), 
                    event=c("VF1", "VF2", "VF3", "VF Deactivated"))


#what are the cow with high values?


high_cows_pulse <- summary_of_cues_per_animal_use_cum %>% 
  filter(max_pulse> 100) %>%   distinct(animal) #same cows as audio

high_cows_audio_summary <-  summary_of_cues_per_animal_use_cum %>% 
  filter(animal %in% c(9380142, 9380268, 9380384, 9380455)) %>% 
  filter( date< "2020-11-04" )

### Plot with pulse

Pulse_LP <- high_cows_audio_summary %>% 
  ggplot( aes(x = date, y = max_pulse, group = as.factor(animal)))+
  geom_line(data= summary_of_cues_per_animal_use_cum , 
            aes(x = date, 
                y = max_pulse, 
                group = as.factor(animal)),
            #size = 2,
            colour = "black",alpha = 0.2,
            #linetype = "dotted"
  )+
  geom_line(aes(color=as.factor(animal)))+
  #geom_line()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_date(date_breaks = "1 day", date_labels = "%d/%m")+
  geom_vline(data=VF_dates, mapping=aes(xintercept=date), color="Black",  size=1) +
  theme(axis.text.x=element_text(angle=90,hjust=1))+ 
  labs(title= "",
       x= "",
       y = "Cummulative pulse cues",col="Animal ID",)
Pulse_LP

ggsave(Pulse_LP,
       filename = "W:/VF/2024/animal behaviour data/Long Plain/data_for_plots/CumPulse_vsDays_byanimals_LP.png", 
       width = 15, height = 8, dpi = 600, units = "cm")

Audio_LP
ggsave(Audio_LP,
       filename = "W:/VF/2024/animal behaviour data/Long Plain/data_for_plots/CumAudio_vsDays_byanimals_LP.png", 
       width = 15, height = 8, dpi = 600, units = "cm")
