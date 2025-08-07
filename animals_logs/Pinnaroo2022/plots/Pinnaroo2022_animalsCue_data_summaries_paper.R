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


summary_of_cues_by_animals <-  VF_no_control %>% 
  group_by(animal) %>% 
  summarise(sum_aduio = sum( Audio_values, na.rm = TRUE),
            sum_pulse = sum( Shock_values, na.rm = TRUE))
summary_of_cues_by_animals


Av_of_cues_per_VFmob <- summary_of_cues_by_animals %>% 
  group_by() %>% 
  summarise(mean_aduio = mean( sum_aduio, na.rm = TRUE),
            mean_pulse = mean( sum_pulse, na.rm = TRUE))
Av_of_cues_per_VFmob



################################################################################
## how long was the VF active for?
names(VF_no_control)
start <- min(VF_no_control$local_time, na.rm = TRUE)  # 
end <-   max(VF_no_control$local_time, na.rm = TRUE) # 

time.interval <- start %--% end
time.interval
time.duration <- as.duration(time.interval)
time.duration #747000s (~1.24 weeks)
seconds_in_day <- 86400
#This is a bit sloppy but 86400 seconds in a day
days <- time.duration/seconds_in_day
days
days_round <- as.numeric(round(days, 0))
days_round #9 days
#####

summary_of_cues_per_animal_use_cum <-  VF_no_control %>% 
  group_by(animal) %>% 
  summarise(max_aduio = max( cumulativeAudioCount, na.rm = TRUE),
            max_pulse = max( cumulativeShockCount, na.rm = TRUE))
summary_of_cues_per_animal_use_cum

Av_of_cues_per_VFmob_use_cum <- summary_of_cues_per_animal_use_cum %>% 
  group_by() %>% 
  summarise(mean_aduio = mean( max_aduio, na.rm = TRUE),
            mean_pulse = mean( max_pulse, na.rm = TRUE),
            
            sd_aduio = sd(max_aduio, na.rm = TRUE),
            sd_pulse = sd(max_pulse, na.rm = TRUE),
            
            aduio_SE = sd_aduio/ sqrt(n()),
            pulse_SE = sd_pulse/ sqrt(n()),
            
            percet_pulse = ((mean_pulse / mean_aduio)*100),
            percet_audio = ((mean_aduio/ (mean_aduio + mean_pulse)*100)),
                            
            mean_aduio_per_day = mean( max_aduio, na.rm = TRUE)/days_round, #number of day the VF was active
            mean_pulse_per_day = mean( max_pulse, na.rm = TRUE)/days_round, #number of day the VF was active
            days = days_round 
            )
            
Av_of_cues_per_VFmob_use_cum

write_csv(Av_of_cues_per_VFmob_use_cum,
          "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/Av_of_cues_per_VFmob_use_cum.csv") 


################################################################################
# By VF 1
names(VF_no_control)
VF1_no_control <-  VF_no_control %>% filter(VF=="VF1")


names(VF1_no_control)
start <- min(VF1_no_control$local_time, na.rm = TRUE)  # 
end <-   max(VF1_no_control$local_time, na.rm = TRUE) # 

time.interval <- start %--% end
time.interval
time.duration <- as.duration(time.interval)
time.duration # 162000s (~1.88 days)"
seconds_in_day <- 86400
#This is a bit sloppy but 86400 seconds in a day
days <- time.duration/seconds_in_day
days_round <- as.numeric(round(days, 0))
days_round #2
#####


VF1_summary_of_cues_per_animal_use_cum <-  VF_no_control %>% 
  filter(VF=="VF1") %>% 
  group_by(animal) %>% 
  summarise(max_aduio = max( cumulativeAudioCount, na.rm = TRUE),
            max_pulse = max( cumulativeShockCount, na.rm = TRUE))
VF1_summary_of_cues_per_animal_use_cum





VF1_Av_of_cues_per_VFmob_use_cum <- VF1_summary_of_cues_per_animal_use_cum %>% 
  group_by() %>% 
  summarise(mean_aduio = mean( max_aduio, na.rm = TRUE),
            mean_pulse = mean( max_pulse, na.rm = TRUE),
            
            sd_aduio = sd(max_aduio, na.rm = TRUE),
            sd_pulse = sd(max_pulse, na.rm = TRUE),
            
            aduio_SE = sd_aduio/ sqrt(n()),
            pulse_SE = sd_pulse/ sqrt(n()),
            
            percet_pulse = ((mean_pulse / mean_aduio)*100),
            percet_audio = ((mean_aduio/ (mean_aduio + mean_pulse)*100)),
            
            mean_aduio_per_day = mean( max_aduio, na.rm = TRUE)/days_round, #number of day the VF was active
            mean_pulse_per_day = mean( max_pulse, na.rm = TRUE)/days_round, #number of day the VF was active
  
            days = days_round 
  )

VF1_Av_of_cues_per_VFmob_use_cum

write_csv(VF1_Av_of_cues_per_VFmob_use_cum,
          "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/Av_of_cues_per_VF1mob_use_cum.csv") 



################################################################################
# By VF 2
names(VF_no_control)
VF2_no_control <-  VF_no_control %>% filter(VF=="VF2")

start <- min(VF2_no_control$local_time, na.rm = TRUE)  # 
end <-   max(VF2_no_control$local_time, na.rm = TRUE) # 

time.interval <- start %--% end
time.interval
time.duration <- as.duration(time.interval)
time.duration #1200s (~20 minutes)"
seconds_in_day <- 86400
#This is a bit sloppy but 86400 seconds in a day
days <- time.duration/seconds_in_day
days_round <- as.numeric(round(days, 0))
days_round
#####

names(VF_no_control)

VF2_summary_of_cues_per_animal_use_cum <-  VF_no_control %>% 
  filter(VF=="VF2") %>% 
  filter(!is.na(animal)) %>% 
  group_by(animal) %>% 
  summarise(max_aduio = max( cumulativeAudioCount, na.rm = TRUE),
            max_pulse = max( cumulativeShockCount, na.rm = TRUE))
VF2_summary_of_cues_per_animal_use_cum

VF2_Av_of_cues_per_VFmob_use_cum <- VF2_summary_of_cues_per_animal_use_cum %>% 
  group_by() %>% 
  summarise(mean_aduio = mean( max_aduio, na.rm = TRUE),
            mean_pulse = mean( max_pulse, na.rm = TRUE),
            
            sd_aduio = sd(max_aduio, na.rm = TRUE),
            sd_pulse = sd(max_pulse, na.rm = TRUE),
            
            aduio_SE = sd_aduio/ sqrt(n()),
            pulse_SE = sd_pulse/ sqrt(n()),
            
            percet_pulse = ((mean_pulse / mean_aduio)*100),
            percet_audio = ((mean_aduio/ (mean_aduio + mean_pulse)*100)),
            
            mean_aduio_per_day = mean( max_aduio, na.rm = TRUE)/5, #number of day the VF was active
            mean_pulse_per_day = mean( max_pulse, na.rm = TRUE)/days_round, #number of day the VF was active
            
            days = days_round 
  )

VF2_Av_of_cues_per_VFmob_use_cum

write_csv(VF2_Av_of_cues_per_VFmob_use_cum,
          "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/Av_of_cues_per_VF2mob_use_cum.csv") 


################################################################################
# By VF 3
names(VF_no_control)
VF3_no_control <-  VF_no_control %>% filter(VF=="VF3")

start <- min(VF3_no_control$local_time, na.rm = TRUE)  # 
end <-   max(VF3_no_control$local_time, na.rm = TRUE) # 

time.interval <- start %--% end
time.interval
time.duration <- as.duration(time.interval)
time.duration # 247800s (~2.87 days)
seconds_in_day <- 86400
#This is a bit sloppy but 86400 seconds in a day
days <- time.duration/seconds_in_day
days_round <- as.numeric(round(days, 0))
days_round #3
##### 



VF3_summary_of_cues_per_animal_use_cum <-  VF_no_control %>% 
  filter(VF=="VF3") %>% 
  filter(!is.na(animal)) %>% 
  group_by(animal) %>% 
  summarise(max_aduio = max( cumulativeAudioCount, na.rm = TRUE),
            max_pulse = max( cumulativeShockCount, na.rm = TRUE))
VF3_summary_of_cues_per_animal_use_cum

VF3_Av_of_cues_per_VFmob_use_cum <- VF3_summary_of_cues_per_animal_use_cum %>% 
  group_by() %>% 
  summarise(mean_aduio = mean( max_aduio, na.rm = TRUE),
            mean_pulse = mean( max_pulse, na.rm = TRUE),
            
            sd_aduio = sd(max_aduio, na.rm = TRUE),
            sd_pulse = sd(max_pulse, na.rm = TRUE),
            
            aduio_SE = sd_aduio/ sqrt(n()),
            pulse_SE = sd_pulse/ sqrt(n()),
            
            percet_pulse = ((mean_pulse / mean_aduio)*100),
            percet_audio = ((mean_aduio/ (mean_aduio + mean_pulse)*100)),
            
            mean_aduio_per_day = mean( max_aduio, na.rm = TRUE)/days_round, #number of day the VF was active
            mean_pulse_per_day = mean( max_pulse, na.rm = TRUE)/days_round, #number of day the VF was active
            
            days = days_round 
  )

VF3_Av_of_cues_per_VFmob_use_cum

write_csv(VF3_Av_of_cues_per_VFmob_use_cum,
          "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/Av_of_cues_per_VF3mob_use_cum.csv") 




################################################################################
# By VF 4
names(VF_no_control)
VF4_no_control <-  VF_no_control %>% filter(VF=="VF4")

start <- min(VF4_no_control$local_time, na.rm = TRUE)  # 
end <-   max(VF4_no_control$local_time, na.rm = TRUE) # 

time.interval <- start %--% end
time.interval
time.duration <- as.duration(time.interval)
time.duration #88800s (~1.03 days)"
seconds_in_day <- 86400
#This is a bit sloppy but 86400 seconds in a day
days <- time.duration/seconds_in_day
days_round <- as.numeric(round(days, 0))
days_round #1
#####



VF4_summary_of_cues_per_animal_use_cum <-  VF_no_control %>% 
  filter(VF=="VF4") %>% 
  filter(!is.na(animal)) %>% 
  group_by(animal) %>% 
  summarise(max_aduio = max( cumulativeAudioCount, na.rm = TRUE),
            max_pulse = max( cumulativeShockCount, na.rm = TRUE))
VF4_summary_of_cues_per_animal_use_cum

VF4_Av_of_cues_per_VFmob_use_cum <- VF4_summary_of_cues_per_animal_use_cum %>% 
  group_by() %>% 
  summarise(mean_aduio = mean( max_aduio, na.rm = TRUE),
            mean_pulse = mean( max_pulse, na.rm = TRUE),
            
            sd_aduio = sd(max_aduio, na.rm = TRUE),
            sd_pulse = sd(max_pulse, na.rm = TRUE),
            
            aduio_SE = sd_aduio/ sqrt(n()),
            pulse_SE = sd_pulse/ sqrt(n()),
            
            percet_pulse = ((mean_pulse / mean_aduio)*100),
            percet_audio = ((mean_aduio/ (mean_aduio + mean_pulse)*100)),
            
            mean_aduio_per_day = mean( max_aduio, na.rm = TRUE)/days_round, #number of day the VF was active
            mean_pulse_per_day = mean( max_pulse, na.rm = TRUE)/days_round, #number of day the VF was active
            
            days = days_round 
  )

VF4_Av_of_cues_per_VFmob_use_cum

write_csv(VF4_Av_of_cues_per_VFmob_use_cum,
          "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/Av_of_cues_per_VF4mob_use_cum.csv") 


################################################################################
# By VF 5
names(VF_no_control)
VF5_no_control <-  VF_no_control %>% filter(VF=="VF5")

start <- min(VF5_no_control$local_time, na.rm = TRUE)  # 
end <-   max(VF5_no_control$local_time, na.rm = TRUE) # 

time.interval <- start %--% end
time.interval
time.duration <- as.duration(time.interval)
time.duration #89400s (~1.03 days)"
seconds_in_day <- 86400
#This is a bit sloppy but 86400 seconds in a day
days <- time.duration/seconds_in_day
days_round <- as.numeric(round(days, 0))
days_round #1
#####



V5_summary_of_cues_per_animal_use_cum <-  VF_no_control %>% 
  filter(VF=="VF5") %>% 
  filter(!is.na(animal)) %>% 
  group_by(animal) %>% 
  summarise(max_aduio = max( cumulativeAudioCount, na.rm = TRUE),
            max_pulse = max( cumulativeShockCount, na.rm = TRUE))
V5_summary_of_cues_per_animal_use_cum

V5_summary_of_cues_per_animal_use_cum <- V5_summary_of_cues_per_animal_use_cum %>% 
  group_by() %>% 
  summarise(mean_aduio = mean( max_aduio, na.rm = TRUE),
            mean_pulse = mean( max_pulse, na.rm = TRUE),
            
            sd_aduio = sd(max_aduio, na.rm = TRUE),
            sd_pulse = sd(max_pulse, na.rm = TRUE),
            
            aduio_SE = sd_aduio/ sqrt(n()),
            pulse_SE = sd_pulse/ sqrt(n()),
            
            percet_pulse = ((mean_pulse / mean_aduio)*100),
            percet_audio = ((mean_aduio/ (mean_aduio + mean_pulse)*100)),
            
            mean_aduio_per_day = mean( max_aduio, na.rm = TRUE)/days_round, #number of day the VF was active
            mean_pulse_per_day = mean( max_pulse, na.rm = TRUE)/days_round, #number of day the VF was active
            
            days = days_round 
  )

V5_summary_of_cues_per_animal_use_cum

write_csv(V5_summary_of_cues_per_animal_use_cum,
          "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/Av_of_cues_per_VF5mob_use_cum.csv") 


################################################################################
# By VF 6
names(VF_no_control)
VF6_no_control <-  VF_no_control %>% filter(VF=="VF6")

start <- min(VF6_no_control$local_time, na.rm = TRUE)  # 
end <-   max(VF6_no_control$local_time, na.rm = TRUE) # 

time.interval <- start %--% end
time.interval
time.duration <- as.duration(time.interval)
time.duration #152400s (~1.76 days)"
seconds_in_day <- 86400
#This is a bit sloppy but 86400 seconds in a day
days <- time.duration/seconds_in_day
days_round <- as.numeric(round(days, 0))
days_round #2
#####



V6_summary_of_cues_per_animal_use_cum <-  VF_no_control %>% 
  filter(VF=="VF6") %>% 
  filter(!is.na(animal)) %>% 
  group_by(animal) %>% 
  summarise(max_aduio = max( cumulativeAudioCount, na.rm = TRUE),
            max_pulse = max( cumulativeShockCount, na.rm = TRUE))
V6_summary_of_cues_per_animal_use_cum

V6_summary_of_cues_per_animal_use_cum <- V6_summary_of_cues_per_animal_use_cum %>% 
  group_by() %>% 
  summarise(mean_aduio = mean( max_aduio, na.rm = TRUE),
            mean_pulse = mean( max_pulse, na.rm = TRUE),
            
            sd_aduio = sd(max_aduio, na.rm = TRUE),
            sd_pulse = sd(max_pulse, na.rm = TRUE),
            
            aduio_SE = sd_aduio/ sqrt(n()),
            pulse_SE = sd_pulse/ sqrt(n()),
            
            percet_pulse = ((mean_pulse / mean_aduio)*100),
            percet_audio = ((mean_aduio/ (mean_aduio + mean_pulse)*100)),
            
            mean_aduio_per_day = mean( max_aduio, na.rm = TRUE)/days_round, #number of day the VF was active
            mean_pulse_per_day = mean( max_pulse, na.rm = TRUE)/days_round, #number of day the VF was active
            
            days = days_round 
  )

V6_summary_of_cues_per_animal_use_cum

write_csv(V6_summary_of_cues_per_animal_use_cum,
          "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/Av_of_cues_per_VF6mob_use_cum.csv") 

Av_of_cues_per_VFmob_use_cum <- Av_of_cues_per_VFmob_use_cum %>% mutate(VF= "all")
VF1_Av_of_cues_per_VFmob_use_cum <- VF1_Av_of_cues_per_VFmob_use_cum %>% mutate(VF= "VF1")
VF2_Av_of_cues_per_VFmob_use_cum <- VF2_Av_of_cues_per_VFmob_use_cum %>% mutate(VF= "VF2")
VF3_Av_of_cues_per_VFmob_use_cum <- VF3_Av_of_cues_per_VFmob_use_cum %>% mutate(VF= "VF3")
VF4_Av_of_cues_per_VFmob_use_cum <- VF4_Av_of_cues_per_VFmob_use_cum %>% mutate(VF= "VF4")
V5_summary_of_cues_per_animal_use_cum <- V5_summary_of_cues_per_animal_use_cum %>% mutate(VF= "VF5")
V6_summary_of_cues_per_animal_use_cum <- V6_summary_of_cues_per_animal_use_cum %>% mutate(VF= "VF6")


V1_6_summary_of_cues_per_animal_use_cum <- rbind(
  Av_of_cues_per_VFmob_use_cum,
  VF1_Av_of_cues_per_VFmob_use_cum,
  VF2_Av_of_cues_per_VFmob_use_cum,
  VF3_Av_of_cues_per_VFmob_use_cum,
  VF4_Av_of_cues_per_VFmob_use_cum,
  V5_summary_of_cues_per_animal_use_cum,
  V6_summary_of_cues_per_animal_use_cum
)


write_csv(V1_6_summary_of_cues_per_animal_use_cum,
          "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/AllVF_1_6Av_of_cues_per_mob_use_cum.csv") 
