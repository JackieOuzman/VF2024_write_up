#Animal cue data for the paper 2024

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
                            
            mean_aduio_per_day = mean( max_aduio, na.rm = TRUE)/14, #number of day the VF was active
            mean_pulse_per_day = mean( max_pulse, na.rm = TRUE)/14 #number of day the VF was active
            )
            
Av_of_cues_per_VFmob_use_cum

write_csv(Av_of_cues_per_VFmob_use_cum, 
          "W:/VF/2024/animal behaviour data/Long Plain/data_for_plots/Av_of_cues_per_VFmob_use_cum_LP.csv")

################################################################################
# By VF 1
names(VF_no_control)

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
            
            mean_aduio_per_day = mean( max_aduio, na.rm = TRUE)/4, #number of day the VF was active
            mean_pulse_per_day = mean( max_pulse, na.rm = TRUE)/4 #number of day the VF was active
  )

VF1_Av_of_cues_per_VFmob_use_cum


################################################################################
# By VF 2
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
            mean_pulse_per_day = mean( max_pulse, na.rm = TRUE)/5 #number of day the VF was active
  )


str(VF2_Av_of_cues_per_VFmob_use_cum)


################################################################################
# By VF 3
names(VF_no_control)

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
            
            mean_aduio_per_day = mean( max_aduio, na.rm = TRUE)/5, #number of day the VF was active
            mean_pulse_per_day = mean( max_pulse, na.rm = TRUE)/5 #number of day the VF was active
  )


str(VF3_Av_of_cues_per_VFmob_use_cum)
