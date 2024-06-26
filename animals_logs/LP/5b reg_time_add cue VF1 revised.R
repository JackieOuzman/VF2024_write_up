### reg time step and distance travelled

################################################################################
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)
################################################################################



################################################################################
#### --------------    Bring in data   -------------- ####? Really step 3 not 4?
################################################################################
GPS_Dist <- readRDS("W:/VF/2024/animal behaviour data/Long Plain/data_prep/VF1step3_clip.rds")

names(GPS_Dist)

### subset the data to the clms I want.

GPS_Dist <- GPS_Dist %>% dplyr::select (ID_jaxs, animal_ID, 
                                        #DOT, #I dont have this
                                        local_time, 
                                        date,
                                        DOY, 
                                        #event,
                                        cumulativeAudioCount, #need to make this
                                        cumulativeShockCount, #need to make this
                                        
                                        # Audio_values,
                                        # Shock_values  
                                        )

## testing out an idea to 
#List of animals

# 9370004
# 9370088
# 9370123
# 9380142

# 9380144
# 9380265
# 9380268
# 9380297

# 9380384
# 9380451
# 9380455
# 9380461

# 9380477
# 9380495
# 9380591
# 9380611

# 9380713
# 9380744
# 9380754
# 9380796


GPS_Dist_9370004 <- GPS_Dist %>% filter(animal_ID==9370004)




### make a cumlative audio and pulse from the event clms

unique(GPS_Dist$event) #"InclusionBorder_m is no pulse or audio

GPS_Dist <- mutate(
  GPS_Dist,
  Audio = case_when(
    event == "Audio started" ~ 1,
    event == "Audio started (short)" ~ 1,
    event == "Audio started (short) [simulated]" ~ 1,
    event == "Audio started [simulated]" ~ 1
  )
)


GPS_Dist <- mutate(GPS_Dist,
                   Pulse = case_when(event == "Pulse started" ~ 1,
                                     event == "Pulse started [simulated]" ~ 1))


### by animals cal the running cumulative totals
GPS_Dist <- GPS_Dist %>% 
  #arrange(animal_ID, local_time) %>% 
  group_by(animal_ID) %>% 
  mutate(cumulativeAudioCount1 = cumsum(ifelse(is.na(Audio), 0, Audio)) + Audio*0) #bit of extra code to deal with the NA

GPS_Dist <- GPS_Dist %>% 
  arrange(animal_ID, local_time) %>% 
  group_by(animal_ID) %>% 
  mutate(cumulativeShockCount1 = cumsum(ifelse(is.na(Pulse), 0, Pulse)) + Pulse*0) #bit of extra code to deal with the NA

  GPS_Dist <- ungroup(GPS_Dist)

### replace the missing data with the above cum value
  
GPS_Dist<- GPS_Dist %>% 
    mutate(cumulativeShockCount = cumulativeShockCount1) %>% 
    arrange(animal_ID, local_time) %>% 
    group_by(animal_ID) %>% 
    fill(cumulativeShockCount)

GPS_Dist<- GPS_Dist %>% 
  mutate(cumulativeAudioCount = cumulativeAudioCount1) %>% 
  arrange(animal_ID, local_time) %>% 
  group_by(animal_ID) %>% 
  fill(cumulativeAudioCount)
  

## remove the clms I no longer need
GPS_Dist <- GPS_Dist %>% 
  dplyr::select(-cumulativeShockCount1,
                -cumulativeAudioCount1,
                -Pulse,
                -Audio
                )

GPS_Dist <- ungroup(GPS_Dist)

str(GPS_Dist)
unique(GPS_Dist$event) #"InclusionBorder_m is no pulse or audio
unique(GPS_Dist$cumulativeAudioCount) #"check to see if above code is working - i.e. I have number so its working!
unique(GPS_Dist$cumulativeShockCount) #"

################################################################################
#### --------------    what is the length of the trail?   -------------- ####
################################################################################


start <- min(GPS_Dist$local_time, na.rm = TRUE)  # 
end <-   max(GPS_Dist$local_time, na.rm = TRUE) #
#Since we’re dealing with elapsed time between two dates, let’s start with Intervals. We can define an Interval using the %--% operator.
start <- round_date(start, unit="10 mins") #2019-05-20 10:40:00 ACST"
end <- round_date(end, unit="10 mins") # "2019-05-20 14:40:00 ACST"

time.interval <- start %--% end
time.interval
#To create a Duration between these two dates, we can use the as.duration function.

time.duration <- as.duration(time.interval)
time.duration # "14400s (~4 hours)"

################################################################################
#### --------------    make a regular time step   -------------- ####
################################################################################


regular_time_interval <-data.frame(time_step = seq(from = ymd_hms(start),
                                                   to = ymd_hms(end), 
                                                   by = '10 mins'))

################################################################################
#### ----   Need to round the local time to the closest 10 min  -------------- ####
################################################################################
str(GPS_Dist)
GPS_Dist <- GPS_Dist %>%  rename(animal = animal_ID)

# Need to round the local time to the closest 10 min
GPS_Dist <- GPS_Dist %>% 
  dplyr::mutate(round_local_time =round_date(local_time, unit="10 mins"),
                Time_animal = paste0(round_local_time,"_", animal) )


rm(end,start, time.duration, time.interval)


###############################################################################  
## ----- function to produce steps per animal
################################################################################

animal_list <- GPS_Dist %>% distinct(animal) %>%  arrange(animal)
dim(animal_list)

### List of sites I want to run analysis for:
animal_list

#animal_list <- "Q10"
animal_list <- animal_list$animal


#### as a function
for (animal_list in animal_list){
  
################################################################################  
#regular_time_interval_per_sheep_ID
################################################################################
  regular_time_interval_animal <- regular_time_interval %>% 
    dplyr::mutate(Time_animal = paste0(time_step,"_", animal_list))
  
################################################################################
#### --------------    Join regular time step to dataset  -------------- ####
################################################################################  
  
  GPS_animal <- GPS_Dist %>%  filter(animal == animal_list)
  
   
  ## the occurrence of a duplicated time_animal, but I want to retain the max audio and max pulse values - its a 3 step process
  
  ##for max audio
  GPS_animal_audio_retain_step1 <- GPS_animal %>% 
    group_by(Time_animal, ID_jaxs) %>% 
    summarise(cumulativeAudioCount_max = max(cumulativeAudioCount, na.rm = TRUE))
  
  GPS_animal_audio_retain <- GPS_animal_audio_retain_step1 %>% 
    distinct(Time_animal, .keep_all = TRUE)
  
  
  GPS_animal_audio_retain <- as.list(GPS_animal_audio_retain$ID_jaxs)
  GPS_animal_audio <- GPS_animal %>% filter(ID_jaxs %in% GPS_animal_audio_retain)
  GPS_animal_audio <- GPS_animal_audio %>% dplyr::select(-cumulativeShockCount )
  
  ##for max pulse
  
  GPS_animal_pulse_retain_step1 <- GPS_animal %>% 
    group_by(Time_animal, ID_jaxs) %>% 
    summarise(cumulativeShockCount_max = max(cumulativeShockCount, na.rm = TRUE))
  
  GPS_animal_pulse_retain <- GPS_animal_pulse_retain_step1 %>% 
    distinct(Time_animal, .keep_all = TRUE)
  
  
  GPS_animal_pulse_retain <- as.list(GPS_animal_pulse_retain$ID_jaxs)
  GPS_animal_pulse <- GPS_animal %>% filter(ID_jaxs %in% GPS_animal_pulse_retain)
  GPS_animal_pulse <- GPS_animal_pulse %>% dplyr::select(cumulativeShockCount, Time_animal )
  
  tail(GPS_animal_audio)
  tail(GPS_animal_pulse)
  
  GPS_animal <- left_join(GPS_animal_audio, GPS_animal_pulse)
  rm(GPS_animal_pulse_retain, GPS_animal_pulse_retain_step1, GPS_animal_pulse, 
     GPS_animal_audio_retain, GPS_animal_audio_retain_step1, GPS_animal_audio)
  
###############################################################################  
  
  
  
  GPS_animal[ is.na(GPS_animal) ] <- 0
 
  GPS_animal_reg_time <- left_join(regular_time_interval_animal, GPS_animal)

  #### Trim the regular time step to match the end of animal time

  start_animal <- min(GPS_animal$local_time, na.rm = TRUE)  
  end_animal <-   max(GPS_animal$local_time, na.rm = TRUE) 
  start_animal <- round_date(start_animal, unit="10 mins")
  end_animal <- round_date(end_animal, unit="10 mins") 
  
  ## trim the joined data to the animals ID time in the trial 
  
  
  GPS_animal_reg_time <- GPS_animal_reg_time %>% 
    dplyr::filter(between(time_step, ymd_hms(start_animal), ymd_hms(end_animal))) 
#-------------------------------------------------------------------------------- 
  ### add in the audio and pulse counts cal from cumulative 
#--------------------------------------------------------------------------------  
   str(GPS_animal_reg_time) 
  
  #FILL missing data 
  GPS_animal_reg_time <- GPS_animal_reg_time %>% 
    tidyr::fill(cumulativeAudioCount, .direction = "down")
  
  GPS_animal_reg_time <- GPS_animal_reg_time %>% 
    tidyr::fill(cumulativeShockCount, .direction = "down")
  
   GPS_animal_reg_time <- GPS_animal_reg_time %>% 
    mutate(Audio_values = cumulativeAudioCount - lag(cumulativeAudioCount))
  
  GPS_animal_reg_time <- GPS_animal_reg_time %>% 
    mutate(Shock_values = cumulativeShockCount - lag(cumulativeShockCount))  
    
    
##################################################################################################################
    

  rm(
    GPS_animal,
    regular_time_interval_animal
    )
  name <- paste0("GPS_animal_reg_time_step", animal_list)
  assign(name,GPS_animal_reg_time)
  
  }       








GPS_animal_reg_time_step_all <- rbind(
  GPS_animal_reg_time_stepQ10,
  GPS_animal_reg_time_stepQ108,
  GPS_animal_reg_time_stepQ109,
  GPS_animal_reg_time_stepQ11,
  GPS_animal_reg_time_stepQ110,
  GPS_animal_reg_time_stepQ15,
  GPS_animal_reg_time_stepQ2,
  GPS_animal_reg_time_stepQ26,
  GPS_animal_reg_time_stepQ27,
  GPS_animal_reg_time_stepQ28,
  GPS_animal_reg_time_stepQ29,
  GPS_animal_reg_time_stepQ36,
  GPS_animal_reg_time_stepQ42,
  GPS_animal_reg_time_stepQ45,
  GPS_animal_reg_time_stepQ46,
  GPS_animal_reg_time_stepQ47,
  GPS_animal_reg_time_stepQ51,
  GPS_animal_reg_time_stepQ75,
  GPS_animal_reg_time_stepQ8,
  GPS_animal_reg_time_stepQ9
  )




rm(GPS_animal_reg_time_stepQ10,
   GPS_animal_reg_time_stepQ108,
   GPS_animal_reg_time_stepQ109,
   GPS_animal_reg_time_stepQ11,
   GPS_animal_reg_time_stepQ110,
   GPS_animal_reg_time_stepQ15,
   GPS_animal_reg_time_stepQ2,
   GPS_animal_reg_time_stepQ26,
   GPS_animal_reg_time_stepQ27,
   GPS_animal_reg_time_stepQ28,
   GPS_animal_reg_time_stepQ29,
   GPS_animal_reg_time_stepQ36,
   GPS_animal_reg_time_stepQ42,
   GPS_animal_reg_time_stepQ45,
   GPS_animal_reg_time_stepQ46,
   GPS_animal_reg_time_stepQ47,
   GPS_animal_reg_time_stepQ51,
   GPS_animal_reg_time_stepQ75,
   GPS_animal_reg_time_stepQ8,
   GPS_animal_reg_time_stepQ9
   )


saveRDS(GPS_animal_reg_time_step_all,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step5b/VF1_step5b.rds")

