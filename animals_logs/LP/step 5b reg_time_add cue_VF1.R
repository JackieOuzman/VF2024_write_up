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
#### --------------    Bring in data   -------------- ####
################################################################################


GPS_Dist <- readRDS("W:/VF/2024/animal behaviour data/Long Plain/data_prep/VF1step3_clip.rds")
names(GPS_Dist)

### subset the data to the clms I want.
GPS_Dist <- GPS_Dist %>%  rename(cumulativeAudioCount = cumulative ,
                       cumulativeShockCount = cumulati_1)

GPS_Dist <- GPS_Dist %>% dplyr::select (ID_jaxs, animal_ID, 
                                        #DOT, #I dont have this
                                        local_time, 
                                        date,
                                        DOY, 
                                        cumulativeAudioCount,
                                        cumulativeShockCount,
                                        # Audio_values,
                                        # Shock_values  
                                        )

GPS_Dist$local_time <- as.POSIXct(GPS_Dist$local_time,  tz = "Australia/Adelaide")

str(GPS_Dist)



################################################################################
#### --------------    what is the length of the trail?   -------------- ####
################################################################################


start <- min(GPS_Dist$local_time, na.rm = TRUE)  # "2020-10-21 14:57:00 ACDT"
end <-   max(GPS_Dist$local_time, na.rm = TRUE) # "2020-10-25 10:47:16 ACDT"
#Since we’re dealing with elapsed time between two dates, let’s start with Intervals. We can define an Interval using the %--% operator.
start <- round_date(start, unit="10 mins") #2022-06-28 10:00:00 ACDT"
end <- round_date(end, unit="10 mins") # "2022-07-02 10:10:00 ACDT"

time.interval <- start %--% end
time.interval
#To create a Duration between these two dates, we can use the as.duration function.

time.duration <- as.duration(time.interval)
time.duration # "330600s (~3.83 days)"

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
# Need to round the local time to the closest 10 min
GPS_Dist <- GPS_Dist %>% 
  dplyr::mutate(round_local_time =round_date(local_time, unit="10 mins"),
                Time_animal = paste0(round_local_time,"_", animal_ID) )


rm(end,start, time.duration, time.interval)


###############################################################################  
## ----- function to produce steps per animal
################################################################################

animal_list <- GPS_Dist %>% distinct(animal_ID) %>%  arrange(animal_ID)
### 20 animals ID I need regular time interval for each animals
### List of sites I want to run analysis for:
animal_list
### Any problems because the collar were swapped over?####
str(GPS_Dist)






#animals_list <- c(1:20) #c(1:6)# should this be 20?
#animals_list <- 9370004 #for testing
animals_list <- as.numeric(animal_list[1:20,])

#### as a function
for (animals_list in animals_list){
  
################################################################################  
#regular_time_interval_per_animal_ID
################################################################################
  regular_time_interval_animal <- regular_time_interval %>% 
    dplyr::mutate(Time_animal = paste0(time_step,"_", animals_list))
  
################################################################################
#### --------------    Join regular time step to dataset  -------------- ####
################################################################################  
  
  GPS_animal <- GPS_Dist %>%  filter(animal_ID == animals_list)
  
   
  ## the occurrence of a duplicated time_animal
  
  GPS_animal <- GPS_animal %>% 
    distinct(Time_animal, .keep_all = TRUE)
 
  
  
  GPS_animal[ is.na(GPS_animal) ] <- 0
 
  GPS_animal_reg_time <- left_join(regular_time_interval_animal, GPS_animal)

  #### Trim the regular time step to match the end of animal time

  start_animal <- min(GPS_animal$local_time, na.rm = TRUE)  
  end_animal <-   max(GPS_animal$local_time, na.rm = TRUE) 
  start_animal <- round_date(start_animal, unit="10 mins")
  end_animal <- round_date(end_animal, unit="10 mins") 
  
  ## trim the joined data to the animal ID time in the trial 
  
  
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
  name <- paste0("GPS_animal_reg_time_step", animals_list)
  assign(name,GPS_animal_reg_time)
  
  }       



# write.csv(GPS_animal_reg_time_step9370004, 
#           paste0("W:/VF/2024/animal behaviour data/Long Plain/data_prep/checking_step3_VF1/WED_step5b_Greg_time_step_dist_travelled_withCue.csv"), 
#           row.names=FALSE)




#file_list <- data.frame(name_df = paste0("GPS_sheep_reg_time_step",c(1:6)))

#9370004
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



GPS_animal_reg_time_step_all <- rbind(
  GPS_animal_reg_time_step9370004,
  GPS_animal_reg_time_step9370088,
  GPS_animal_reg_time_step9370123,
  GPS_animal_reg_time_step9380142,

  GPS_animal_reg_time_step9380144,
  GPS_animal_reg_time_step9380265,
  GPS_animal_reg_time_step9380268,
  GPS_animal_reg_time_step9380297,
  
  GPS_animal_reg_time_step9380384,
  GPS_animal_reg_time_step9380451,
  GPS_animal_reg_time_step9380455,
  GPS_animal_reg_time_step9380461,
 
  GPS_animal_reg_time_step9380477,
  GPS_animal_reg_time_step9380495,
  GPS_animal_reg_time_step9380591,
  GPS_animal_reg_time_step9380611,
 
  GPS_animal_reg_time_step9380713,
  GPS_animal_reg_time_step9380744,
  GPS_animal_reg_time_step9380754,
  GPS_animal_reg_time_step9380796
  )




rm(GPS_animal_reg_time_step9370004,
   GPS_animal_reg_time_step9370088,
   GPS_animal_reg_time_step9370123,
   GPS_animal_reg_time_step9380142,
   
   GPS_animal_reg_time_step9380144,
   GPS_animal_reg_time_step9380265,
   GPS_animal_reg_time_step9380268,
   GPS_animal_reg_time_step9380297,
   
   GPS_animal_reg_time_step9380384,
   GPS_animal_reg_time_step9380451,
   GPS_animal_reg_time_step9380455,
   GPS_animal_reg_time_step9380461,
   
   GPS_animal_reg_time_step9380477,
   GPS_animal_reg_time_step9380495,
   GPS_animal_reg_time_step9380591,
   GPS_animal_reg_time_step9380611,
   
   GPS_animal_reg_time_step9380713,
   GPS_animal_reg_time_step9380744,
   GPS_animal_reg_time_step9380754,
   GPS_animal_reg_time_step9380796
   )




GPS_animal_reg_time_step_all <- GPS_animal_reg_time_step_all %>% 
  dplyr::mutate(Time_sheep = paste0(round_local_time,"_", sheep) )

unique(GPS_animal_reg_time_step_all$animal_ID)







# write.csv(GPS_animal_reg_time_step_all, 
#           paste0(output_path,"/step5b_reg_time_step_dist_travelled_withCue_VF1.csv"), 
#           row.names=FALSE)

saveRDS(GPS_animal_reg_time_step_all,  "W:/VF/2024/animal behaviour data/Long Plain/data_prep/VF1_step5b.rds")

