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
GPS_Dist <- readRDS("W:/VF/2024/animal behaviour data/Long Plain/data_prep/VF3_step4.rds")

names(GPS_Dist)

### subset the data to the clms I want.

GPS_Dist <- GPS_Dist %>% dplyr::select (ID_jaxs, animal_ID, 
                                        #DOT, #I dont have this
                                        local_time, 
                                        date,
                                        DOY, 
                                        X , 
                                        Y, 
                                        dist_to_VF, 
                                        VF_EX,
                                        #Audio_values,#bring in at the next step 5b
                                        #Shock_values, #bring in at the next step 5b
                                        # resting_percentage, #my dataset doesnt have this
                                        # moving_percentage,
                                        # grazing_percentage 
                                       )

GPS_Dist$local_time <- as.POSIXct(GPS_Dist$local_time,  tz = "Australia/Adelaide")
GPS_Dist <- GPS_Dist %>%  rename(animal = animal_ID)
str(GPS_Dist)
################################################################################
#### --------------    what is the length of the trail for VF X?   -------------- ####
################################################################################


start <- min(GPS_Dist$local_time, na.rm = TRUE)  # "2020-10-30 08:05:19 ACDT"
end <-   max(GPS_Dist$local_time, na.rm = TRUE) # "2020-11-03 11:47:16 ACDT"
#Since we’re dealing with elapsed time between two dates, let’s start with Intervals. We can define an Interval using the %--% operator.
start <- round_date(start, unit="10 mins") #2020-10-30 08:10:00 ACDT"
end <- round_date(end, unit="10 mins") # "2020-11-03 11:50:00 ACDT"

time.interval <- start %--% end
time.interval
#To create a Duration between these two dates, we can use the as.duration function.

time.duration <- as.duration(time.interval)
time.duration # "358800s (~4.15 days)"

################################################################################
#### --------------    make a regular time step   -------------- ####
################################################################################


regular_time_interval <-data.frame(time_step = seq(from = ymd_hms(start),
                                                   to = ymd_hms(end), 
                                                   by = '10 mins'))
################################################################################
#### ----   Write out regular time step for later reference -------------- #####
################################################################################


saveRDS(regular_time_interval,  "W:/VF/2024/animal behaviour data/Long Plain/data_prep/VF3_regular_time_interval.rds")




################################################################################
#### ----   Need to round the local time to the closest 10 min  -------------- ####
################################################################################
str(GPS_Dist)
# Need to round the local time to the closest 10 min
GPS_Dist <- GPS_Dist %>% 
  dplyr::mutate(round_local_time =round_date(local_time, unit="10 mins"),
                Time_animal = paste0(round_local_time,"_", animal) ,
                Time_animal_zone = paste0(round_local_time,"_", animal, "_", VF_EX))


rm(end,start, time.duration, time.interval)


###############################################################################  
## ----- function to produce steps per animal
################################################################################

animal_list <- GPS_Dist %>% distinct(animal) %>%  arrange(animal)
dim(animal_list)
### 20 animals ID I need regular time interval for each animal
### List of sites I want to run analysis for:
animal_list
#sheep_list <- c(1:6)
#animal_list <- "Q10"
animal_list <- animal_list$animal


### as a function
for (animal_list in animal_list){
  
################################################################################  
#regular_time_interval_per_animal_ID
################################################################################
  regular_time_interval_animal <- regular_time_interval %>% 
    dplyr::mutate(Time_animal = paste0(time_step,"_", animal_list))
  
################################################################################
#### --------------    Join regular time step to dataset  -------------- ####
################################################################################  
  
  GPS_animal <- GPS_Dist %>%  filter(animal == animal_list)
  GPS_animal <- GPS_animal %>%
    dplyr::distinct(Time_animal_zone, .keep_all = TRUE)
  
  ## the occurrence of a duplicated time_animal
  
 # It might be a better to split the data into  outside_VF and inside_VF
  
  #outside_VF <- GPS_animal %>% filter(VF_EX == "outside_VF") %>% dplyr::distinct(Time_animal, .keep_all = TRUE) #
  #inside_VF <- GPS_animal %>% filter(VF_EX == "inside_VF") %>% dplyr::distinct(Time_animal, .keep_all = TRUE) #
  
  ##try this to retain the max distance from the VF when outside the grazing zone and middle distance when inside get the ID values only
  outside_VF_ID_retain <- GPS_animal %>% filter(VF_EX == "outside_VF") %>% 
    group_by(Time_animal, ID_jaxs) %>% 
    summarise(dist = max(dist_to_VF, na.rm = TRUE))
  outside_VF_ID_retain <- as.list(outside_VF_ID_retain$ID_jaxs)
  
  outside_VF <- GPS_animal %>% filter(VF_EX == "outside_VF") %>% filter(ID_jaxs %in% outside_VF_ID_retain)
  
  
  inside_VF_ID_retain <- GPS_animal %>% filter(VF_EX == "inside_VF") %>% 
    group_by(Time_animal, ID_jaxs) %>% 
    summarise(dist = median(dist_to_VF, na.rm = TRUE))
  inside_VF_ID_retain <- as.list(inside_VF_ID_retain$ID_jaxs)
  
  inside_VF <- GPS_animal %>% filter(VF_EX == "inside_VF") %>% filter(ID_jaxs %in% inside_VF_ID_retain)
  
  
  
  
  GPS_animal <- rbind(outside_VF,inside_VF )
  
  duplication_report <- GPS_animal %>% count(Time_animal)
   
   GPS_animal <- left_join(GPS_animal,duplication_report ) %>% rename(occurance = n )
   str(GPS_animal)
  # 
   GPS_animal <- GPS_animal %>% mutate(
     what_to_retain = case_when(
       occurance == 1 & VF_EX == "outside_VF" ~ "retain",
       occurance == 2 & VF_EX == "outside_VF" ~ "retain", #
       occurance == 1 & VF_EX == "inside_VF" ~ "retain",
       TRUE                      ~ "discard"
     )
   ) 
  
  # remove the rows tp discard
  GPS_animal <- GPS_animal %>% filter(what_to_retain == "retain")
  
  GPS_animal_reg_time <- left_join(regular_time_interval_animal, GPS_animal)

  #### Trim the regular time step to match the end of animal time

  start_animal <- min(GPS_animal$local_time, na.rm = TRUE)  
  end_animal <-   max(GPS_animal$local_time, na.rm = TRUE) 
  start_animal <- round_date(start_animal, unit="10 mins")
  end_animal <- round_date(end_animal, unit="10 mins") 
  
  ## trim the joined data to the animal ID time in the trial 
  
  
  GPS_animal_reg_time <- GPS_animal_reg_time %>% 
    dplyr::filter(between(time_step, ymd_hms(start_animal), ymd_hms(end_animal))) 
 
  
  ################################################################################
  #### Do some cals  steps or distance travelled since last logged point ---- ####
  ################################################################################ 

    GPS_animal_reg_time <- GPS_animal_reg_time %>% 
    arrange(local_time)
  
  GPS_animal_reg_time <- GPS_animal_reg_time %>% 
    dplyr::mutate(step = sqrt( ((lead(X) - X)^ 2) + ((lead(Y) - Y)^ 2) ) )
  
  rm(
    GPS_animal,
    regular_time_interval_animal
    )
  name <- paste0("GPS_animal_reg_time_step", animal_list)
  assign(name,GPS_animal_reg_time)
  
  }       


print(animal_list$animal)

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




################################################################################
#### ----   Write out regular time step for later reference -------------- #####
################################################################################

saveRDS(GPS_animal_reg_time_step_all,  "W:/VF/2024/animal behaviour data/Long Plain/data_prep/VF3_step5.rds")










