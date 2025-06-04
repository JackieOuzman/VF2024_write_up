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
GPS_Dist <- readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2021/data_prep/VF2_step4.rds")

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


start <- min(GPS_Dist$local_time, na.rm = TRUE)  # "2021-10-11 12:33:00 ACDT"
end <-   max(GPS_Dist$local_time, na.rm = TRUE) # "2021-10-12 08:53:00 ACDT"
#Since we’re dealing with elapsed time between two dates, let’s start with Intervals. We can define an Interval using the %--% operator.
start <- round_date(start, unit="10 mins") #2021-10-11 12:30:00 ACDT"
end <- round_date(end, unit="10 mins") # "2021-10-12 08:50:00 ACDT"

time.interval <- start %--% end
time.interval
#To create a Duration between these two dates, we can use the as.duration function.

time.duration <- as.duration(time.interval)
time.duration # 73200s (~20.33 hours

################################################################################
#### --------------    make a regular time step   -------------- ####
################################################################################


regular_time_interval <-data.frame(time_step = seq(from = ymd_hms(start),
                                                   to = ymd_hms(end), 
                                                   by = '10 mins'))
################################################################################
#### ----   Write out regular time step for later reference -------------- #####
################################################################################


saveRDS(regular_time_interval,  "W:/VF/2024/animal behaviour data/Pinnaroo2021/data_prep/VF2_regular_time_interval.rds")




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
### 39 animals ID I need regular time interval for each animal
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




# 9370004
# 2  9370088
# 3  9370123
# 4  9370124
# 5  9380134
# 6  9380142
# 7  9380144
# 8  9380196
# 9  9380265
# 10 9380268
# 11 9380297
# 12 9380310
# 13 9380332
# 14 9380384
# 15 9380422
# 16 9380430
# 17 9380434
# 18 9380451
# 19 9380455
# 20 9380461
# 21 9380470
# 22 9380477
# 23 9380479
# 24 9380495
# 25 9380527
# 26 9380572
# 27 9380591
# 28 9380611
# 29 9380672
# 30 9380674
# 31 9380713
# 32 9380743
# 33 9380744
# 34 9380754
# 35 9380774
# 36 9380787
# 37 9380796
# 38 9380807
# 39 9380821

GPS_animal_reg_time_step_all <- rbind(
  GPS_animal_reg_time_step9370004,
  GPS_animal_reg_time_step9370088,
  GPS_animal_reg_time_step9370123,
  GPS_animal_reg_time_step9370124,
  GPS_animal_reg_time_step9380134,
  GPS_animal_reg_time_step9380142,
  GPS_animal_reg_time_step9380144,
  GPS_animal_reg_time_step9380196,
  GPS_animal_reg_time_step9380265,
  GPS_animal_reg_time_step9380268,
  GPS_animal_reg_time_step9380297,
  GPS_animal_reg_time_step9380310,
  GPS_animal_reg_time_step9380332,
  GPS_animal_reg_time_step9380384,
  GPS_animal_reg_time_step9380422,
  GPS_animal_reg_time_step9380430,
  GPS_animal_reg_time_step9380434,
  GPS_animal_reg_time_step9380451,
  GPS_animal_reg_time_step9380455,
  GPS_animal_reg_time_step9380461,
  GPS_animal_reg_time_step9380470,
  GPS_animal_reg_time_step9380477,
  GPS_animal_reg_time_step9380479,
  GPS_animal_reg_time_step9380495,
  GPS_animal_reg_time_step9380527,
  GPS_animal_reg_time_step9380572,
  GPS_animal_reg_time_step9380591,
  GPS_animal_reg_time_step9380611,
  GPS_animal_reg_time_step9380672,
  GPS_animal_reg_time_step9380674,
  GPS_animal_reg_time_step9380713,
  GPS_animal_reg_time_step9380743,
  GPS_animal_reg_time_step9380744,
  GPS_animal_reg_time_step9380754,
  GPS_animal_reg_time_step9380774,
  GPS_animal_reg_time_step9380787,
  GPS_animal_reg_time_step9380796,
  GPS_animal_reg_time_step9380807,
  GPS_animal_reg_time_step9380821
  )




################################################################################
#### ----   Write out regular time step for later reference -------------- #####
################################################################################

saveRDS(GPS_animal_reg_time_step_all,  "W:/VF/2024/animal behaviour data/Pinnaroo2021/data_prep/VF2_step5.rds")


# write.csv(GPS_animal_reg_time_step9370004,
#           "W:/VF/2024/animal behaviour data/Pinnaroo2021/data_prep/checking_step3_VF2/WED_GPS_animal_reg_time_step9370004.csv")






