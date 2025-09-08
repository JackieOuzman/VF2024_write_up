library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)


### Retain data that is for start of fence move and then for 8 hours...

VF_all <-  readRDS("W:/VF/2024/animal behaviour data/Long Plain/data_for_plots/LP_all_data.rds") 
VF_no_control <- VF_all %>% filter(VF != "VFControl")


names(VF_all)
str(VF_all)
VF_no_control %>% distinct(VF) 

################################################################################
VF1_trimmed_DOY <- VF_no_control %>%
  filter(VF == "VF1") %>%
  #filter(DOY == "201") %>% 
  filter(local_time >= min(local_time, na.rm = TRUE) & 
           local_time <= min(local_time, na.rm = TRUE) + 8 * 60 * 60) 
  
VF1_trimmed_DOY %>% distinct(DOY)

VF1_trimmed_DOY_sf <- VF1_trimmed_DOY %>%
  st_as_sf(coords = c("X", "Y"), crs = 28354)  # Adjust column names and CRS as needed

# Save as shapefile
st_write(VF1_trimmed_DOY_sf, "W:/VF/2024/animal behaviour data/Long Plain/data_for_plots/fence_move_8hrs/VF1_trimmed_DOY_sf_XY.shp")



################################################################################
VF2_trimmed_DOY <- VF_no_control %>%
  filter(VF == "VF2") %>%
  
  filter(local_time >= min(local_time, na.rm = TRUE) & 
           local_time <= min(local_time, na.rm = TRUE) + 8 * 60 * 60) 

VF2_trimmed_DOY %>% distinct(DOY)

VF2_trimmed_DOY_sf <- VF2_trimmed_DOY %>%
  st_as_sf(coords = c("X", "Y"), crs = 28354)  # Adjust column names and CRS as needed

# Save as shapefile
st_write(VF2_trimmed_DOY_sf, "W:/VF/2024/animal behaviour data/Long Plain/data_for_plots/fence_move_8hrs/VF2_trimmed_DOY_sf.shp")



################################################################################
VF3_trimmed_DOY <- VF_no_control %>%
  filter(VF == "VF3") %>%
  
  filter(local_time >= min(local_time, na.rm = TRUE) & 
           local_time <= min(local_time, na.rm = TRUE) + 8 * 60 * 60) 

VF3_trimmed_DOY %>% distinct(DOY)

VF3_trimmed_DOY_sf <- VF3_trimmed_DOY %>%
  st_as_sf(coords = c("X", "Y"), crs = 28354)  # Adjust column names and CRS as needed

# Save as shapefile
st_write(VF3_trimmed_DOY_sf, "W:/VF/2024/animal behaviour data/Long Plain/data_for_plots/fence_move_8hrs/VF3_trimmed_DOY_sf.shp")


################################################################################
VF4_trimmed_DOY <- VF_no_control %>%
  filter(VF == "VF4") %>%
  
  filter(local_time >= min(local_time, na.rm = TRUE) & 
           local_time <= min(local_time, na.rm = TRUE) + 8 * 60 * 60) 

VF4_trimmed_DOY %>% distinct(DOY)

VF4_trimmed_DOY_sf <- VF4_trimmed_DOY %>%
  st_as_sf(coords = c("X", "Y"), crs = 28354)  # Adjust column names and CRS as needed

# Save as shapefile
st_write(VF4_trimmed_DOY_sf, "W:/VF/2024/animal behaviour data/Long Plain/data_for_plots/fence_move_8hrs/VF4_trimmed_DOY_sf.shp")





#################################################################################
### Control with filter time as above ##############
control <- VF_all %>% filter(VF == "VFControl")

####VF1

time_bounds_VF1 <- VF_no_control %>%
  filter(VF == "VF1") %>%
  summarise(
    min_time = min(local_time, na.rm = TRUE),
    max_time = min(local_time, na.rm = TRUE) + 8 * 60 * 60
  )

# Apply the same time period to another dataframe
control_VF1 <- control %>%
  filter(local_time >= time_bounds_VF1$min_time & 
           local_time <= time_bounds_VF1$max_time)

control_VF1_sf <- control_VF1 %>%
  st_as_sf(coords = c("X", "Y"), crs = 28354)  # Adjust column names and CRS as needed

# Save as shapefile
st_write(control_VF1_sf, "W:/VF/2024/animal behaviour data/Long Plain/data_for_plots/fence_move_8hrs/control_VF1_sf.shp")


####VF2

time_bounds_VF2 <- VF_no_control %>%
  filter(VF == "VF2") %>%
  summarise(
    min_time = min(local_time, na.rm = TRUE),
    max_time = min(local_time, na.rm = TRUE) + 8 * 60 * 60
  )

# Apply the same time period to another dataframe
control_VF2 <- control %>%
  filter(local_time >= time_bounds_VF2$min_time & 
           local_time <= time_bounds_VF2$max_time)

control_VF2_sf <- control_VF2 %>%
  st_as_sf(coords = c("X", "Y"), crs = 28354)  # Adjust column names and CRS as needed

# Save as shapefile
st_write(control_VF2_sf, "W:/VF/2024/animal behaviour data/Long Plain/data_for_plots/fence_move_8hrs/control_VF2_sf.shp")


####VF3

time_bounds_VF3 <- VF_no_control %>%
  filter(VF == "VF3") %>%
  summarise(
    min_time = min(local_time, na.rm = TRUE),
    max_time = min(local_time, na.rm = TRUE) + 8 * 60 * 60
  )

# Apply the same time period to another dataframe
control_VF3 <- control %>%
  filter(local_time >= time_bounds_VF3$min_time & 
           local_time <= time_bounds_VF3$max_time)

control_VF3_sf <- control_VF3 %>%
  st_as_sf(coords = c("X", "Y"), crs = 28354)  # Adjust column names and CRS as needed

# Save as shapefile
st_write(control_VF3_sf, "W:/VF/2024/animal behaviour data/Long Plain/data_for_plots/fence_move_8hrs/control_VF3_sf.shp")

####VF4

time_bounds_VF4 <- VF_no_control %>%
  filter(VF == "VF4") %>%
  summarise(
    min_time = min(local_time, na.rm = TRUE),
    max_time = min(local_time, na.rm = TRUE) + 8 * 60 * 60
  )

# Apply the same time period to another dataframe
control_VF4 <- control %>%
  filter(local_time >= time_bounds_VF4$min_time & 
           local_time <= time_bounds_VF4$max_time)

control_VF4_sf <- control_VF4 %>%
  st_as_sf(coords = c("X", "Y"), crs = 28354)  # Adjust column names and CRS as needed

# Save as shapefile
st_write(control_VF4_sf, "W:/VF/2024/animal behaviour data/Long Plain/data_for_plots/fence_move_8hrs/control_VF4_sf.shp")

