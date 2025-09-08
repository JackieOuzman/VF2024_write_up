library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)


### Retain data that is for start of fence move and then for 8 hours...

VF_all <-  readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/Pinnaroo2022_all_data.rds") 
VF_no_control <- VF_all %>% filter(VF != "VFControl")


names(VF_all)
str(VF_all)
VF_all %>% distinct(VF) 

################################################################################
VF1_trimmed_DOY201 <- VF_all %>%
  filter(VF == "VF1") %>%
  filter(DOY == "201") %>% 
  filter(local_time >= min(local_time, na.rm = TRUE) & 
           local_time <= min(local_time, na.rm = TRUE) + 8 * 60 * 60) 
  

VF1_trimmed_DOY201_sf <- VF1_trimmed_DOY201 %>%
  st_as_sf(coords = c("X", "Y"), crs = 28354)  # Adjust column names and CRS as needed

# Save as shapefile
st_write(VF1_trimmed_DOY201_sf, "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/fence_move_8hrs/VF1_trimmed_DOY201_sf.shp")

################################################################################

VF2_trimmed_DOY <- VF_all %>%
  filter(VF == "VF2") %>%
  #filter(DOY == "202") %>% 
  filter(local_time >= min(local_time, na.rm = TRUE) & 
           local_time <= min(local_time, na.rm = TRUE) + 8 * 60 * 60) 

VF2_trimmed_DOY %>% distinct(DOY) #this is DOY 203
VF2_trimmed_DOY_sf <- VF2_trimmed_DOY %>%
  st_as_sf(coords = c("X", "Y"), crs = 28354)  # Adjust column names and CRS as needed

# Save as shapefile
st_write(VF2_trimmed_DOY_sf, "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/fence_move_8hrs/VF2_trimmed_DOY_sf.shp")

################################################################################

VFX_trimmed_DOY202 <- VF_all %>%
  #filter(VF == "VF2") %>%
  filter(DOY == "202") %>% 
  filter(local_time >= min(local_time, na.rm = TRUE) & 
           local_time <= min(local_time, na.rm = TRUE) + 8 * 60 * 60) 

VFX_trimmed_DOY202 %>% distinct(VF) #this is DOY 202 and VF1
VFX_trimmed_DOY202_sf <- VFX_trimmed_DOY202 %>%
  st_as_sf(coords = c("X", "Y"), crs = 28354)  # Adjust column names and CRS as needed

# Save as shapefile
st_write(VFX_trimmed_DOY202_sf, "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/fence_move_8hrs/VFX_trimmed_DOY202_sf.shp")



################################################################################

VF3_trimmed_DOY203 <- VF_all %>%
  filter(VF == "VF3") %>%
  filter(DOY == "203") %>% 
  filter(local_time >= min(local_time, na.rm = TRUE) & 
           local_time <= min(local_time, na.rm = TRUE) + 8 * 60 * 60) 

VF3_trimmed_DOY203 %>% distinct(DOY) #this is DOY 203
VF3_trimmed_DOY203 %>% distinct(VF) #this is DOY 203

VF3_trimmed_DOY_sf <- VF3_trimmed_DOY203 %>%
  st_as_sf(coords = c("X", "Y"), crs = 28354)  # Adjust column names and CRS as needed

# Save as shapefile
st_write(VF3_trimmed_DOY_sf, "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/fence_move_8hrs/VF3_trimmed_DOY203_sf.shp")

################################################################################



VF4_trimmed_DOY206 <- VF_all %>%
  filter(VF == "VF4") %>%
  #filter(DOY == "205") %>% 
  filter(local_time >= min(local_time, na.rm = TRUE) & 
           local_time <= min(local_time, na.rm = TRUE) + 8 * 60 * 60) 

VF4_trimmed_DOY206 %>% distinct(DOY) #this is DOY 206
VF4_trimmed_DOY206 %>% distinct(VF) #this is VF4

VF4_trimmed_DOY206_sf <- VF4_trimmed_DOY206 %>%
  st_as_sf(coords = c("X", "Y"), crs = 28354)  # Adjust column names and CRS as needed

# Save as shapefile
st_write(VF4_trimmed_DOY206_sf, "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/fence_move_8hrs/VF4_trimmed_DOY206_sf.shp")

################################################################################


VF5_trimmed_DOY207 <- VF_all %>%
  filter(VF == "VF5") %>%
  #filter(DOY == "205") %>% 
  filter(local_time >= min(local_time, na.rm = TRUE) & 
           local_time <= min(local_time, na.rm = TRUE) + 8 * 60 * 60) 

VF5_trimmed_DOY207 %>% distinct(DOY) #this is DOY 207
VF5_trimmed_DOY207 %>% distinct(VF) #this is VF5

VF5_trimmed_DOY207_sf <- VF5_trimmed_DOY207 %>%
  st_as_sf(coords = c("X", "Y"), crs = 28354)  # Adjust column names and CRS as needed

# Save as shapefile
st_write(VF5_trimmed_DOY207_sf, "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/fence_move_8hrs/VF5_trimmed_DOY207_sf.shp")

################################################################################
VF6_trimmed_DOY208 <- VF_all %>%
  filter(VF == "VF6") %>%
  #filter(DOY == "205") %>% 
  filter(local_time >= min(local_time, na.rm = TRUE) & 
           local_time <= min(local_time, na.rm = TRUE) + 8 * 60 * 60) 

VF6_trimmed_DOY208 %>% distinct(DOY) #this is DOY 208
VF6_trimmed_DOY208 %>% distinct(VF) #this is VF6

VF6_trimmed_DOY208_sf <- VF6_trimmed_DOY208 %>%
  st_as_sf(coords = c("X", "Y"), crs = 28354)  # Adjust column names and CRS as needed

# Save as shapefile
st_write(VF6_trimmed_DOY208_sf, "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/fence_move_8hrs/VF6_trimmed_DOY208_sf.shp")

################################################################################



#################################################################################
### Control with filter time as above ##############

control <-  read_csv("W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/control_step3_clip.csv") 



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
st_write(control_VF1_sf, "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/fence_move_8hrs/control_VF1_sf.shp")


####VF2

time_bounds_VF2 <- VF_no_control %>%
  filter(DOY == "202") %>% 
  #filter(VF == "VF1") %>%
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
st_write(control_VF2_sf, "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/fence_move_8hrs/control_VF2_sf.shp")



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
st_write(control_VF3_sf, "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/fence_move_8hrs/control_VF3_sf.shp")

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
st_write(control_VF4_sf, "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/fence_move_8hrs/control_VF4_sf.shp")

####VF5

time_bounds_VF5 <- VF_no_control %>%
  filter(VF == "VF5") %>%
  summarise(
    min_time = min(local_time, na.rm = TRUE),
    max_time = min(local_time, na.rm = TRUE) + 8 * 60 * 60
  )

# Apply the same time period to another dataframe
control_VF5 <- control %>%
  filter(local_time >= time_bounds_VF5$min_time & 
           local_time <= time_bounds_VF5$max_time)

control_VF5_sf <- control_VF5 %>%
  st_as_sf(coords = c("X", "Y"), crs = 28354)  # Adjust column names and CRS as needed

# Save as shapefile
st_write(control_VF5_sf, "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/fence_move_8hrs/control_VF5_sf.shp")


####VF6

time_bounds_VF6 <- VF_no_control %>%
  filter(VF == "VF6") %>%
  summarise(
    min_time = min(local_time, na.rm = TRUE),
    max_time = min(local_time, na.rm = TRUE) + 8 * 60 * 60
  )

# Apply the same time period to another dataframe
control_VF6 <- control %>%
  filter(local_time >= time_bounds_VF6$min_time & 
           local_time <= time_bounds_VF6$max_time)

control_VF6_sf <- control_VF6 %>%
  st_as_sf(coords = c("X", "Y"), crs = 28354)  # Adjust column names and CRS as needed

# Save as shapefile
st_write(control_VF6_sf, "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/fence_move_8hrs/control_VF6_sf.shp")

