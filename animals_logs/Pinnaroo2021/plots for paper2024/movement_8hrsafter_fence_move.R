library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)


### Retain data that is for start of fence move and then for 8 hours...

VF_all <-  readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2021/data_for_plots/Pinnaroo2021_all_data.rds") 
VF_no_control <- VF_all %>% filter(VF != "VFControl")


names(VF_all)
str(VF_all)
VF_all %>% distinct(VF) 

################################################################################
VF1_trimmed <- VF_all %>%
  filter(VF == "VF1") %>%
   filter(local_time >= min(local_time, na.rm = TRUE) & 
           local_time <= min(local_time, na.rm = TRUE) + 8 * 60 * 60) 
  

VF1_trimmed_sf <- VF1_trimmed %>%
  st_as_sf(coords = c("X", "Y"), crs = 28354)  # Adjust column names and CRS as needed

# Save as shapefile
st_write(VF1_trimmed_sf, "W:/VF/2024/animal behaviour data/Pinnaroo2021/data_for_plots/fence_move_8hrs/VF1_trimmed_sf.shp")

################################################################################

VF2_trimmed <- VF_all %>%
  filter(VF == "VF2") %>%
  filter(local_time >= min(local_time, na.rm = TRUE) & 
           local_time <= min(local_time, na.rm = TRUE) + 8 * 60 * 60) 


VF2_trimmed_sf <- VF2_trimmed %>%
  st_as_sf(coords = c("X", "Y"), crs = 28354)  # Adjust column names and CRS as needed

# Save as shapefile
st_write(VF2_trimmed_sf, "W:/VF/2024/animal behaviour data/Pinnaroo2021/data_for_plots/fence_move_8hrs/VF2_trimmed_sf.shp")

################################################################################

VF3_trimmed <- VF_all %>%
  filter(VF == "VF3") %>%
  filter(local_time >= min(local_time, na.rm = TRUE) & 
           local_time <= min(local_time, na.rm = TRUE) + 8 * 60 * 60) 


VF3_trimmed_sf <- VF3_trimmed %>%
  st_as_sf(coords = c("X", "Y"), crs = 28354)  # Adjust column names and CRS as needed

# Save as shapefile
st_write(VF3_trimmed_sf, "W:/VF/2024/animal behaviour data/Pinnaroo2021/data_for_plots/fence_move_8hrs/VF3_trimmed_sf.shp")


################################################################################

VF4_trimmed <- VF_all %>%
  filter(VF == "VF4") %>%
  filter(local_time >= min(local_time, na.rm = TRUE) & 
           local_time <= min(local_time, na.rm = TRUE) + 8 * 60 * 60) 


VF4_trimmed_sf <- VF4_trimmed %>%
  st_as_sf(coords = c("X", "Y"), crs = 28354)  # Adjust column names and CRS as needed

# Save as shapefile
st_write(VF4_trimmed_sf, "W:/VF/2024/animal behaviour data/Pinnaroo2021/data_for_plots/fence_move_8hrs/VF4_trimmed_sf.shp")
