## step 3 clipping the data 

library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)


############################################################################################
############                  bring in boundaries             ##############################
############################################################################################

Hard_fence_bound <- st_read("W:/VF/LongPlain/LP Blk Bound/LongPlain_GDA_internal_bound.shp")  # this is the hard fences
Hard_fence_bound <-
  st_transform(Hard_fence_bound, crs = 28354)


Hard_fence_bound_control <- Hard_fence_bound %>% filter(VF_name ==  "Control")
Hard_fence_bound_VF <- Hard_fence_bound %>% filter(VF_name ==  "VF")

VF_fence_bound <- st_read("W:/VF/LongPlain/LP Blk Bound/LongPlainVF_bound.shp")  # this is the hard fences
VF_fence_bound <-
  st_transform(VF_fence_bound, crs = 28354)





path_step1 <- "W:/VF/2024/animal behaviour data/Long Plain/data_prep/"


################################################################
### Clip to the VF1 hard fences  with    #########
################################################################


step1_2_VF1 <- readRDS(paste0(path_step1, "VF1.rds"))
str(step1_2_VF1)

## Add a clm for ID_jaxs
step1_2_VF1 <- step1_2_VF1 %>% 
  dplyr::mutate( ID_jaxs = row_number()) %>% 
  dplyr::mutate(fencesID = "VF1")


step1_2_VF1 <- step1_2_VF1 %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))





#turn into spatial data

step1_2_VF1_sf <-
  st_as_sf(step1_2_VF1,
           coords = c("gpsData.lng", "gpsData.lat"),
           crs = 4326,
           agr = "constant")

GPS_sf_trans_VF1 <-
  st_transform(step1_2_VF1_sf, crs = 28354)





#To the large block boundary with buffer
GPS_sf_trans_VF1_clip <-
  st_intersection(GPS_sf_trans_VF1, st_difference(Hard_fence_bound_VF)) #this 'st_difference' function is supposed to remove the duplication




ggplot() +
  geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
  #geom_sf(data = VF1_paddock, color = "black", fill = NA) +
   geom_sf(data = GPS_sf_trans_VF1_clip ,alpha = 0.03) +
   theme_bw()+
   facet_wrap(.~ date)+
   theme(legend.position = "none",
         axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
   labs(title = "clipped")

## convert the geom clm into x and y clms


coordinates <-as.data.frame( st_coordinates(GPS_sf_trans_VF1_clip))
GPS_sf_trans_VF1_clip_df <- as.data.frame(GPS_sf_trans_VF1_clip)

GPS_sf_trans_VF1_clip_df <- GPS_sf_trans_VF1_clip_df %>% 
  dplyr::select(-"geometry")


GPS_sf_trans_VF1_clip_df <-   cbind(GPS_sf_trans_VF1_clip_df,coordinates )




saveRDS(GPS_sf_trans_VF1_clip_df,  "W:/VF/2024/animal behaviour data/Long Plain/data_prep/VF1step3_clip.rds")


rm(list=ls()[! ls() %in% c("Hard_fence_bound","VF_fence_bound","Hard_fence_bound_VF",
                           "path_step1",
                           "GPS_sf_trans_VF1_clip_df"
                           )])


###############################################################################
#### VF 2 


step1_2VF <- readRDS(paste0(path_step1, "VF2.rds"))

#format time and date clm from character to time

## Add a clm for ID_jaxs
step1_2_VF <- step1_2VF %>% 
  dplyr::mutate( ID_jaxs = row_number()) %>% 
  dplyr::mutate(fencesID = "VF2")


step1_2VF <- step1_2VF %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))



names(step1_2VF)

#turn into spatial data
step1_2VF <- step1_2VF %>% 
  filter(!is.na(gpsData.lng))

step1_2VF <-
  st_as_sf(step1_2VF,
           coords = c("gpsData.lng", "gpsData.lat"),
           crs = 4326,
           agr = "constant")

GPS_sf_trans_VF2 <-
  st_transform(step1_2VF, crs = 28354)





#To the large block boundary with buffer
step1_2_sf_clip_VF2 <-
  st_intersection(GPS_sf_trans_VF2, st_difference(Hard_fence_bound_VF)) #this 'st_difference' function is supposed to remove the duplication




 ggplot() +
   geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
   #geom_sf(data = VF1_paddock, color = "black", fill = NA) +
   geom_sf(data = step1_2_sf_clip_VF2 ,alpha = 0.03) +
   theme_bw()+
   facet_wrap(.~ date)+
   theme(legend.position = "none",
         axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
   labs(title = "clipped")

## convert the geom clm into x and y clms


coordinates <-as.data.frame( st_coordinates(step1_2_sf_clip_VF2))
step1_2_sf_clip_VF2_df <- as.data.frame(step1_2_sf_clip_VF2)

step1_2_sf_clip_VF2_df <- step1_2_sf_clip_VF2_df %>% 
  dplyr::select(-"geometry")


step1_2_sf_clip_VF2_df <-   cbind(step1_2_sf_clip_VF2_df,coordinates )



saveRDS(step1_2_sf_clip_VF2_df,  "W:/VF/2024/animal behaviour data/Long Plain/data_prep/VF2step3_clip.rds")


rm(list=ls()[! ls() %in% c("Hard_fence_bound","VF_fence_bound","Hard_fence_bound_VF",
                           "path_step1",
                           "GPS_sf_trans_VF1_clip_df",
                           "step1_2_sf_clip_VF2_df"
)])



###############################################################################
#### VF 3 


step1_3VF <- readRDS(paste0(path_step1, "VF3.rds"))

#format time and date clm from character to time


## Add a clm for ID_jaxs
step1_2_VF3 <- step1_3VF %>% 
  dplyr::mutate( ID_jaxs = row_number()) %>% 
  dplyr::mutate(fencesID = "VF3")


step1_2_VF3 <- step1_2_VF3 %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))



names(step1_2_VF3)

#turn into spatial data
step1_2_VF3 <- step1_2_VF3 %>% 
  filter(!is.na(gpsData.lng))

step1_2_VF3 <-
  st_as_sf(step1_2_VF3,
           coords = c("gpsData.lng", "gpsData.lat"),
           crs = 4326,
           agr = "constant")

GPS_sf_trans_VF3 <-
  st_transform(step1_2_VF3, crs = 28354)





#To the large block boundary with buffer
step1_2_sf_clip_VF3 <-
  st_intersection(GPS_sf_trans_VF3, st_difference(Hard_fence_bound_VF)) #this 'st_difference' function is supposed to remove the duplication




 ggplot() +
   geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
   
   geom_sf(data = step1_2_sf_clip_VF3 ,alpha = 0.03) +
   theme_bw()+
   facet_wrap(.~ date)+
   theme(legend.position = "none",
         axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
   labs(title = "clipped")

## convert the geom clm into x and y clms


coordinates <-as.data.frame( st_coordinates(step1_2_sf_clip_VF3))
step1_2_sf_clip_VF3_df <- as.data.frame(step1_2_sf_clip_VF3)

step1_2_sf_clip_VF3_df <- step1_2_sf_clip_VF3_df %>% 
  dplyr::select(-"geometry")


step1_2_sf_clip_VF3_df <-   cbind(step1_2_sf_clip_VF3_df,coordinates )

saveRDS(step1_2_sf_clip_VF3_df,  "W:/VF/2024/animal behaviour data/Long Plain/data_prep/VF3step3_clip.rds")


rm(list=ls()[! ls() %in% c("Hard_fence_bound","VF_fence_bound","Hard_fence_bound_VF",
                           "path_step1",
                           "GPS_sf_trans_VF1_clip_df",
                           "step1_2_sf_clip_VF2_df",
                           "step1_2_sf_clip_VF3_df"
)])


###############################################################################
#### VF - VF_deactivation


step1_2_De_VF <-  readRDS(paste0(path_step1, "VF_deactivation.rds"))

#format time and date clm from character to time

## Add a clm for ID_jaxs
step1_2_De_VF <- step1_2_De_VF %>% 
  dplyr::mutate( ID_jaxs = row_number()) %>% 
  dplyr::mutate(fencesID = "VF_deactivation")


step1_2_De_VF <- step1_2_De_VF %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))


names(step1_2_De_VF)


#turn into spatial data
step1_2_De_VF <- step1_2_De_VF %>% 
  filter(!is.na(gpsData.lng))

step1_2_De_VF_sf <-
  st_as_sf(step1_2_De_VF,
           coords = c("gpsData.lng", "gpsData.lat"),
           crs = 4326,
           agr = "constant")

GPS_sf_trans_De_VF <-
  st_transform(step1_2_De_VF_sf, crs = 28354)





#To the large block boundary with buffer
GPS_sf_trans_De_VF_clip <-
  st_intersection(GPS_sf_trans_De_VF, st_difference(Hard_fence_bound_VF)) #this 'st_difference' function is supposed to remove the duplication




 ggplot() +
   geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
#   geom_sf(data = VF1_paddock, color = "black", fill = NA) +
   geom_sf(data = GPS_sf_trans_De_VF_clip ,alpha = 0.03) +
   theme_bw()+
   facet_wrap(.~ date)+
   theme(legend.position = "none",
         axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
   labs(title = "clipped")

## convert the geom clm into x and y clms


coordinates <-as.data.frame( st_coordinates(GPS_sf_trans_De_VF_clip))
GPS_sf_trans_De_VF_clip_df <- as.data.frame(GPS_sf_trans_De_VF_clip)

GPS_sf_trans_De_VF_clip_df <- GPS_sf_trans_De_VF_clip_df %>% 
  dplyr::select(-"geometry")


GPS_sf_trans_De_VF_clip_df <-   cbind(GPS_sf_trans_De_VF_clip_df,coordinates )


saveRDS(GPS_sf_trans_De_VF_clip_df,  "W:/VF/2024/animal behaviour data/Long Plain/data_prep/VF_deactivated_step3_clip.rds")


rm(list=ls()[! ls() %in% c("Hard_fence_bound","VF_fence_bound","Hard_fence_bound_VF",
                           "path_step1",
                           "GPS_sf_trans_VF1_clip_df",
                           "step1_2_sf_clip_VF2_df",
                           "step1_2_sf_clip_df",
                           "GPS_sf_trans_De_VF_clip_df"
)])

###############################################################################
#### No VF  - control


step1_2_control <- readRDS(paste0(path_step1, "No_VF_animal_GPS_data_1_4.rds"))

#format time and date clm from character to time


## Add a clm for ID_jaxs
step1_2_control <- step1_2_control %>% 
  dplyr::mutate( ID_jaxs = row_number()) %>% 
  dplyr::mutate(fencesID = "VF5")


step1_2_control <- step1_2_control %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))



glimpse(step1_2_control)

### remove missing values

step1_2_control <- step1_2_control %>% 
  filter(!is.na(gpsData.lng))

#turn into spatial data

step1_2_control_sf <-
  st_as_sf(step1_2_control,
           coords = c("gpsData.lng", "gpsData.lat"),
           crs = 4326,
           agr = "constant")

GPS_sf_trans_control <-
  st_transform(step1_2_control_sf, crs = 28354)


unique(Hard_fence_bound$VF_name)

Hard_fence_bound_control <- Hard_fence_bound %>% filter(VF_name ==  "Control")

#To the large block boundary with buffer
step1_2_control_sf_clip <-
  st_intersection(GPS_sf_trans_control, st_difference(Hard_fence_bound_control)) #this 'st_difference' function is supposed to remove the duplication




 ggplot() +
   geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
#   geom_sf(data = VF1_paddock, color = "black", fill = NA) +
   geom_sf(data = step1_2_control_sf_clip ,alpha = 0.03) +
   theme_bw()+
   facet_wrap(.~ date)+
   theme(legend.position = "none",
         axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
   labs(title = "clipped")

## convert the geom clm into x and y clms


coordinates <-as.data.frame( st_coordinates(step1_2_control_sf_clip))
step1_2_control_sf_clip_df <- as.data.frame(step1_2_control_sf_clip)

step1_2_control_sf_clip_df <- step1_2_control_sf_clip_df %>% 
  dplyr::select(-"geometry")


step1_2_control_sf_clip_df <-   cbind(step1_2_control_sf_clip_df,coordinates )


saveRDS(step1_2_control_sf_clip_df,  "W:/VF/2024/animal behaviour data/Long Plain/data_prep/control_step3_clip.rds")


rm(list=ls()[! ls() %in% c("Hard_fence_bound","VF_fence_bound","Hard_fence_bound_VF",
                           "path_step1",
                           "GPS_sf_trans_VF1_clip_df",
                           "step1_2_sf_clip_VF2_df",
                           "step1_2_sf_clip_df",
                           "GPS_sf_trans_De_VF_clip_df",
                           "step1_2_control_sf_clip_df"
)])






