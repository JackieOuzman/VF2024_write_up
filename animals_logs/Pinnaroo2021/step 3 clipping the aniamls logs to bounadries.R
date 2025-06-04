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
#"W:\VF\2024\spatial\Pinnaroo_2021\Pinnaroo_Bound_GDA.shp"
Hard_fence_bound <- st_read("W:/VF/2024/spatial/Pinnaroo_2021/Pinnaroo_Bound_GDA.shp")  # this is the hard fences
Hard_fence_bound <-
  st_transform(Hard_fence_bound, crs = 28354)




VF_fence_bound <- st_read("W:/VF/2024/spatial/Pinnaroo_2021//Pinnaroo_VF_clip_external_GDA.shp")  # this has 2 hard fences
VF_fence_bound <-
  st_transform(VF_fence_bound, crs = 28354)

#Test/ check#
# str(Hard_fence_bound)
# st_write(Hard_fence_bound, "W:/VF/2024/animal behaviour data/Long Plain/data_prep/checking_step3_VF1/Hard_fence_bound_GDA.shp")
# st_write(Hard_fence_bound_VF, "W:/VF/2024/animal behaviour data/Long Plain/data_prep/checking_step3_VF1/Hard_fence_bound_VF_GDA.shp")
# st_write(Hard_fence_bound_control, "W:/VF/2024/animal behaviour data/Long Plain/data_prep/checking_step3_VF1/Hard_fence_bound_control_GDA.shp")


ggplot() +
  geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF_fence_bound ,color = "blue", fill = NA) 

################################################################

path_step1 <- "W:/VF/2024/animal behaviour data/Pinnaroo2021/data_prep/"


################################################################
### Clip to the VF1 hard fences  with    #########
################################################################


step1_2_VF1 <- readRDS(paste0(path_step1, "VF1.rds"))
#step1_2_VF1 <- readRDS(paste0(path_step1, "VF1_step1_9370004.rds"))
str(step1_2_VF1)



## Add a clm for ID_jaxs
step1_2_VF1 <- step1_2_VF1 %>% 
  dplyr::mutate( ID_jaxs = row_number())# %>% 
  #dplyr::mutate(fencesID = "VF1")


step1_2_VF1 <- step1_2_VF1 %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))



names(step1_2_VF1)

#turn into spatial data when I project the data first and add x and y and save as CSV

step1_2_VF1_sf <-
  st_as_sf(step1_2_VF1,
           coords = c("xcoord", "ycoord"),
           crs = 28354, #
           agr = "constant")

##quick check
# st_write(step1_2_VF1_sf, "W:/VF/2024/animal behaviour data/Pinnaroo2021/CHECKstep1_2_VF1_sf_project_first.shp", layer_options = "GEOMETRY=AS_XY")
# write_csv(step1_2_VF1, "W:/VF/2024/animal behaviour data/Pinnaroo2021/CHECK.csv")
# 
# ggplot() +
#   geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
#   geom_sf(data = VF_fence_bound ,color = "blue", fill = NA) +
#   geom_sf(data = step1_2_VF1_sf ,color = "green") 




GPS_sf_trans_VF1 <-step1_2_VF1_sf


#To the large block boundary with buffer
GPS_sf_trans_VF1_clip <-
  st_intersection(GPS_sf_trans_VF1, st_difference(Hard_fence_bound)) #this 'st_difference' function is supposed to remove the duplication




ggplot() +
  geom_sf(data = VF_fence_bound, color = "black", fill = NA) +
  #geom_sf(data = VF1_paddock, color = "black", fill = NA) +
   geom_sf(data = GPS_sf_trans_VF1 ,alpha = 0.03) +
   theme_bw()+
   facet_wrap(.~ date)+
   theme(legend.position = "none",
         axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
   labs(title = "clipped to the hard fence - so just the south and east fences")

## convert the geom clm into x and y clms


coordinates <-as.data.frame( st_coordinates(GPS_sf_trans_VF1_clip))
GPS_sf_trans_VF1_clip_df <- as.data.frame(GPS_sf_trans_VF1_clip)

GPS_sf_trans_VF1_clip_df <- GPS_sf_trans_VF1_clip_df %>% 
  dplyr::select(-"geometry")


GPS_sf_trans_VF1_clip_df <-   cbind(GPS_sf_trans_VF1_clip_df,coordinates )




saveRDS(GPS_sf_trans_VF1_clip_df,  "W:/VF/2024/animal behaviour data/Pinnaroo2021/data_prep/VF1step3_clip.rds")
# write.csv(GPS_sf_trans_VF1_clip_df, "W:/VF/2024/animal behaviour data/Long Plain/Raw data/Projected_test/GPS_sf_WED_VF1_clip_df.csv")
# 
# 
# GPS_sf_trans_VF1_clip_df_9370004 <- GPS_sf_trans_VF1_clip_df %>% filter(animal_ID == 9370004)
# write.csv(GPS_sf_trans_VF1_clip_df_9370004, "W:/VF/2024/animal behaviour data/Long Plain/Raw data/Projected_test/GPS_sf_WED_VF1_clip_df_9370004.csv")


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
  dplyr::mutate( ID_jaxs = row_number())# %>% 
 # dplyr::mutate(fencesID = "VF2")


step1_2VF <- step1_2VF %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))



names(step1_2VF)

#turn into spatial data
step1_2VF <- step1_2VF %>% 
  filter(!is.na(xcoord))

step1_2VF <-
  st_as_sf(step1_2VF,
           coords = c("xcoord", "ycoord"),
           crs = 28354,
           agr = "constant")

GPS_sf_trans_VF2 <- step1_2VF
  





#To the large block boundary with buffer
step1_2_sf_clip_VF2 <-
  st_intersection(GPS_sf_trans_VF2, st_difference(Hard_fence_bound)) #this 'st_difference' function is supposed to remove the duplication




 ggplot() +
   geom_sf(data = VF_fence_bound, color = "black", fill = NA) +
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



saveRDS(step1_2_sf_clip_VF2_df,  "W:/VF/2024/animal behaviour data/Pinnaroo2021/data_prep/VF2step3_clip.rds")


rm(list=ls()[! ls() %in% c("Hard_fence_bound","VF_fence_bound","Hard_fence_bound_VF",
                           "path_step1",
                           "GPS_sf_trans_VF1_clip_df",
                           "step1_2_sf_clip_VF2_df"
)])



###############################################################################
#### VF 3 


step1_3VF <- readRDS(paste0(path_step1, "VF3.rds"))
unique(step1_3VF$VF_Fence) #
unique(step1_3VF$fencesID)



#format time and date clm from character to time


## Add a clm for ID_jaxs
step1_2_VF3 <- step1_3VF %>% 
  dplyr::mutate( ID_jaxs = row_number()) #%>% 
  #dplyr::mutate(fencesID = "VF3")




step1_2_VF3 <- step1_2_VF3 %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))



names(step1_2_VF3)

#turn into spatial data
step1_2_VF3 <- step1_2_VF3 %>% 
  filter(!is.na(xcoord))

step1_2_VF3 <-
  st_as_sf(step1_2_VF3,
           coords = c("xcoord", "ycoord"),
           crs = 28354,
           agr = "constant")

GPS_sf_trans_VF3 <-step1_2_VF3
 





#To the large block boundary with buffer
step1_2_sf_clip_VF3 <-
  st_intersection(GPS_sf_trans_VF3, st_difference(Hard_fence_bound)) #this 'st_difference' function is supposed to remove the duplication

step1_3VF_NULL <- step1_2_sf_clip_VF3 %>% filter(fencesID == "NULL")
step1_3VF_NA <- step1_2_sf_clip_VF3 %>% filter(is.na(fencesID)) 


 ggplot() +
   geom_sf(data = VF_fence_bound, color = "black", fill = NA) +
   
   geom_sf(data = step1_3VF_NULL, color = "green", fill = NA) + # occurs on the 15 only a few point
   geom_sf(data = step1_3VF_NA, color = "blue", fill = NA) + # occurs on the 14 only a few point
   
   geom_sf(data = step1_2_sf_clip_VF3 ,alpha = 0.03) +
   theme_bw()+
   facet_wrap(.~ date)+
   theme(legend.position = "none",
         axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
   labs(title = "clipped to the hard fence")

## convert the geom clm into x and y clms


coordinates <-as.data.frame( st_coordinates(step1_2_sf_clip_VF3))
step1_2_sf_clip_VF3_df <- as.data.frame(step1_2_sf_clip_VF3)

step1_2_sf_clip_VF3_df <- step1_2_sf_clip_VF3_df %>% 
  dplyr::select(-"geometry")


step1_2_sf_clip_VF3_df <-   cbind(step1_2_sf_clip_VF3_df,coordinates )

saveRDS(step1_2_sf_clip_VF3_df,  "W:/VF/2024/animal behaviour data/Pinnaroo2021/data_prep/VF3step3_clip.rds")


rm(list=ls()[! ls() %in% c("Hard_fence_bound","VF_fence_bound","Hard_fence_bound_VF",
                           "path_step1",
                           "GPS_sf_trans_VF1_clip_df",
                           "step1_2_sf_clip_VF2_df",
                           "step1_2_sf_clip_VF3_df"
)])


###############################################################################
#### VF - 4


step1_4VF <-  readRDS(paste0(path_step1, "VF4.rds"))

#format time and date clm from character to time

## Add a clm for ID_jaxs
step1_4VF <- step1_4VF %>% 
  dplyr::mutate( ID_jaxs = row_number()) #%>% 
  #dplyr::mutate(fencesID = "VF_deactivation")


step1_4VF <- step1_4VF %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))


names(step1_4VF)


#turn into spatial data
step1_4VF <- step1_4VF %>% 
  filter(!is.na(xcoord))

step1_4VF_sf <-
  st_as_sf(step1_4VF,
           coords = c("xcoord", "ycoord"),
           crs = 28354,
           agr = "constant")

GPS_sf_trans_4VF <- step1_4VF_sf
  





#To the large block boundary with buffer
GPS_sf_trans_4VF_clip <-
  st_intersection(GPS_sf_trans_4VF, st_difference(Hard_fence_bound)) #this 'st_difference' function is supposed to remove the duplication

step1_4VF_NULL <- GPS_sf_trans_4VF_clip %>% filter(fencesID == "NULL")
step1_4VF_NA <- GPS_sf_trans_4VF_clip %>% filter(is.na(fencesID)) 
step1_4VF_No_NA_NULL <- GPS_sf_trans_4VF_clip %>% filter(!is.na(fencesID)) %>% filter(fencesID != "NULL") 

unique(step1_4VF_No_NA_NULL$fencesID)

 ggplot() +
   geom_sf(data = VF_fence_bound, color = "black", fill = NA) +
   geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
   
   #geom_sf(data = GPS_sf_trans_4VF_clip ,color = "green",alpha = 0.03) + #heaps of data point
   #geom_sf(data = GPS_sf_trans_4VF_clip ,color = "blue",alpha = 0.03) + #heaps of data point
   
   geom_sf(data = GPS_sf_trans_4VF_clip ,alpha = 0.03) +
   #geom_sf(data = step1_4VF_No_NA_NULL ,color = "pink") +
   
   theme_bw()+
   facet_wrap(.~ date)+
   theme(legend.position = "none",
         axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
   labs(title = "clipped")

## convert the geom clm into x and y clms

 coordinates <-as.data.frame( st_coordinates(GPS_sf_trans_4VF_clip))
 step1_2_sf_clip_VF4_df <- as.data.frame(GPS_sf_trans_4VF_clip)
 
 step1_2_sf_clip_VF4_df <- step1_2_sf_clip_VF4_df %>% 
   dplyr::select(-"geometry")
 
 
 step1_2_sf_clip_VF4_df <-   cbind(step1_2_sf_clip_VF4_df,coordinates )
 
 saveRDS(step1_2_sf_clip_VF4_df,  "W:/VF/2024/animal behaviour data/Pinnaroo2021/data_prep/VF4step3_clip.rds")





rm(list=ls()[! ls() %in% c("Hard_fence_bound","VF_fence_bound","Hard_fence_bound_VF",
                           "path_step1",
                           "GPS_sf_trans_VF1_clip_df",
                           "step1_2_sf_clip_VF2_df",
                           "step1_2_sf_clip_df",
                           "GPS_sf_trans_De_VF_clip_df"
)])




