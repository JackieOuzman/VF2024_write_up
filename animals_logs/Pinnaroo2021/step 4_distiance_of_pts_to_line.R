### working how to cal a distance from a line.
#https://gis.stackexchange.com/questions/360675/how-to-calculate-the-distance-of-points-to-line-in-r
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)


# The data points with a blank fencesID id and aminDistanceToIZ = -1000000 has lying and standing values.
# 
# While the data points with a blank fencesID id and minDistanceToIZ = NA has no lying and standing values.
# 
# Tanusri has suggested:
#   
#“If the values in “fenceID” column as blank, this means there is no active virtual fence for that neckband yet. 
#Therefore, if you are performing some analysis related to virtual fencing, you can ignore those rows with blank fenceIDs."


##################################################################################
###########               VF1                      ##############################
##################################################################################
GPS <- readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2021/data_prep/VF1step3_clip.rds")

str(GPS)
#turn into spatial data
GPS <-   st_as_sf(GPS,
                         coords = c("X", "Y"),
                         crs = 28354,
                         agr = "constant")
names(GPS)

GPS <- GPS %>%  rename(cumulativeAudioCount = cumulative ,
                       cumulativeShockCount = cumulati_1)

GPS <- GPS %>% dplyr::select (ID_jaxs, #got
                              animal_ID, #got
                              #time,
                              local_time, #got
                              date,#got
                              DOY, #got
                              geometry,#got
                              fencesID, #got
                              VF_Fence,
                              # Audio_values,
                              # Shock_values,
                              cumulativeAudioCount, #do I need this one?
                              cumulativeShockCount, #do I need this one?
                              #event, #I think this contains pulse and audio
                              #resting_percentage, #this is in sep data files
                              #moving_percentage,
                              #grazing_percentage,
                              resting_pe, #this is in sep data files
                              moving_per,
                              grazing_pe,
                              #training_period
                              #id, 
                              #sheep, 
                              #treatment,
                              #DOT,
                              
                              )
names(GPS)

GPS <- GPS %>%  rename(
  resting = resting_pe,
  moving = moving_per,
  grazing = grazing_pe
)


############################################################################################
############                  bring in boundaries             ##############################
############################################################################################


Hard_fence_bound <- st_read("W:/VF/2024/spatial/Pinnaroo_2021/Pinnaroo_Bound_GDA.shp")  # this is the hard fences
Hard_fence_bound <-
  st_transform(Hard_fence_bound, crs = 28354)

VF_fence_bound <- st_read("W:/VF/2024/spatial/Pinnaroo_2021/Pinnaroo_VF_clip_external_GDA.shp")  # this has 2 hard fences
VF_fence_bound <-
  st_transform(VF_fence_bound, crs = 28354)


VF1_paddock <-   st_read("W:/VF/2024/spatial/Pinnaroo_2021/VF1_Graze_GDA.shp") #?not sure if this is correct GDA
VF1_exclusion_zone <- st_read("W:/VF/2024/spatial/Pinnaroo_2021/VF1_NonGraze.shp")
VF_1_line <-  st_read("W:/VF/2024/spatial/Pinnaroo_2021/VF1_Fence.shp")

############################################################################################

### check by plotting

str(GPS)


ggplot() +
  geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF1_paddock, color = "blue", fill = NA) +
  geom_sf(data = VF_1_line, color = "red", fill = NA) +
  
  geom_sf(data = GPS ,alpha = 0.03) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "check")


############################################################################################


GPS <- GPS %>% 
  dplyr::mutate(dist_to_VF = st_distance(GPS, VF_1_line))
############################################################################################
### report if the point is in the exclusion zone
str(VF1_paddock)
VF1_paddock <- VF1_paddock %>%  dplyr::select(id, geometry) #was ID

VF_points <-  st_intersection(GPS, st_difference(VF1_paddock)) %>% 
  dplyr::mutate(VF_EX = "inside_VF")

Exclusion_points <-  st_intersection(GPS, st_difference(VF1_exclusion_zone))%>% 
  dplyr::mutate(VF_EX = "outside_VF")
names(Exclusion_points)

Exclusion_points <- Exclusion_points %>% dplyr::select(ID_jaxs:dist_to_VF,geometry, VF_EX) #missing id

#ensure the clm names match
names(VF_points)
names(Exclusion_points)
VF_points <- VF_points %>% select(-id)

GPS_all <- rbind(VF_points, Exclusion_points)

str(GPS_all)


GPS_all <- GPS_all %>% dplyr::select(ID_jaxs:dist_to_VF,  VF_EX, geometry) #this has been modified



coordinates <-as.data.frame( st_coordinates(GPS_all))
GPS_all_df <- as.data.frame(GPS_all)

GPS_all_df <- GPS_all_df %>% 
  dplyr::select(-"geometry")

names(GPS_all_df)
GPS_all_df <-   cbind(GPS_all_df,coordinates )

#write.csv(GPS_all_df,  "W:/VF/2024/animal behaviour data/Pinnaroo_2021/data_prep/VF1_step4.csv")
saveRDS(GPS_all_df,  "W:/VF/2024/animal behaviour data/Pinnaroo2021/data_prep/VF1_step4.rds")

rm(GPS, GPS_all, GPS_all_df, Exclusion_points, coordinates, VF_points, 
   VF_1_line, VF1_exclusion_zone, VF1_paddock)

##################################################################################
###########               VF2                      ##############################
##################################################################################


GPS <- readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2021/data_prep/VF2step3_clip.rds")
#turn into spatial data
GPS <-   st_as_sf(GPS,
                  coords = c("X", "Y"),
                  crs = 28354,
                  agr = "constant")
names(GPS)
GPS <- GPS %>%  rename(cumulativeAudioCount = cumulative ,
                       cumulativeShockCount = cumulati_1)


GPS <- GPS %>% dplyr::select (ID_jaxs, #got
                              animal_ID, #got
                              #time,
                              local_time, #got
                              date,#got
                              DOY, #got
                              geometry,#got
                              fencesID, #got
                              VF_Fence,
                              # Audio_values,
                              # Shock_values,
                              cumulativeAudioCount, #do I need this one?
                              cumulativeShockCount, #do I need this one?
                              #event, #I think this contains pulse and audio
                              #resting_percentage, #this is in sep data files
                              #moving_percentage,
                              #grazing_percentage,
                              # resting., #this is in sep data files
                              # moving.,
                              # grazing.,
                              resting_pe, #this is in sep data files
                              moving_per,
                              grazing_pe,
                              #training_period
                              #ID, 
                              #sheep, 
                              #treatment,
                              #DOT,
                              
)
names(GPS)

GPS <- GPS %>%  rename(
  resting = resting_pe,
  moving = moving_per,
  grazing = grazing_pe
)


############################################################################################
############                  bring in boundaries             ##############################
############################################################################################


VF2_paddock <-   st_read("W:/VF/2024/spatial/Pinnaroo_2021/VF2_Graze_GDA.shp")
VF2_exclusion_zone <- st_read("W:/VF/2024/spatial/Pinnaroo_2021/VF2_NonGraze.shp")
VF_2_line <-  st_read("W:/VF/2024/spatial/Pinnaroo_2021/VF2_Fence.shp")

############################################################################################

### check by plotting

str(GPS)


ggplot() +
  geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF2_paddock, color = "blue", fill = NA) +
  geom_sf(data = VF_2_line, color = "red", fill = NA) +

  geom_sf(data = GPS ,alpha = 0.03) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "check")


############################################################################################


GPS <- GPS %>% 
  dplyr::mutate(dist_to_VF = st_distance(GPS, VF_2_line))
############################################################################################
### report if the point is in the exclusion zone

VF2_paddock <- VF2_paddock %>%  dplyr::select(id, geometry)

VF_points <-  st_intersection(GPS, st_difference(VF2_paddock)) %>% 
  dplyr::mutate(VF_EX = "inside_VF")

Exclusion_points <-  st_intersection(GPS, st_difference(VF2_exclusion_zone))%>% 
  dplyr::mutate(VF_EX = "outside_VF")

Exclusion_points <- Exclusion_points %>% dplyr::select(ID_jaxs:dist_to_VF,geometry, VF_EX) #missing id

#ensure the clm names match
names(VF_points)
names(Exclusion_points)
VF_points <- VF_points %>% select(-id)

GPS_all <- rbind(VF_points, Exclusion_points)

str(GPS_all)



GPS_all <- rbind(VF_points, Exclusion_points)





GPS_all <- GPS_all %>% dplyr::select(ID_jaxs:dist_to_VF,  VF_EX, geometry) #this has been modified



coordinates <-as.data.frame( st_coordinates(GPS_all))
GPS_all_df <- as.data.frame(GPS_all)

GPS_all_df <- GPS_all_df %>% 
  dplyr::select(-"geometry")

names(GPS_all_df)
GPS_all_df <-   cbind(GPS_all_df,coordinates )


saveRDS(GPS_all_df,  "W:/VF/2024/animal behaviour data/Pinnaroo2021/data_prep/VF2_step4.rds")

rm(GPS, GPS_all, GPS_all_df, Exclusion_points, coordinates, VF_points, 
   VF_2_line, VF2_exclusion_zone, VF2_paddock)

##################################################################################
###########               VF3                      ##############################
##################################################################################


GPS <- readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2021/data_prep/VF3step3_clip.rds")
#turn into spatial data
GPS <-   st_as_sf(GPS,
                  coords = c("X", "Y"),
                  crs = 28354,
                  agr = "constant")
names(GPS)
GPS <- GPS %>%  rename(cumulativeAudioCount = cumulative ,
                       cumulativeShockCount = cumulati_1)

GPS <- GPS %>% dplyr::select (ID_jaxs, #got
                              animal_ID, #got
                              #time,
                              local_time, #got
                              date,#got
                              DOY, #got
                              geometry,#got
                              fencesID, #got
                              VF_Fence,
                              # Audio_values,
                              # Shock_values,
                              cumulativeAudioCount, #do I need this one?
                              cumulativeShockCount, #do I need this one?
                              #event, #I think this contains pulse and audio
                              #resting_percentage, #this is in sep data files
                              #moving_percentage,
                              #grazing_percentage,
                              resting_pe, #this is in sep data files
                              moving_per,
                              grazing_pe,
                              #training_period
                              #id, 
                              #sheep, 
                              #treatment,
                              #DOT,
                              
)
names(GPS)

GPS <- GPS %>%  rename(
  resting = resting_pe,
  moving = moving_per,
  grazing = grazing_pe
)



############################################################################################
############                  bring in boundaries             ##############################
############################################################################################


VF3_paddock <-   st_read("W:/VF/2024/spatial/Pinnaroo_2021/VF3_Graze_GDA.shp")
VF3_exclusion_zone <- st_read("W:/VF/2024/spatial/Pinnaroo_2021/VF3_NonGraze.shp")
VF_3_line <-  st_read("W:/VF/2024/spatial/Pinnaroo_2021/VF3_Fence.shp")
############################################################################################

### check by plotting

str(GPS)

# VF2_paddock <-   st_read("W:/VF/2024/spatial/Pinnaroo_2021/VF2_Graze_GDA.shp")
# VF4_paddock <-   st_read("W:/VF/2024/spatial/Pinnaroo_2021/VF4_Graze_GDA.shp")
 VFexternal_paddock <-   st_read("W:/VF/2024/spatial/Pinnaroo_2021/Pinnaroo_VF_clip_external_GDA.shp")


ggplot() +
  geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF3_paddock, color = "blue", fill = NA) +
  geom_sf(data = VF_3_line, color = "red", fill = NA) +
  # geom_sf(data = VF2_paddock, color = "pink", fill = NA) + # this is paddock 2
  # geom_sf(data = VF4_paddock, color = "green", fill = NA) + # this is paddock 4
   geom_sf(data = VFexternal_paddock, color = "green", fill = NA) + # 

  geom_sf(data = GPS ,alpha = 0.03) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "check")


############################################################################################


GPS <- GPS %>% 
  dplyr::mutate(dist_to_VF = st_distance(GPS, VF_3_line))
############################################################################################
### report if the point is in the exclusion zone

VF3_paddock <- VF3_paddock %>%  dplyr::select(id, geometry)

VF_points <-  st_intersection(GPS, st_difference(VF3_paddock)) %>% 
  dplyr::mutate(VF_EX = "inside_VF")

Exclusion_points <-  st_intersection(GPS, st_difference(VF3_exclusion_zone))%>% 
  dplyr::mutate(VF_EX = "outside_VF")

Exclusion_points <- Exclusion_points %>% dplyr::select(ID_jaxs:Id,geometry, VF_EX)

#ensure the clm names match
names(VF_points)
names(Exclusion_points)
VF_points <- VF_points %>% select(-id)

GPS_all <- rbind(VF_points, Exclusion_points)

str(GPS_all)


GPS_all <- GPS_all %>% dplyr::select(ID_jaxs:dist_to_VF,  VF_EX, geometry) #this hs been modified



coordinates <-as.data.frame( st_coordinates(GPS_all))
GPS_all_df <- as.data.frame(GPS_all)

GPS_all_df <- GPS_all_df %>% 
  dplyr::select(-"geometry")

names(GPS_all_df)
GPS_all_df <-   cbind(GPS_all_df,coordinates )


saveRDS(GPS_all_df,  "W:/VF/2024/animal behaviour data/Pinnaroo2021/data_prep/VF3_step4.rds")



rm(GPS, GPS_all, GPS_all_df, Exclusion_points, coordinates, VF_points, 
   VF_3_line, VF3_exclusion_zone, VF3_paddock)





##################################################################################
###########               VF 4                      ##############################
##################################################################################


GPS <- readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2021/data_prep/VF4step3_clip.rds")
#turn into spatial data
GPS <-   st_as_sf(GPS,
                  coords = c("X", "Y"),
                  crs = 28354,
                  agr = "constant")
names(GPS)
GPS <- GPS %>%  rename(cumulativeAudioCount = cumulative ,
                       cumulativeShockCount = cumulati_1)

GPS <- GPS %>% dplyr::select (ID_jaxs, #got
                              animal_ID, #got
                              #time,
                              local_time, #got
                              date,#got
                              DOY, #got
                              geometry,#got
                              fencesID, #got
                              VF_Fence,
                              # Audio_values,
                              # Shock_values,
                              cumulativeAudioCount, #do I need this one?
                              cumulativeShockCount, #do I need this one?
                              #event, #I think this contains pulse and audio
                              #resting_percentage, #this is in sep data files
                              #moving_percentage,
                              #grazing_percentage,
                              resting_pe, #this is in sep data files
                              moving_per,
                              grazing_pe,
                              #training_period
                              #id, 
                              #sheep, 
                              #treatment,
                              #DOT,
                              
)
names(GPS)

GPS <- GPS %>%  rename(
  resting = resting_pe,
  moving = moving_per,
  grazing = grazing_pe
)



############################################################################################
############                  bring in boundaries             ##############################
############################################################################################


VF4_paddock <-   st_read("W:/VF/2024/spatial/Pinnaroo_2021/VF4_Graze_GDA.shp")
VF4_exclusion_zone <- st_read("W:/VF/2024/spatial/Pinnaroo_2021/VF4_NonGraze.shp")
VF_4_line <-  st_read("W:/VF/2024/spatial/Pinnaroo_2021/VF4_Fence.shp")
############################################################################################

### check by plotting

str(GPS)


ggplot() +
  geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF4_paddock, color = "blue", fill = NA) +
  geom_sf(data = VF_4_line, color = "red", fill = NA) +
  
  geom_sf(data = GPS ,alpha = 0.03) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "check")


############################################################################################


# GPS <- GPS %>% 
#   dplyr::mutate(dist_to_VF = st_distance(GPS, VF_3_line))

GPS <- GPS %>% 
  dplyr::mutate(dist_to_VF = "noVF_line")
############################################################################################
### report if the point is in the exclusion zone

VF4_paddock <- VF4_paddock %>%  dplyr::select(id, geometry)

VF_points <-  st_intersection(GPS, st_difference(VF4_paddock)) %>% 
  dplyr::mutate(VF_EX = "inside_VF")

Exclusion_points <-  st_intersection(GPS, st_difference(VF4_exclusion_zone))%>% 
  dplyr::mutate(VF_EX = "outside_VF")

Exclusion_points <- Exclusion_points %>% dplyr::select(ID_jaxs:dist_to_VF,geometry, VF_EX) #missing id

#ensure the clm names match
names(VF_points)
names(Exclusion_points)
VF_points <- VF_points %>% select(-id)

GPS_all <- rbind(VF_points, Exclusion_points)

str(GPS_all)


GPS_all <- GPS_all %>% dplyr::select(ID_jaxs:dist_to_VF,  VF_EX, geometry) #this hs been modified



coordinates <-as.data.frame( st_coordinates(GPS_all))
GPS_all_df <- as.data.frame(GPS_all)

GPS_all_df <- GPS_all_df %>% 
  dplyr::select(-"geometry")

names(GPS_all_df)
GPS_all_df <-   cbind(GPS_all_df,coordinates )


saveRDS(GPS_all_df,  "W:/VF/2024/animal behaviour data/Pinnaroo2021/data_prep/VF4_step4.rds")



rm(GPS, GPS_all, GPS_all_df, Exclusion_points, coordinates, VF_points, 
   VF_3_line, VF3_exclusion_zone, VF3_paddock)





