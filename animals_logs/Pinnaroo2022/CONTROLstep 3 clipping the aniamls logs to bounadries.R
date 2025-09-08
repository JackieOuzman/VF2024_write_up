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
Hard_fence_bound <- st_read("W:/VF/2024/spatial/Pinnaroo_2022/Paddock_outline.shp")  # this is the hard fences
Hard_fence_bound <-
  st_transform(Hard_fence_bound, crs = 28354)

control_bound <- st_read("W:/VF/2024/spatial/Pinnaroo_2022/control.shp")  # this is the hard fences
control_bound <-
  st_transform(control_bound, crs = 28354)


VF_fence_bound <- st_read("W:/VF/2024/spatial/Pinnaroo_2022/all_VF.shp")  # 
VF_fence_bound <-
  st_transform(VF_fence_bound, crs = 28354)

VF_fence_VF1 <- st_read("W:/VF/2024/spatial/Pinnaroo_2022/VF1_Fence.shp")
VF_fence_VF2 <- st_read("W:/VF/2024/spatial/Pinnaroo_2022/VF2_Fence.shp")
VF_fence_VF3 <- st_read("W:/VF/2024/spatial/Pinnaroo_2022/VF3_Fence.shp")
VF_fence_VF4 <- st_read("W:/VF/2024/spatial/Pinnaroo_2022/VF4_Fence.shp")
VF_fence_VF5 <- st_read("W:/VF/2024/spatial/Pinnaroo_2022/VF5_Fence.shp")
VF_fence_VF6 <- st_read("W:/VF/2024/spatial/Pinnaroo_2022/VF6_Fence.shp")



ggplot() +
  geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF_fence_bound ,color = "blue", fill = NA) 

################################################################

path_step1 <- "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/"


################################################################
### Clip to the VF1 hard fences  with    #########
################################################################

step1_2_control <- readRDS(paste0(path_step1, "control.rds"))



## Add a clm for ID_jaxs
step1_2_control <- step1_2_control %>% 
  dplyr::mutate( ID_jaxs = row_number())


step1_2_control <- step1_2_control %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))



names(step1_2_control)

#turn into spatial data when I project the data first and add x and y and save as CSV

step1_2_control_sf <-
  st_as_sf(step1_2_control,
           coords = c("xcoord", "ycoord"),
           crs = 28354, #
           agr = "constant")

##quick check
# st_write(step1_2_VF1_sf, "W:/VF/2024/animal behaviour data/Pinnaroo2021/CHECKstep1_2_VF1_sf_project_first.shp", layer_options = "GEOMETRY=AS_XY")
# write_csv(step1_2_VF1, "W:/VF/2024/animal behaviour data/Pinnaroo2021/CHECK.csv")
# 
 ggplot() +
   geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
   geom_sf(data = VF_fence_bound ,color = "blue", fill = NA) +
   geom_sf(data = control_bound ,color = "red", fill = NA) +
   geom_sf(data = step1_2_control_sf ,color = "green") 




GPS_sf_trans_control <-step1_2_control_sf



#To the large block boundary with buffer
GPS_sf_trans_control_clip <-
  st_intersection(GPS_sf_trans_control, st_difference(Hard_fence_bound)) #this 'st_difference' function is supposed to remove the duplication

#To the large block boundary with buffer
GPS_sf_trans_control_clip_control <-
  st_intersection(GPS_sf_trans_control, st_difference(control_bound)) #this 'st_difference' function is supposed to remove the duplication



ggplot() +
  geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
  #geom_sf(data = GPS_sf_trans_control_clip ,alpha = 0.03) +
  geom_sf(data = GPS_sf_trans_control_clip_control ,alpha = 0.03) +
   theme_bw()+
   facet_wrap(.~ date)+
   theme(legend.position = "none",
         axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
   labs(title = "clipped to the hard fence")

## convert the geom clm into x and y clms


coordinates <-as.data.frame( st_coordinates(GPS_sf_trans_control_clip_control))
GPS_sf_trans_control_clip_control_df <- as.data.frame(GPS_sf_trans_control_clip_control)

GPS_sf_trans_control_clip_control_df <- GPS_sf_trans_control_clip_control_df %>% 
  dplyr::select(-"geometry")


GPS_sf_trans_control_clip_control_df <-   cbind(GPS_sf_trans_control_clip_control_df,coordinates )




saveRDS(GPS_sf_trans_control_clip_control_df,  "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/control_step3_clip.rds")

write_csv(GPS_sf_trans_control_clip_control_df,
          "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/control_step3_clip.csv") 



