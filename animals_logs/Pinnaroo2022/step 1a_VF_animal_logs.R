
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)
library(readxl )

############################################################################################
########    bring in data  this is a massive mess and heaps of data ########################
############################################################################################

## see Pre step1 use QGIS for projection script for Pinnaroo 2022 I used ArcMap

path_step1 <- "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/"
#raw_data <-   "W:/VF/2024/animal behaviour data/Pinnaroo202/Raw data/Projected_use/"

               
#This code only works for some of the data

### I am having some trouble with the time stamp format
## I have downloaded the raw data again and not opened in excel 
# indexed the data and saved as CSV
# imported index csv into QGIS, saved a shapefile projected as WGS and then projected as GDA and add geom clm saved as CSV file
# if the time stamp clm get messed up I was going to use the index data fram to match it but its working

# CSIRO_Pinnaroo_from_email_no_proj <- read_csv("H:/Output-2/Jax_temp_temp/trial_csiro_pinnaroo_mob_273_angus_heifers_filtered.csv", 
#                                       col_types = cols(timeOfEven = col_character()))
# 

VF_animal_GPS_data <- read_csv("W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/XYtrial_csiro_pinnaroo_mob_273_angus_heifers_filtered_GDA.csv")

str(VF_animal_GPS_data)                              
# #index the data
VF_animal_GPS_data <- VF_animal_GPS_data  %>%   
  mutate(rowid = row_number()) %>%  select(rowid, everything())
# write_csv(CSIRO_Pinnaroo_from_email_no_proj, "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/trial_csiro_pinnaroo_mob_273_angus_heifers_filtered.csv")
head(VF_animal_GPS_data$timeOfEven)
VF_animal_GPS_data$timeOfEven_v1 <- 
  as.POSIXct(VF_animal_GPS_data$timeOfEven, format="%d/%m/%Y %H:%M", tz="UTC")


head(VF_animal_GPS_data$timeOfEven_v1)
check_it_worked <-VF_animal_GPS_data %>%  select(timeOfEven, timeOfEven_v1)
rm(check_it_worked)
################################################################################
###                    Local local_time          #############
################################################################################


str(VF_animal_GPS_data)
#str(No_VF_animal_GPS_data)

#VF_animal_GPS_data <- VF_animal_GPS_data %>% rename(timeOfEvent = timeOfEven_v1)
#No_VF_animal_GPS_data <- No_VF_animal_GPS_data %>% rename(timeOfEvent = timeOfEven)





#format time and date clm from character to time
# VF_animal_GPS_data <-
#   VF_animal_GPS_data %>%
#   mutate(timeOfEvent = as.POSIXct(timeOfEvent, tz = "GMT", format = "%Y/%m/%d %H:%M"))

str(VF_animal_GPS_data$timeOfEven)
str(VF_animal_GPS_data$timeOfEven_v1)

VF_animal_GPS_data <- VF_animal_GPS_data %>% 
  mutate(GMT = dmy_hm(timeOfEven, tz = "GMT"))
str(VF_animal_GPS_data$GMT)

VF_animal_GPS_data <- VF_animal_GPS_data %>% 
  mutate(local_time = with_tz(GMT, tz = "Australia/Adelaide"))
## Add a clm for ID_jaxs
VF_animal_GPS_data <- VF_animal_GPS_data %>% 
  dplyr::mutate( ID_jaxs = row_number())
VF_animal_GPS_data <- VF_animal_GPS_data %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))

check_it_worked <-VF_animal_GPS_data %>%  select(
  rowid, 
  #ID_jaxs, 
  timeOfEven_v1, 
  timeOfEven, 
  GMT,local_time,  
  date, 
  DOY)
rm(check_it_worked)








################################################################################
### Umm this is a problem because the workflow is slightly different.
## Lets see if I can trim based on time first  I will 
#1. Collar ID is converted to a collar ID and time clm (where the time is set to TZ = Australia/Adelaide)
#2. the Collar ID and animal ID has been adjusted so that the data accommodated changes in collar ID.

##########################################################################################################
#############          Add clm for fence name     ########################################################
##########################################################################################################
unique(VF_animal_GPS_data$fencesID)
unique(VF_animal_GPS_data$ID_jaxs)

## I am not sure what is what...maybe the date and time might give me a clue


max_min_time_fence <- VF_animal_GPS_data %>% 
  group_by(fencesID) %>% 
  summarise(min_time = min(local_time, na.rm = TRUE),
            max_time = max(local_time, na.rm = TRUE)) %>% 
  arrange(min_time)
max_min_time_fence


#This file helps
VF_details <- read_csv("W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/trial_csiro_pinnaroo_vf_gps_location_jaxs.csv")
VF_details
VF_details <- VF_details %>% rename(
  created_on = boundary_created_on,
  fence_Id_2 = boundary_id,
  fence_Id =vp_name2
)

list_ofFenceID_VF_area <-VF_details %>%  
  distinct(fence_Id, .keep_all = TRUE) %>% 
  select(vp_name, fence_Id, VF_area, created_on, fence_Id_2)
list_ofFenceID_VF_area

names(VF_animal_GPS_data)
fences_names_supplied_data <-VF_animal_GPS_data %>%  distinct(fencesID, .keep_all = TRUE) %>% select(fencesID)
fences_names_supplied_data


VF_animal_GPS_data <- VF_animal_GPS_data %>% 
  mutate(
    VF_Fence = case_when(
    fencesID ==   "1a459" ~ "VP01",
    fencesID ==   "1a2b9" ~ "VP02",
    fencesID ==   "14143" ~ "VP03",
    fencesID ==   "1b424" ~ "VP04",
    fencesID ==   "18711" ~ "VP05",
    fencesID ==   "1cdd8" ~ "VP06", 
    .default ="no_fence_assigned"))
    
 
check <- VF_animal_GPS_data %>%  distinct(fencesID, VF_Fence, .keep_all = TRUE) %>%     
  select(VF_Fence, fencesID) %>% 
  arrange(VF_Fence)
check
Local_time_each_fence <- VF_animal_GPS_data %>% 
  group_by(fencesID, VF_Fence) %>% 
  summarise(min_local_time = min(local_time, na.rm=TRUE),
            max_loca_time = max(local_time, na.rm = TRUE)) %>% 
  arrange(min_local_time)

Local_time_each_fence

###########################################################################################################
## Some ear tags were not on our list
ear_tags_not_on_animal_list <- read_excel("W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/animal_wt_ID_ear_tags.xlsx", 
                                         sheet = "not on our animal list")

ear_tags_not_on_animal_list <- ear_tags_not_on_animal_list %>% 
  rename(deviceName= Neckband)


VF_animal_GPS_data <- VF_animal_GPS_data %>%
  anti_join(ear_tags_not_on_animal_list,VF_animal_GPS_data, by = "deviceName")



##########################################################################################################
#############    Trim the whole df based on start and end of trial   ######################################
##########################################################################################################
VF_animal_GPS_data <- filter(VF_animal_GPS_data,
              between(local_time,
                      ymd_hms('2022-07-20 14:53:00', tz="Australia/Adelaide"),
                      ymd_hms('2022-07-29 06:30:00', tz="Australia/Adelaide")))# this is from the Jims notes

##########################################################################################################
#############                VF 1                 ########################################################
##########################################################################################################

names(VF_animal_GPS_data)
str(VF_animal_GPS_data$VF_Fence)
#I don't have accurate time keeping for when the fence was moved, I will use max and min local time for each fence
VF1 <- filter(VF_animal_GPS_data,
                            between(local_time,
                                    ymd_hms('2022-07-20 14:53:00', tz="Australia/Adelaide"),
                                    ymd_hms('2022-07-22 12:23:00', tz="Australia/Adelaide")))
VF1 <- VF1 %>% filter(VF_Fence == "VP01"  ) # I don't think I can do do this because some entries have no fence
VF1 <- VF1 %>% mutate(VF_Fence = "fence1")

#early time to late time
max_min_time_fence %>% filter(fencesID == "1a459") 
list_ofFenceID_VF_area %>% filter(fence_Id == "1a459") 


min(VF1$local_time)
max(VF1$local_time)
unique(VF1$VF_Fence) #
unique(VF1$fencesID)
# VF1_sf <-
#   st_as_sf(VF1,
#            coords = c("gpsData.lng", "gpsData.lat"),
#            crs = 4326,
#            agr = "constant")
# 
# VF1_sf_trans <-
#   st_transform(VF1_sf, crs = 28354)



##########################################################################################################
#############    assign the collar ID to animal ID  VF 1 ########################################################
##########################################################################################################
unique(VF1$deviceName)
VF1 %>%  distinct(deviceName) %>% tally
#35 cows (this is the same cows that were weighed)

## all VF1 device names match neckband number in animal wt dataset = no need to change anything 45cows

VF1 <-VF1 %>%  mutate(animal_ID = deviceName) 

#check we are assignining all the collar ID to animal names
head(VF1)
with(VF1, table(date, animal_ID))

#the location of the NA
NA_VF1 <- filter(VF1,animal_ID == "NA")
with(NA_VF1, table(date, deviceName))





##########################################################################################################
#############                VF 2                 ########################################################
##########################################################################################################
#early time to late time
max_min_time_fence %>% filter(fencesID == "1a2b9") 
list_ofFenceID_VF_area


VF2 <- filter(VF_animal_GPS_data,
                            between(
                              local_time,
                              ymd_hms('2022-07-22 12:23:00', tz = "Australia/Adelaide"),
                              ymd_hms('2022-07-22 12:45:00', tz = "Australia/Adelaide")
                            ))

#VF2 <- VF2 %>% filter(VF_Fence != "VP02") #(not sure I can do this what about the NA)
VF2 <- VF2 %>% filter(fencesID == "1a2b9") #(not sure I can do this what about the NA)

min(VF2$local_time)
max(VF2$local_time)
unique(VF2$VF_Fence) #
unique(VF2$fencesID)

VF2 <- VF2 %>% mutate(VF_Fence = "fence2"  )

##########################################################################################################
#############    assign the collar ID to animal ID  VF 2 ########################################################
##########################################################################################################
VF2 %>%  distinct(deviceName) %>% tally
#34 cows (this is the same cows that were weighed)

VF2 <-VF2 %>%  mutate(animal_ID = deviceName) 

#check we are assignining all the collar ID to animal names
head(VF2)
with(VF2, table(date, animal_ID))

#the location of the NA
NA_VF2 <- filter(VF2,animal_ID == "NA")
with(NA_VF2, table(date, deviceName))




##########################################################################################################
#############                VF 3                 ########################################################
##########################################################################################################
unique(VF_animal_GPS_data$fencesID)
#early time to late time
max_min_time_fence %>% filter(fencesID == "14143" ) 
list_ofFenceID_VF_area

VF3 <- filter(VF_animal_GPS_data,
              between(
                local_time,
                ymd_hms('2022-07-22 13:13:00', tz = "Australia/Adelaide"),
                ymd_hms('2022-07-25 10:03:00', tz = "Australia/Adelaide")
              ))

VF3 <- VF3 %>% filter(VF_Fence != "VP04" ) %>% 
  filter(VF_Fence != "VP02"& VF_Fence != "VP01") #(not sure I can do this what about the NA)

min(VF3$local_time)
max(VF3$local_time)
unique(VF3$VF_Fence) #
unique(VF3$fencesID)

#VF3 <- VF3 %>% filter(VF_Fence == "fence3"  )
VF3 <- VF3 %>% mutate(VF_Fence = "fence3"  )

#########################################################################################################
#############    assign the collar ID to animal ID  VF 3 ########################################################
##########################################################################################################
VF3 %>%  distinct(deviceName) %>% tally
#35 cows (this is the same cows that were weighed)

VF3 <-VF3 %>%  mutate(animal_ID = deviceName) 

#check we are assignining all the collar ID to animal names
head(VF3)
with(VF3, table(date, animal_ID))

#the location of the NA
NA_VF3 <- filter(VF3,animal_ID == "NA")
with(NA_VF3, table(date, deviceName))




##########################################################################################################
#############                 VF 4                   ########################################################
##########################################################################################################

#early time to late time
max_min_time_fence %>% filter(fencesID == "1b424" ) 
list_ofFenceID_VF_area

VF4 <- filter(VF_animal_GPS_data,
              between(
                local_time,
                ymd_hms('2022-07-25 10:13:00', tz = "Australia/Adelaide"),
                ymd_hms('2022-07-26 10:53:00', tz = "Australia/Adelaide")
              ))

VF4 <- VF4 %>% filter(VF_Fence != "VP03" ) #(not sure I can do this what about the NA)

min(VF4$local_time)
max(VF4$local_time)
unique(VF4$VF_Fence) 
VF4 <- VF4 %>% mutate(VF_Fence = "fence4"  )

#########################################################################################################
#############    assign the collar ID to animal ID  ########################################################
##########################################################################################################
VF4 %>%  distinct(deviceName) %>% tally
#34 cows (this is the same cows that were weighed)

VF4 <-VF4 %>%  mutate(animal_ID = deviceName) 

#check we are assignining all the collar ID to animal names
head(VF4)
with(VF4, table(date, animal_ID))

#the location of the NA
NA_VF4 <- filter(VF4,animal_ID == "NA")
with(NA_VF4, table(date, deviceName))


##########################################################################################################
#############                 VF 5                   ########################################################
##########################################################################################################

#early time to late time
max_min_time_fence %>% filter(fencesID == "18711" ) 
list_ofFenceID_VF_area

VF5 <- filter(VF_animal_GPS_data,
              between(
                local_time,
                ymd_hms('2022-07-26 11:03:00', tz = "Australia/Adelaide"),
                ymd_hms('2022-07-27 11:53:00', tz = "Australia/Adelaide")
              ))

VF5 <- VF5 %>% filter(VF_Fence != "VP04" ) #(not sure I can do this what about the NA)

min(VF5$local_time)
max(VF5$local_time)
unique(VF5$VF_Fence) 
VF5 <- VF5 %>% mutate(VF_Fence = "fence5"  )

#########################################################################################################
#############    assign the collar ID to animal ID  ########################################################
##########################################################################################################
VF5 %>%  distinct(deviceName) %>% tally
#34 cows (this is the same cows that were weighed)

VF5 <-VF5 %>%  mutate(animal_ID = deviceName) 

#check we are assignining all the collar ID to animal names
head(VF5)
with(VF5, table(date, animal_ID))

#the location of the NA
NA_VF5 <- filter(VF5,animal_ID == "NA")
with(NA_VF5, table(date, deviceName))


##########################################################################################################
#############                 VF 6                   ########################################################
##########################################################################################################

#early time to late time
max_min_time_fence %>% filter(fencesID == "1cdd8" ) 
list_ofFenceID_VF_area

VF6 <- filter(VF_animal_GPS_data,
              between(
                local_time,
                ymd_hms('2022-07-27 12:03:00', tz = "Australia/Adelaide"),
                ymd_hms('2022-07-29 06:23:00', tz = "Australia/Adelaide")
              ))

VF6 <- VF6 %>% filter(VF_Fence != "VP05" ) #(not sure I can do this what about the NA)

min(VF6$local_time)
max(VF6$local_time)
unique(VF6$VF_Fence) 
VF6 <- VF6 %>% mutate(VF_Fence = "fence6"  )

#########################################################################################################
#############    assign the collar ID to animal ID  ########################################################
##########################################################################################################
VF6 %>%  distinct(deviceName) %>% tally
#34 cows (this is the same cows that were weighed)

VF6 <-VF6 %>%  mutate(animal_ID = deviceName) 

#check we are assignining all the collar ID to animal names
head(VF6)
with(VF6, table(date, animal_ID))

#the location of the NA
NA_VF6 <- filter(VF6,animal_ID == "NA")
with(NA_VF5, table(date, deviceName))





check <- rbind(VF1, 
               VF2, 
               VF3, 
               VF4,
               VF5,
               VF6
               )

names(check)

test <- check %>% count(animal_ID, VF_Fence) 
test2 <- test %>%  count(VF_Fence)

test <- check %>% count(animal_ID, date) 
test2 <- test %>%  count(date)

list_neck_bands <- as.data.frame(distinct(check,deviceName)) %>% arrange(deviceName)
list_neck_bands

## nb this list matches the list of animals weighted which is 35
## This is a bit weird most days we only have 34 collar records which one is missing?

str(check)
test <- check %>% 
  filter(VF_Fence == "fence6") %>% 
  count(animal_ID, VF_Fence) %>% arrange(animal_ID)


##########################################################################################################
"W:\VF\2024\animal behaviour data\Pinnaroo2022\data_prep"

##########################################################################################################
## save files ###
saveRDS(VF1,              "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF1.rds")
saveRDS(VF2,              "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF2.rds")
saveRDS(VF3,              "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF3.rds")
saveRDS(VF4,               "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF4.rds")

saveRDS(VF5,               "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF5.rds")
saveRDS(VF6,               "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF6.rds")

#saveRDS(No_VF_animal_GPS_data,  "W:/VF/2024/animal behaviour data/XXX/data_prep/No_VF_animal_GPS_data_1_4.rds")

# VF11_9370004 <- VF1 %>% filter(deviceName == 9370004)
# 
# write.csv(VF11_9370004, "W:/VF/2024/animal behaviour data/xx/Raw data/Projected_test/VF1_step1_9370004.csv")
# saveRDS(VF11_9370004,  "W:/VF/2024/animal behaviour data/xx/Raw data/Projected_test/VF1_step1_9370004.rds")


##########################################################################################################
### make sure the label VF aligns with the bounday and dates
str(VF1)
names(check)
unique(check$VF_Fence)

Hard_fence_bound <- st_read("W:/VF/2024/spatial/Pinnaroo_2022/Paddock_outline.shp")  # this is the hard fences
Hard_fence_bound <-
  st_transform(Hard_fence_bound, crs = 28354)

VF_fence_bound <- st_read("W:/VF/2024/spatial/Pinnaroo_2022/all_VF.shp")  # 
VF_fence_bound <-
  st_transform(VF_fence_bound, crs = 28354)

VF_fence_VF1 <- st_read("W:/VF/2024/spatial/Pinnaroo_2022/VF1_Fence.shp")
VF_fence_VF2 <- st_read("W:/VF/2024/spatial/Pinnaroo_2022/VF2_Fence.shp")
VF_fence_VF3 <- st_read("W:/VF/2024/spatial/Pinnaroo_2022/VF3_Fence.shp")
VF_fence_VF4 <- st_read("W:/VF/2024/spatial/Pinnaroo_2022/VF4_Fence.shp")
VF_fence_VF5 <- st_read("W:/VF/2024/spatial/Pinnaroo_2022/VF5_Fence.shp")
VF_fence_VF6 <- st_read("W:/VF/2024/spatial/Pinnaroo_2022/VF6_Fence.shp")

check_sf <-
  st_as_sf(check,
           coords = c("xcoord", "ycoord"),
           crs = 28354, #
           agr = "constant")



ggplot() +
  geom_sf(data = Hard_fence_bound, color = "black", fill = NA) +
  #geom_sf(data = VF_fence_VF1, color = "blue", fill = NA) +
  #geom_sf(data = VF_fence_VF2, color = "red", fill = NA) +
  #geom_sf(data = VF_fence_VF3, color = "green", fill = NA) + # 
  #geom_sf(data = VF_fence_VF4, color = "blue", fill = NA) + # 
  #geom_sf(data = VF_fence_VF5, color = "red", fill = NA) + # 
  geom_sf(data = VF_fence_VF6, color = "green", fill = NA) + # 
  
  #geom_sf(data = filter(check_sf, VF_Fence ==  "fence1"),alpha = 0.03) +
  #geom_sf(data = filter(check_sf, VF_Fence ==  "fence2"),alpha = 0.03) +
  #geom_sf(data = filter(check_sf, VF_Fence ==  "fence3"),alpha = 0.03) +
  #geom_sf(data = filter(check_sf, VF_Fence ==  "fence4"),alpha = 0.03) +
  #geom_sf(data = filter(check_sf, VF_Fence ==  "fence5"),alpha = 0.03) +
  geom_sf(data = filter(check_sf, VF_Fence ==  "fence6"),alpha = 0.03) +
  theme_bw()+
  facet_wrap(.~ date)+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "check")
