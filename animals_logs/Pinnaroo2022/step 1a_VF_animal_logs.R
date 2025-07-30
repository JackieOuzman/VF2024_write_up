
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)


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

# CSIRO_Pinnaroo_from_email_no_proj <- read_csv("H:/Output-2/Jax_temp_temp/CSIRO_Pinnaroo from email.csv", 
#                                       col_types = cols(timeOfEven = col_character()))
# 
# #index the data
# CSIRO_Pinnaroo_from_email_no_proj <- CSIRO_Pinnaroo_from_email_no_proj  %>%   mutate(rowid = row_number()) %>%  select(rowid, everything())
# write_csv(CSIRO_Pinnaroo_from_email_no_proj, "H:/Output-2/Jax_temp_temp/CSIRO_Pinnaroo_from_email_no_proj_index.csv")

                              
VF_animal_GPS_data <- read_csv("W:/VF/2024/animal behaviour data/Pinnaroo2022/Jax_temp_temp/CSIRO_Pinnaroo from email GDA index v2.csv", 
                                              col_types = cols(timeOfEven = col_character()))



head(VF_animal_GPS_data$timeOfEven)
VF_animal_GPS_data$timeOfEven_v1 <- 
  as.POSIXct(VF_animal_GPS_data$timeOfEven, format="%Y/%m/%d %H:%M", tz="UTC")


head(VF_animal_GPS_data$timeOfEven_v1)
check_it_worked <-VF_animal_GPS_data %>%  select(timeOfEven, timeOfEven_v1)
rm(check_it_worked)
################################################################################
###                    Local local_time          #############
################################################################################


str(VF_animal_GPS_data)
#str(No_VF_animal_GPS_data)

VF_animal_GPS_data <- VF_animal_GPS_data %>% rename(timeOfEvent = timeOfEven_v1)
#No_VF_animal_GPS_data <- No_VF_animal_GPS_data %>% rename(timeOfEvent = timeOfEven)


#format time and date clm from character to time
VF_animal_GPS_data <-
  VF_animal_GPS_data %>%
  mutate(timeOfEvent = as.POSIXct(timeOfEvent, tz = "GMT", format = "%Y/%m/%d %H:%M"))


VF_animal_GPS_data <- VF_animal_GPS_data %>% 
  mutate(GMT = ymd_hms(timeOfEvent, tz = "GMT"))
VF_animal_GPS_data <- VF_animal_GPS_data %>% 
  mutate(local_time = with_tz(GMT, tz = "Australia/Adelaide"))
## Add a clm for ID_jaxs
VF_animal_GPS_data <- VF_animal_GPS_data %>% 
  dplyr::mutate( ID_jaxs = row_number())
VF_animal_GPS_data <- VF_animal_GPS_data %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))

check_it_worked <-VF_animal_GPS_data %>%  select(rowid, ID_jaxs, timeOfEven, timeOfEvent, GMT,local_time,  date, DOY)
rm(check_it_worked)








################################################################################
### Umm this is a problem because the workflow is slightly different.
## Lets see if I can trim based on time first first I will 
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
VF_details <- read_csv("W:/VF/Pinnaroo/activation_VF/FenceMapping_v2_edited.csv")
VF_details
list_ofFenceID_VF_area <-VF_details %>%  distinct(fence_Id, .keep_all = TRUE) %>% select(vp_name, fence_Id, VF_area, created_on)
list_ofFenceID_VF_area

names(VF_animal_GPS_data)
fences_names_supplied_data <-VF_animal_GPS_data %>%  distinct(fencesID, .keep_all = TRUE) %>% select(fencesID)
fences_names_supplied_data


VF_animal_GPS_data <- VF_animal_GPS_data %>% 
  mutate(
    VF_Fence = case_when(
    fencesID ==   "165e0" ~ "VP01",
    fencesID ==   "1f6da" ~ "VP02",
    fencesID ==   "1d10b" ~ "VP03",
    fencesID ==   "1ab95" ~ "VP03",
    fencesID ==   "1df99" ~ "VP04",
    fencesID ==   "1f076" ~ "VP03", # this is from the R markdown report
    .default ="no_fence_assigned"))
    
 
check <- VF_animal_GPS_data %>%  distinct(fencesID, VF_Fence, .keep_all = TRUE) %>%     select(VF_Fence, fencesID)
check
Local_time_each_fence <- VF_animal_GPS_data %>% 
  group_by(fencesID) %>% 
  summarise(min_local_time = min(local_time, na.rm=TRUE),
            max_loca_time = max(local_time, na.rm = TRUE)) %>% 
  arrange(min_local_time)

Local_time_each_fence

##########################################################################################################
#############    Trim the whole df based on start and end of trial   ######################################
##########################################################################################################
VF_animal_GPS_data <- filter(VF_animal_GPS_data,
              between(local_time,
                      ymd_hms('2021-10-07 16:03:00', tz="Australia/Adelaide"),
                      ymd_hms('2021-10-20 08:54:00', tz="Australia/Adelaide")))# this is from the R markdown report

##########################################################################################################
#############                VF 1                 ########################################################
##########################################################################################################

names(VF_animal_GPS_data)

#I don't have accurate time keeping for when the fence was moved, I will use max and min local time for each fence
VF1 <- filter(VF_animal_GPS_data,
                            between(local_time,
                                    ymd_hms('2021-10-07 17:11:00', tz="Australia/Adelaide"),
                                    ymd_hms('2021-10-11 12:23:00', tz="Australia/Adelaide")))
#VF1 <- VF1 %>% filter(VF_Fence == "fence1"  ) # I don't think I can do do this because some entries have no fence
VF1 <- VF1 %>% mutate(VF_Fence = "fence1")

#early time to late time
max_min_time_fence %>% filter(fencesID == "165e0") 
list_ofFenceID_VF_area %>% filter(fence_Id == "1.65E+02") 


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
#39 cows (this is the same cows that were weighed)

## all VF1 device names match neckband number in animal wt dataset = no need to change anything 39cows

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
max_min_time_fence %>% filter(fencesID == "1f6da") 
list_ofFenceID_VF_area


VF2 <- filter(VF_animal_GPS_data,
                            between(
                              local_time,
                              ymd_hms('2021-10-11 12:33:00', tz = "Australia/Adelaide"),
                              ymd_hms('2021-10-12 09:04:00', tz = "Australia/Adelaide")
                            ))

VF2 <- VF2 %>% filter(VF_Fence != "VP03") #(not sure I can do this what about the NA)
VF2 <- VF2 %>% filter(fencesID != "1d10b") #(not sure I can do this what about the NA)

min(VF2$local_time)
max(VF2$local_time)
unique(VF2$VF_Fence) #
unique(VF2$fencesID)

VF2 <- VF2 %>% mutate(VF_Fence = "fence2"  )

##########################################################################################################
#############    assign the collar ID to animal ID  VF 2 ########################################################
##########################################################################################################
VF2 %>%  distinct(deviceName) %>% tally
#39 cows (this is the same cows that were weighed)

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
max_min_time_fence %>% filter(fencesID == "1d10b" | fencesID == "1ab95" | fencesID == "1f076") 
list_ofFenceID_VF_area

VF3 <- filter(VF_animal_GPS_data,
              between(
                local_time,
                ymd_hms('2021-10-12 09:03:00', tz = "Australia/Adelaide"),
                ymd_hms('2021-10-17 09:33:00', tz = "Australia/Adelaide")
              ))

VF3 <- VF3 %>% filter(VF_Fence != "VP04" ) %>% 
  filter(VF_Fence != "VP02") #(not sure I can do this what about the NA)

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
#39 cows (this is the same cows that were weighed)

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
max_min_time_fence %>% filter(fencesID == "1df99" ) 
list_ofFenceID_VF_area

VF4 <- filter(VF_animal_GPS_data,
              between(
                local_time,
                ymd_hms('2021-10-17 09:33:00', tz = "Australia/Adelaide"),
                ymd_hms('2021-10-20 08:53:00', tz = "Australia/Adelaide")
              ))

VF4 <- VF4 %>% filter(VF_Fence != "VP03" ) #(not sure I can do this what about the NA)

min(VF4$local_time)
max(VF4$local_time)
unique(VF4$VF_Fence) 
VF4 <- VF4 %>% mutate(VF_Fence = "fence4"  )

#########################################################################################################
#############    assign the collar ID to animal ID  deactivation ########################################################
##########################################################################################################
VF4 %>%  distinct(deviceName) %>% tally
#39 cows (this is the same cows that were weighed)

VF4 <-VF4 %>%  mutate(animal_ID = deviceName) 

#check we are assignining all the collar ID to animal names
head(VF4)
with(VF4, table(date, animal_ID))

#the location of the NA
NA_VF4 <- filter(VF4,animal_ID == "NA")
with(NA_VF4, table(date, deviceName))





check <- rbind(VF1, 
               VF2, 
               VF3, 
               VF4
               )




##########################################################################################################
"W:\VF\2024\animal behaviour data\Pinnaroo2021\data_prep"

##########################################################################################################
## save files ###
saveRDS(VF1,              "W:/VF/2024/animal behaviour data/Pinnaroo2021/data_prep/VF1.rds")
saveRDS(VF2,              "W:/VF/2024/animal behaviour data/Pinnaroo2021/data_prep/VF2.rds")
saveRDS(VF3,              "W:/VF/2024/animal behaviour data/Pinnaroo2021/data_prep/VF3.rds")
saveRDS(VF4,               "W:/VF/2024/animal behaviour data/Pinnaroo2021/data_prep/VF4.rds")

#saveRDS(No_VF_animal_GPS_data,  "W:/VF/2024/animal behaviour data/XXX/data_prep/No_VF_animal_GPS_data_1_4.rds")

# VF11_9370004 <- VF1 %>% filter(deviceName == 9370004)
# 
# write.csv(VF11_9370004, "W:/VF/2024/animal behaviour data/xx/Raw data/Projected_test/VF1_step1_9370004.csv")
# saveRDS(VF11_9370004,  "W:/VF/2024/animal behaviour data/xx/Raw data/Projected_test/VF1_step1_9370004.rds")

