
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

## see Pre step1 use QGIS for projection script

path_step1 <- "W:/VF/2024/animal behaviour data/Long Plain/data_prep/"
raw_data <-   "W:/VF/2024/animal behaviour data/Long Plain/Raw data/Projected_use/"
               

No_VF_animal_GPS_data_1_4 <- read_csv(paste0(raw_data, "No_VF_animal_GPS_data_1_4_GDA.csv"), 
                                      col_types = cols(timeOfEven = col_datetime(format = " %Y/%m/%d %H:%M:%S")))
VF_animal_GPS_data_1_4 <- read_csv(paste0(raw_data, "VF_animal_GPS_data_1_4_GDA.csv"),
                                   col_types = cols(timeOfEven = col_datetime(format = " %Y/%m/%d %H:%M:%S")))




################################################################################
###                    Local local_time          #############
################################################################################
str(VF_animal_GPS_data_1_4)
str(No_VF_animal_GPS_data_1_4)

VF_animal_GPS_data_1_4 <- VF_animal_GPS_data_1_4 %>% rename(timeOfEvent = timeOfEven)
No_VF_animal_GPS_data_1_4 <- No_VF_animal_GPS_data_1_4 %>% rename(timeOfEvent = timeOfEven)


#format time and date clm from character to time
VF_animal_GPS_data_1_4 <-
  VF_animal_GPS_data_1_4 %>%
  mutate(timeOfEvent = as.POSIXct(timeOfEvent, tz = "GMT", format = "%d/%m/%Y %H:%M"))


VF_animal_GPS_data_1_4 <- VF_animal_GPS_data_1_4 %>% 
  mutate(GMT = ymd_hms(timeOfEvent, tz = "GMT"))
VF_animal_GPS_data_1_4 <- VF_animal_GPS_data_1_4 %>% 
  mutate(local_time = with_tz(GMT, tz = "Australia/Adelaide"))
## Add a clm for ID_jaxs
VF_animal_GPS_data_1_4 <- VF_animal_GPS_data_1_4 %>% 
  dplyr::mutate( ID_jaxs = row_number())
VF_animal_GPS_data_1_4 <- VF_animal_GPS_data_1_4 %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))



No_VF_animal_GPS_data_1_4 <-
  No_VF_animal_GPS_data_1_4 %>%
  mutate(timeOfEvent = as.POSIXct(timeOfEvent, tz = "GMT", format = "%d/%m/%Y %H:%M"))
No_VF_animal_GPS_data_1_4 <- No_VF_animal_GPS_data_1_4 %>% 
  mutate(GMT = ymd_hms(timeOfEvent, tz = "GMT"))
No_VF_animal_GPS_data_1_4 <- No_VF_animal_GPS_data_1_4 %>% 
  mutate(local_time = with_tz(GMT, tz = "Australia/Adelaide"))
## Add a clm for ID_jaxs
No_VF_animal_GPS_data_1_4 <- No_VF_animal_GPS_data_1_4 %>% 
  dplyr::mutate( ID_jaxs = row_number())
No_VF_animal_GPS_data_1_4 <- No_VF_animal_GPS_data_1_4 %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))







################################################################################
### Umm this is a problem because the workflow is slightly different.
## Lets see if I can trim based on time first first I will 
#1. Collar ID is converted to a collar ID and time clm (where the time is set to TZ = Australia/Adelaide)
#2. the Collar ID and animal ID has been adjusted so that the data accommodated changes in collar ID.

##########################################################################################################
#############          Add clm for fence name     ########################################################
##########################################################################################################
VF_animal_GPS_data_1_4 <- VF_animal_GPS_data_1_4 %>% 
  mutate(
    VF_Fence = case_when(
    fencesID ==   "19000" ~ "fence1",
    fencesID ==   "1922f" ~ "fence2",
    fencesID ==   "11eab" ~ "fence3"))#,
    #.default ="deactive_VF3"))
    
    
No_VF_animal_GPS_data_1_4 <- No_VF_animal_GPS_data_1_4 %>% 
  mutate(VF_Fence = "noVF")
     
 unique(VF_animal_GPS_data_1_4$fencesID)   

##########################################################################################################
#############                VF 1                 ########################################################
##########################################################################################################

names(VF_animal_GPS_data_1_4)

VF1 <- filter(VF_animal_GPS_data_1_4, 
                            between(local_time, 
                                    ymd_hms('2020-10-21 14:55:00', tz="Australia/Adelaide"),
                                    ymd_hms('2020-10-25 10:50:00', tz="Australia/Adelaide"))) 
#VF1 <- VF1 %>% filter(VF_Fence == "fence1"  ) # I don't think I can do do this because some entries have no fence
VF1 <- VF1 %>% mutate(VF_Fence = "fence1")

unique(VF1$fencesID)
unique(VF1$VF_Fence)

#early time to late time

# activation_fence1 2020-10-21 14:55:00
# activation_fence2 2020-10-25 10:50:00

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
## all VF1 device names match neckband number in animal wt dataset = no need to change anything 20cows

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


VF2 <- filter(VF_animal_GPS_data_1_4,
                            between(
                              local_time,
                              ymd_hms('2020-10-25 10:50:00', tz = "Australia/Adelaide"),
                              ymd_hms('2020-10-30 08:03:00', tz = "Australia/Adelaide")
                            ))

# activation_fence2 2020-10-25 10:50:00
# activation_fence3 2020-10-30 08:03:00

min(VF2$local_time)
max(VF2$local_time)
unique(VF2$VF_Fence) #

#VF2 <- VF2 %>% filter(VF_Fence == "fence2"  )
VF2 <- VF2 %>% mutate(VF_Fence = "fence2"  )

##########################################################################################################
#############    assign the collar ID to animal ID  VF 2 ########################################################
##########################################################################################################
unique(VF2$deviceName)
## all VF2 device names match neckband number in animal wt dataset = no need to change anything 20cows



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

VF3 <- filter(VF_animal_GPS_data_1_4,
              between(
                local_time,
                ymd_hms('2020-10-30 08:03:00', tz = "Australia/Adelaide"),
                ymd_hms('2020-11-03 11:56:00', tz = "Australia/Adelaide")
              ))

# activation_fence3 2020-10-30 08:03:00
#deactivation_fence4 2020-11-03 11:56:00

min(VF3$local_time)
max(VF3$local_time)
unique(VF3$VF_Fence) #
#VF3 <- VF3 %>% filter(VF_Fence == "fence3"  )
VF3 <- VF3 %>% mutate(VF_Fence = "fence3"  )

#########################################################################################################
#############    assign the collar ID to animal ID  VF 3 ########################################################
##########################################################################################################
unique(VF3$deviceName)
## all VF3 device names match neckband number in animal wt dataset = no need to change anything 20cows
VF3 <-VF3 %>%  mutate(animal_ID = deviceName) 

#check we are assignining all the collar ID to animal names
head(VF3)
with(VF3, table(date, animal_ID))

#the location of the NA
NA_VF3 <- filter(VF3,animal_ID == "NA")
with(NA_VF3, table(date, deviceName))




##########################################################################################################
#############                VF deactivation                 ########################################################
##########################################################################################################



VF_deactivation <- filter(VF_animal_GPS_data_1_4,
              between(
                local_time,
                ymd_hms('2020-11-03 11:56:00', tz = "Australia/Adelaide"),
                ymd_hms('2020-11-09 09:00:00', tz = "Australia/Adelaide")
              ))

#deactivation_fence4 2020-11-03 11:56:00
#end of trial End of trail 2020-11-09 09:00:00 (not sure the exact time)? I will use this time?

min(VF_deactivation$local_time)
max(VF_deactivation$local_time)
unique(VF_deactivation$VF_Fence)

#VF_deactivation <- VF_deactivation %>% filter(VF_Fence == "deactive_VF3"  )
VF_deactivation <- VF_deactivation %>% mutate(VF_Fence = "deactive_VF3"  )

#########################################################################################################
#############    assign the collar ID to animal ID  deactivation ########################################################
##########################################################################################################
unique(VF_deactivation$deviceName)
## all VF_deactivation device names match neckband number in animal wt dataset = no need to change anything 20cows
VF_deactivation <-VF_deactivation %>%  mutate(animal_ID = deviceName) 

#check we are assignining all the collar ID to animal names
head(VF_deactivation)
with(VF_deactivation, table(date, animal_ID))

#the location of the NA
NA_VF_deactivation <- filter(VF_deactivation,animal_ID == "NA")
with(NA_VF_deactivation, table(date, deviceName))


str(VF1)
check <- rbind(VF1, 
               VF2, 
               VF3, 
               VF_deactivation
               )




##########################################################################################################
unique(No_VF_animal_GPS_data_1_4$deviceName)
## all No_VF_animal_GPS_data_1_4 device names match neckband number in animal wt dataset = no need to change anything 20cows
No_VF_animal_GPS_data_1_4 <-No_VF_animal_GPS_data_1_4 %>%  mutate(animal_ID = deviceName) 

##########################################################################################################
## save files ###
saveRDS(VF1,              "W:/VF/2024/animal behaviour data/Long Plain/data_prep/VF1.rds")
saveRDS(VF2,              "W:/VF/2024/animal behaviour data/Long Plain/data_prep/VF2.rds")
saveRDS(VF3,              "W:/VF/2024/animal behaviour data/Long Plain/data_prep/VF3.rds")
saveRDS(VF_deactivation,  "W:/VF/2024/animal behaviour data/Long Plain/data_prep/VF_deactivation.rds")

saveRDS(No_VF_animal_GPS_data_1_4,  "W:/VF/2024/animal behaviour data/Long Plain/data_prep/No_VF_animal_GPS_data_1_4.rds")

VF11_9370004 <- VF1 %>% filter(deviceName == 9370004)

write.csv(VF11_9370004, "W:/VF/2024/animal behaviour data/Long Plain/Raw data/Projected_test/VF1_step1_9370004.csv")
saveRDS(VF11_9370004,  "W:/VF/2024/animal behaviour data/Long Plain/Raw data/Projected_test/VF1_step1_9370004.rds")

