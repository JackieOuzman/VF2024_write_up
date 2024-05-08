
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

# "W:\VF\2024\animal behaviour data\Long Plain\Raw data\csiro_weeklyexports_ending_2020-10-26.csv"
# "W:\VF\2024\animal behaviour data\Long Plain\Raw data\csiro_weeklyexports_ending_2020-11-02.csv"
# "W:\VF\2024\animal behaviour data\Long Plain\Raw data\csiro_weeklyexports_ending_2020-11-09.csv"
# "W:\VF\2024\animal behaviour data\Long Plain\Raw data\csiro_weeklyexports_ending_2020-11-16.csv"

# "W:\VF\2024\animal behaviour data\Long Plain\Raw data\csiro_weeklyexports_novf_ending_2020-10-27.csv"
# "W:\VF\2024\animal behaviour data\Long Plain\Raw data\csiro_weeklyexports_novf_ending_2020-11-03.csv"
# "W:\VF\2024\animal behaviour data\Long Plain\Raw data\csiro_weeklyexports_novf_ending_2020-11-09.csv"
# "W:\VF\2024\animal behaviour data\Long Plain\Raw data\csiro_weeklyexports_novf_ending_2020-11-16.csv"

path_step1 <- "W:/VF/2024/animal behaviour data/Long Plain/data_prep/"
raw_data <-   "W:/VF/2024/animal behaviour data/Long Plain/Raw data/"


No_VF_animal_GPS_data_1 <- read_csv(paste0(raw_data, "csiro_weeklyexports_novf_ending_2020-10-27.csv"))
No_VF_animal_GPS_data_2 <- read_csv(paste0(raw_data, "csiro_weeklyexports_novf_ending_2020-11-03.csv"))
No_VF_animal_GPS_data_3 <- read_csv(paste0(raw_data, "csiro_weeklyexports_novf_ending_2020-11-09.csv"))
No_VF_animal_GPS_data_4 <- read_csv(paste0(raw_data, "csiro_weeklyexports_novf_ending_2020-11-16.csv"))

VF_animal_GPS_data_1 <- read_csv(paste0(raw_data, "csiro_weeklyexports_ending_2020-10-26.csv"))
VF_animal_GPS_data_2 <- read_csv(paste0(raw_data, "csiro_weeklyexports_ending_2020-11-02.csv"))
VF_animal_GPS_data_3 <- read_csv(paste0(raw_data, "csiro_weeklyexports_ending_2020-11-09.csv"))
#? not sure why this is a problem
#VF_animal_GPS_data_3 <- read.csv("W:/VF/2024/animal behaviour data/Long Plain/Raw data/csiro_weeklyexports_ending_2020-11-09.csv")
VF_animal_GPS_data_4 <- read_csv(paste0(raw_data, "csiro_weeklyexports_ending_2020-11-16.csv"))


No_VF_animal_GPS_data_1_4 <- rbind(No_VF_animal_GPS_data_1, No_VF_animal_GPS_data_2,
                                   No_VF_animal_GPS_data_3, No_VF_animal_GPS_data_4)

VF_animal_GPS_data_1_4 <- rbind(VF_animal_GPS_data_1, VF_animal_GPS_data_2,
                                   VF_animal_GPS_data_3, VF_animal_GPS_data_4)

rm(list=ls()[! ls() %in% c("No_VF_animal_GPS_data_1_4","VF_animal_GPS_data_1_4")])


################################################################################
###                    Local local_time          #############
################################################################################
str(VF_animal_GPS_data_1_4)
str(No_VF_animal_GPS_data_1_4)

VF_animal_GPS_data_1_4 <- VF_animal_GPS_data_1_4 %>% 
  mutate(local_time =  ymd_hms(timeOfEvent, tz= "Australia/Adelaide"))

No_VF_animal_GPS_data_1_4 <- No_VF_animal_GPS_data_1_4 %>% 
  mutate(local_time =  ymd_hms(timeOfEvent, tz= "Australia/Adelaide"))


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
    fencesID ==   "11eab" ~ "fence3",
    .default ="deactive_VF3"))
    
    
No_VF_animal_GPS_data_1_4 <- No_VF_animal_GPS_data_1_4 %>% 
  mutate(VF_Fence = "noVF")
     
    

##########################################################################################################
#############                VF 1                 ########################################################
##########################################################################################################

names(VF_animal_GPS_data_1_4)

VF1 <- filter(VF_animal_GPS_data_1_4, 
                            between(local_time, 
                                    ymd_hms('2020-10-21 14:55:00', tz="Australia/Adelaide"),
                                    ymd_hms('2020-10-25 10:50:00', tz="Australia/Adelaide"))) 
VF1 <- VF1 %>% filter(VF_Fence == "fence1"  )

#early time to late time

# activation_fence1 2020-10-21 14:55:00
# activation_fence2 2020-10-25 10:50:00

min(VF1$local_time)
max(VF1$local_time)
unique(VF1$VF_Fence) #

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
NA_VF1_InclusionBord <- filter(VF1_InclusionBord,
                                           animal_ID == "NA")
with(NA_VF1_InclusionBord, table(date, collar_ID))





##########################################################################################################
#############                VF 2                 ########################################################
##########################################################################################################


VF2_InclusionBord <- filter(VF_week1_2_3_InclusionBord, 
                            between(local_time, ymd_hms('2019-05-20 14:50:00', tz="Australia/Adelaide"),
                                    ymd_hms('2019-05-23 08:30:00', tz="Australia/Adelaide")))
min(VF2_InclusionBord$local_time)
max(VF2_InclusionBord$local_time)
unique(VF2_InclusionBord$event) #"InclusionBorder_m is no pulse or audio
##########################################################################################################
#############    assign the collar ID to animal ID  VF 21 ########################################################
##########################################################################################################
VF2_InclusionBord <- mutate(VF2_InclusionBord,
                                        animal_ID = case_when(
                                          collar_ID == "ac138" ~ "Q46",
                                          collar_ID == "ac187" ~ "Q36",
                                          collar_ID == "ac204" ~ "Q108",
                                          collar_ID == "ac207" ~ "Q42",
                                          collar_ID == "ac212" ~ "Q29",
                                          collar_ID == "ac213" &
                                            between(local_time, ymd_hms('2019-05-20 10:15:00', tz="Australia/Adelaide"),
                                                    ymd_hms('2019-05-28 06:44:00', tz="Australia/Adelaide")) ~ "Q47",
                                          collar_ID == "ac320" &
                                            between(local_time, ymd_hms('2019-05-28 11:01:00', tz="Australia/Adelaide"),
                                                    ymd_hms('2019-06-06 17:27:00', tz="Australia/Adelaide")) ~ "Q47" ,
                                          collar_ID == "ac217" ~ "Q27",
                                          collar_ID == "ac218" ~ "Q2",
                                          collar_ID == "ac219" &
                                            between(local_time, ymd_hms('2019-05-20 10:15:00', tz="Australia/Adelaide"),
                                                    ymd_hms('2019-05-25 11:10:00', tz="Australia/Adelaide"))~ "Q10",
                                          collar_ID == "ac220" &
                                            between(local_time, ymd_hms('2019-05-25 11:01:00', tz="Australia/Adelaide"),
                                                    ymd_hms('2019-06-06 17:27:18', tz="Australia/Adelaide"))~ "Q10",
                                          collar_ID == "ac325" ~ "Q9",
                                          collar_ID == "ac328" ~ "Q109",
                                          collar_ID == "ac331" ~ "Q51",
                                          collar_ID == "ad1945" ~ "Q28",
                                          collar_ID == "ad2042" ~ "Q26",
                                          collar_ID == "ad2043" ~ "Q75",
                                          collar_ID == "ad3374" ~ "Q11",
                                          collar_ID == "ad3396"  &
                                            between(local_time, ymd_hms('2019-05-20 10:15:00', tz="Australia/Adelaide"),
                                                    ymd_hms('2019-05-27 16:19:00', tz="Australia/Adelaide"))~ "Q45",
                                          collar_ID == "ac209"  &
                                            between(local_time, ymd_hms('2019-05-28 11:11:00', tz="Australia/Adelaide"),
                                                    ymd_hms('2019-06-06 17:00:00', tz="Australia/Adelaide"))~ "Q45",
                                          collar_ID == "ad3471" ~ "Q15",
                                          collar_ID == "ad3502" ~ "Q8",
                                          collar_ID == "ad3925" ~ "Q110",
                                          TRUE ~ "NA"))

#check we are assignining all the collar ID to animal names
head(VF2_InclusionBord)
with(VF2_InclusionBord, table(date, animal_ID))

#the location of the NA
NA_VF2_InclusionBord <- filter(VF2_InclusionBord,
                               animal_ID == "NA")
with(NA_VF2_InclusionBord, table(date, collar_ID))

##########################################################################################################
#############                VF 3                 ########################################################
##########################################################################################################


VF3_InclusionBord <- filter(VF_week1_2_3_InclusionBord, 
                            between(local_time, ymd_hms('2019-05-23 08:30:00', tz="Australia/Adelaide"),
                                    ymd_hms('2019-05-28 11:00:00', tz="Australia/Adelaide")))
min(VF3_InclusionBord$local_time)
max(VF3_InclusionBord$local_time)
unique(VF3_InclusionBord$event) #"InclusionBorder_m is no pulse or audio

VF3_InclusionBord <- mutate(VF3_InclusionBord,
                                        animal_ID = case_when(
                                          collar_ID == "ac138" ~ "Q46",
                                          collar_ID == "ac187" ~ "Q36",
                                          collar_ID == "ac204" ~ "Q108",
                                          collar_ID == "ac207" ~ "Q42",
                                          collar_ID == "ac212" ~ "Q29",
                                          collar_ID == "ac213" &
                                            between(local_time, ymd_hms('2019-05-20 10:15:00', tz="Australia/Adelaide"),
                                                    ymd_hms('2019-05-28 06:44:00', tz="Australia/Adelaide")) ~ "Q47",
                                          collar_ID == "ac320" &
                                            between(local_time, ymd_hms('2019-05-28 11:01:00', tz="Australia/Adelaide"),
                                                    ymd_hms('2019-06-06 17:27:00', tz="Australia/Adelaide")) ~ "Q47" ,
                                          collar_ID == "ac217" ~ "Q27",
                                          collar_ID == "ac218" ~ "Q2",
                                          collar_ID == "ac219" &
                                            between(local_time, ymd_hms('2019-05-20 10:15:00', tz="Australia/Adelaide"),
                                                    ymd_hms('2019-05-25 11:10:00', tz="Australia/Adelaide"))~ "Q10",
                                          collar_ID == "ac220" &
                                            between(local_time, ymd_hms('2019-05-25 11:01:00', tz="Australia/Adelaide"),
                                                    ymd_hms('2019-06-06 17:27:18', tz="Australia/Adelaide"))~ "Q10",
                                          collar_ID == "ac325" ~ "Q9",
                                          collar_ID == "ac328" ~ "Q109",
                                          collar_ID == "ac331" ~ "Q51",
                                          collar_ID == "ad1945" ~ "Q28",
                                          collar_ID == "ad2042" ~ "Q26",
                                          collar_ID == "ad2043" ~ "Q75",
                                          collar_ID == "ad3374" ~ "Q11",
                                          collar_ID == "ad3396"  &
                                            between(local_time, ymd_hms('2019-05-20 10:15:00', tz="Australia/Adelaide"),
                                                    ymd_hms('2019-05-27 16:19:00', tz="Australia/Adelaide"))~ "Q45",
                                          collar_ID == "ac209"  &
                                            between(local_time, ymd_hms('2019-05-28 11:11:00', tz="Australia/Adelaide"),
                                                    ymd_hms('2019-06-06 17:00:00', tz="Australia/Adelaide"))~ "Q45",
                                          collar_ID == "ad3471" ~ "Q15",
                                          collar_ID == "ad3502" ~ "Q8",
                                          collar_ID == "ad3925" ~ "Q110",
                                          TRUE ~ "NA"))

#check we are assignining all the collar ID to animal names
head(VF3_InclusionBord)
with(VF3_InclusionBord, table(date, animal_ID))

#the location of the NA
NA_VF3_InclusionBord <- filter(VF3_InclusionBord,
                                           animal_ID == "NA")
with(NA_VF3_InclusionBord, table(date, collar_ID))



##########################################################################################################
#############                VF 4                 ########################################################
##########################################################################################################


#Fence 4 called bron next training fence check that the local_time range is working
VF4_InclusionBord <- filter(VF_week1_2_3_InclusionBord, 
                            between(local_time, ymd_hms('2019-05-28 11:15:00', tz="Australia/Adelaide"),
                                    ymd_hms('2019-06-03 09:30:00', tz="Australia/Adelaide")))

min(VF4_InclusionBord$local_time)
max(VF4_InclusionBord$local_time)
unique(VF4_InclusionBord$event)

VF4_InclusionBord <- mutate(VF4_InclusionBord,
                                        animal_ID = case_when(
                                          collar_ID == "ac138" ~ "Q46",
                                          collar_ID == "ac187" ~ "Q36",
                                          collar_ID == "ac204" ~ "Q108",
                                          #collar_ID == "ac220" ~ "Q108", # replaced with 220 on the 28th
                                          collar_ID == "ac207" ~ "Q42",
                                          collar_ID == "ac212" ~ "Q29",
                                          collar_ID == "ac213" &
                                            between(local_time, ymd_hms('2019-05-20 10:15:00', tz="Australia/Adelaide"),
                                                    ymd_hms('2019-05-28 06:44:00', tz="Australia/Adelaide")) ~ "Q47",
                                          collar_ID == "ac320" &
                                            between(local_time, ymd_hms('2019-05-28 11:01:00', tz="Australia/Adelaide"),
                                                    ymd_hms('2019-06-06 17:27:00', tz="Australia/Adelaide")) ~ "Q47" ,
                                          collar_ID == "ac217" ~ "Q27",
                                          collar_ID == "ac218" ~ "Q2",#problem with battery on unit
                                          collar_ID == "ac219" &
                                            between(local_time, ymd_hms('2019-05-20 10:15:00', tz="Australia/Adelaide"),
                                                    ymd_hms('2019-05-25 11:10:00', tz="Australia/Adelaide"))~ "Q10",
                                          collar_ID == "ac220" &
                                            between(local_time, ymd_hms('2019-05-25 11:01:00', tz="Australia/Adelaide"),
                                                    ymd_hms('2019-06-06 17:27:18', tz="Australia/Adelaide"))~ "Q10",
                                          collar_ID == "ac325" ~ "Q9",
                                          collar_ID == "ac328" ~ "Q109",
                                          collar_ID == "ac331" ~ "Q51",
                                          collar_ID == "ad1945" ~ "Q28",
                                          collar_ID == "ad2042" ~ "Q26",
                                          collar_ID == "ad2043" ~ "Q75",
                                          collar_ID == "ad3374" ~ "Q11",
                                          collar_ID == "ad3396"  &
                                            between(local_time, ymd_hms('2019-05-20 10:15:00', tz="Australia/Adelaide"),
                                                    ymd_hms('2019-05-27 16:19:00', tz="Australia/Adelaide"))~ "Q45",
                                          collar_ID == "ac209"  &
                                            between(local_time, ymd_hms('2019-05-28 11:11:00', tz="Australia/Adelaide"),
                                                    ymd_hms('2019-06-06 17:00:00', tz="Australia/Adelaide"))~ "Q45",
                                          collar_ID == "ad3471" ~ "Q15",
                                          collar_ID == "ad3502" ~ "Q8",
                                          collar_ID == "ad3925" ~ "Q110",
                                          TRUE ~ "NA"))

#check we are assignining all the collar ID to animal names
head(VF4_InclusionBord)
with(VF4_InclusionBord, table(date, animal_ID))
with(VF4_InclusionBord, table(date, collar_ID))

check <- rbind(VF1_InclusionBord, VF2_InclusionBord, VF3_InclusionBord, VF4_InclusionBord)


##########################################################################################################
## save files ###
saveRDS(VF1_InclusionBord,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step1/VF1_InclusionBord.rds")
saveRDS(VF2_InclusionBord,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step1/VF2_InclusionBord.rds")
saveRDS(VF3_InclusionBord,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step1/VF3_InclusionBord.rds")
saveRDS(VF4_InclusionBord,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step1/VF4_InclusionBord.rds")



##########################################################################################################
#####               VF 5   week 3  onwards                      ##########################################
##########################################################################################################

VF_week3_4_5_6_7_InclusionBord <- readRDS("W:/VF/Eden_Valley/logged_VF_data/updated collar logs/VF_week3_4_5_6_7.rds")
dim(VF_week3_4_5_6_7_InclusionBord)
unique(VF_week3_4_5_6_7_InclusionBord$event)

################################################################################
###                    Local local_time          #############
################################################################################
str(VF_week3_4_5_6_7_InclusionBord)

VF_week3_4_5_6_7_InclusionBord <- VF_week3_4_5_6_7_InclusionBord %>% 
  mutate(local_time =  ymd_hms(time, tz= "Australia/Adelaide"))


################################################################################
VF5_InclusionBord <- filter(VF_week3_4_5_6_7_InclusionBord, 
                            between(local_time, ymd_hms('2019-06-03 09:31:00', tz="Australia/Adelaide"),
                                    ymd_hms('2019-07-02 06:11:00', tz="Australia/Adelaide"))) #ends at 2019-06-06 23:59:56 ACST


min(VF5_InclusionBord$local_time)
max(VF5_InclusionBord$local_time)

VF5_InclusionBord <- mutate(
  VF5_InclusionBord,
  animal_ID = case_when(
    
    collar_ID == "ac218" ~ "Q2", #missing a heap of these records for multiple days - note in file says its 'low power'
    collar_ID == "ad2658" ~  "Q2",
    
    collar_ID == "ac204" ~ "Q108", #missing a heap of these records for multiple days - note in file says its 'low power'
    collar_ID == "ad2637" ~ "Q108",
    
    collar_ID == "ad2042" ~ "Q26",
    collar_ID == "ad2655" ~ "Q26",
    #collar_ID == "ad2650" ~ "Q26", # the notes say this is cow75
    collar_ID == "ad2643" ~ "Q26", # 
    
    collar_ID == "ac138" ~ "Q46",
    collar_ID == "ac187" ~ "Q36",
    
    collar_ID == "ac207" ~ "Q42",
    collar_ID == "ac212" ~ "Q29",
    collar_ID == "ac213" &
      between(
        time,
        as_datetime('2019-05-20 10:15:00', tz = "GMT"),
        as_datetime('2019-05-28 07:00:00', tz = "GMT")
      ) ~ "Q47",
    #collar_ID == "ac320" &
    #  between(time, as_datetime('2019-05-28 11:01:00', tz="GMT"),
    #           as_datetime('2019-06-06 17:27:00', tz="GMT")) ~ "Q47" ,
    collar_ID == "ac320" ~ "Q47",
    collar_ID == "ac217" ~ "Q27",
    
    collar_ID == "ac219" &
      between(
        time,
        as_datetime('2019-05-20 10:15:00', tz = "GMT"),
        as_datetime('2019-05-25 11:10:00', tz =
                      "GMT")
      ) ~ "Q10",
    collar_ID == "ac220" &
      between(
        time,
        as_datetime('2019-05-25 10:55:00', tz = "GMT"),
        as_datetime('2019-06-06 17:27:18', tz =
                      "GMT")
      ) ~ "Q10",
    collar_ID == "ac325" ~ "Q9",
    collar_ID == "ac328" ~ "Q109",
    collar_ID == "ac331" ~ "Q51",
    collar_ID == "ad1945" ~ "Q28",
   
    collar_ID == "ad2043" ~ "Q75",
    collar_ID == "ad3374" ~ "Q11",
    collar_ID == "ad3396"  &
      between(
        time,
        as_datetime('2019-05-20 10:15:00', tz = "GMT"),
        as_datetime('2019-05-27 16:25:00', tz =
                      "GMT")
      ) ~ "Q45",
    collar_ID == "ac209"  &
      between(
        time,
        as_datetime('2019-05-28 11:11:00', tz = "GMT"),
        as_datetime('2019-06-06 17:00:00', tz =
                      "GMT")
      ) ~ "Q45",
    collar_ID == "ad3471" ~ "Q15",
    collar_ID == "ad3502" ~ "Q8",
    collar_ID == "ad3925" ~ "Q110",
    collar_ID == "ad2644" ~ "Q46",
    collar_ID == "ad2640" ~ "Q36",
    collar_ID == "ad2643" ~ "Q36",
    
    collar_ID == "ad2645" ~ "Q42",
    collar_ID == "ad2649" ~ "Q29",
    collar_ID == "ad2635" ~ "Q47",
    collar_ID == "ad2638" ~ "Q27",
    
    collar_ID == "ad2647" ~ "Q10",
    collar_ID == "ad2646" ~ "Q9",
    collar_ID == "ad2648" ~ "Q109",
    collar_ID == "ad2639" ~ "Q51",
    collar_ID == "ad2656" ~ "Q28",
    
    collar_ID == "ad2653" ~ "Q75",
    collar_ID == "ad2650" ~ "Q75", #who knows the notes are a mess - just follwing the notes
    
    collar_ID == "ad2634" ~ "Q11",
    collar_ID == "ad2654" ~ "Q45",
    collar_ID == "ad2651" ~ "Q15",
    collar_ID == "ad2633" ~ "Q8",
    collar_ID == "ad2657" ~ "Q110",
    TRUE ~ "NA"
  )
)

#check we are assignining all the collar ID to animal names
head(VF5_InclusionBord)
with(VF5_InclusionBord, table(date, animal_ID))

#the location of the NA
NA_VF5_InclusionBord <- filter(VF5_InclusionBord,
                               animal_ID == "NA")
with(NA_VF5_InclusionBord, table(date, collar_ID))



saveRDS(VF5_InclusionBord,  "W:/VF/Optimising_VF/Eden Valley/data_prep/step1/VF5_InclusionBord.rds")
