## pre analysis to get the projection correct - same method for all data boundaries and pt data
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)





#raw_data <-   "W:/VF/2024/animal behaviour data/Long Plain/Raw data/Projected/"
raw_data <-   "W:/VF/LongPlain/Collar_data/Raw_data_projected/"

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

write.csv(No_VF_animal_GPS_data_1_4, 
          "W:/VF/LongPlain/Collar_data/Raw_data_projected/No_VF_animal_GPS_data_1_4.csv",
          row.names = FALSE)

write.csv(VF_animal_GPS_data_1_4, 
          "W:/VF/LongPlain/Collar_data/Raw_data_projected/VF_animal_GPS_data_1_4.csv",
          row.names = FALSE)
