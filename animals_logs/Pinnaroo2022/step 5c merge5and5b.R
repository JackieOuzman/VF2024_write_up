library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)

################################################################################
### Merge step 5 and step 5b For VF1
################################################################################

                 #"W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/

step5 <-  readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF1_step5.rds")
step5b <- readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF1_step5b.rds")

str(step5)
str(step5b)

step5b <- step5b %>% 
  dplyr::select(Time_animal,cumulativeAudioCount:Shock_values)


all_step5 <- left_join(step5, step5b)
str(all_step5)
saveRDS(all_step5,  "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF1_step5c.rds")

# write.csv(step5, "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/checking_step3_VF1/WED_VF1_step5a.csv")
# write.csv(step5b, "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/checking_step3_VF1/WED_VF1_step5b.csv")
# write.csv(all_step5, "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/checking_step3_VF1/WED_VF1_step5c.csv")


rm(step5,step5b, all_step5)

################################################################################
### Merge step 5 and step 5b For VF2
################################################################################

step5 <-  readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF2_step5.rds")
step5b <- readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF2_step5b.rds")

step5b <- step5b %>% 
  dplyr::select(Time_animal,cumulativeAudioCount:Shock_values)


all_step5 <- left_join(step5, step5b)
#unique(all_step5$date)
saveRDS(all_step5,  "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF2_step5c.rds")

rm(step5,step5b, all_step5)

################################################################################
### Merge step 5 and step 5b For VF3
################################################################################

step5 <-  readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF3_step5.rds")
step5b <- readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF3_step5b.rds")

step5b <- step5b %>% 
  dplyr::select(Time_animal,cumulativeAudioCount:Shock_values)


all_step5 <- left_join(step5, step5b)
saveRDS(all_step5,  "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF3_step5c.rds")

rm(step5,step5b, all_step5)

################################################################################
### Merge step 5 and step 5b For VF4
################################################################################

step5 <-  readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF4_step5.rds")
step5b <- readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF4_step5b.rds")

step5b <- step5b %>% 
  dplyr::select(Time_animal,cumulativeAudioCount:Shock_values)


all_step5 <- left_join(step5, step5b)
saveRDS(all_step5,  "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF4_step5c.rds")

rm(step5,step5b, all_step5)

#
################################################################################
### Merge step 5 and step 5b For VF5
################################################################################

step5 <-  readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF5_step5.rds")
step5b <- readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF5_step5b.rds")

step5b <- step5b %>% 
  dplyr::select(Time_animal,cumulativeAudioCount:Shock_values)


all_step5 <- left_join(step5, step5b)
saveRDS(all_step5,  "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF5_step5c.rds")

rm(step5,step5b, all_step5)

#

################################################################################
### Merge step 5 and step 5b For VF6
################################################################################

step5 <-  readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF6_step5.rds")
step5b <- readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF6_step5b.rds")

step5b <- step5b %>% 
  dplyr::select(Time_animal,cumulativeAudioCount:Shock_values)


all_step5 <- left_join(step5, step5b)
saveRDS(all_step5,  "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF6_step5c.rds")

rm(step5,step5b, all_step5)
