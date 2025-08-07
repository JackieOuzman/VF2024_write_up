
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)

## step for the paper and plots


### The problem is that I have tow fences so I need to think about fence 1 and 2 


data_source <- "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/"



VF1 <-  readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF1_step5c.rds") 
VF2 <-  readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF2_step5c.rds") 
VF3 <-  readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF3_step5c.rds")
VF4 <-  readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF4_step5c.rds")
VF5 <-  readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF5_step5c.rds")
VF6 <-  readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2022/data_prep/VF6_step5c.rds")



VF1 <- VF1 %>% mutate(VF = "VF1")
VF2 <- VF2 %>% mutate(VF = "VF2")
VF3 <- VF3 %>% mutate(VF = "VF3")
VF4 <- VF4 %>% mutate(VF = "VF4")
VF5 <- VF5 %>% mutate(VF = "VF5")
VF6 <- VF6 %>% mutate(VF = "VF6")


## this paddock has multiple VF, only 1 VF has one fence line the others have 2 or more.

class(VF1$dist_to_VF)
class(VF2$dist_to_VF)

dim(VF1$dist_to_VF) #1
dim(VF2$dist_to_VF) #2
dim(VF3$dist_to_VF) #2

dim(VF4) #21
names(VF4)
dim(VF4$dist_to_VF_1) #Null umm not sure this looks like 2
dim(VF5$dist_to_VF)
dim(VF6$dist_to_VF)

# My script is having trouble with this.
# I need to merge this into something like closest or furtherest from the VF.
# but I don't have the energy for this now, also I think that VF3 onwards distance to VF is not working

################################################################################

#VF1$dist_to_VF <- as.numeric(VF1$dist_to_VF)
# VF2 <- VF2 %>% 
#   mutate(
#     dist_to_VF_1 = dist_to_VF[, 1],
#     dist_to_VF_2 = dist_to_VF[, 2]
#   ) 
################################################################################
# so for now I will just remove this clm for my dataset.

VF1 <- VF1 %>%  select(-dist_to_VF)
VF2 <- VF2 %>%  select(-dist_to_VF)
VF3 <- VF3 %>%  select(-dist_to_VF)
VF4 <- VF4 %>%  select(-dist_to_VF)
VF5 <- VF5 %>%  select(-dist_to_VF)
VF6 <- VF6 %>%  select(-dist_to_VF)




Pinnaroo2022_all_data <- rbind(VF1, VF2, VF3, VF4, VF5, VF6)




write.csv(Pinnaroo2022_all_data, "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/Pinnaroo2022_all_data.csv")
saveRDS( Pinnaroo2022_all_data, "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/Pinnaroo2022_all_data.rds")     


