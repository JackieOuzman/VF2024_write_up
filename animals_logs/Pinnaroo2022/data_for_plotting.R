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

class(VF1$dist_to_VF)
class(VF2$dist_to_VF)

dim(VF1$dist_to_VF)
dim(VF2$dist_to_VF)

#VF1 <- VF1 %>% mutate(dist_to_VF = as.numeric(dist_to_VF))
#VF2 <- VF2 %>% mutate(dist_to_VF = as.numeric(dist_to_VF))


VF2 <- VF2 %>% 
  mutate(
    dist_to_VF_1 = dist_to_VF[, 1],
    dist_to_VF_2 = dist_to_VF[, 2]
  ) 
#%>%
#  select(-dist_to_VF)  # remove original matrix column


VF1$dist_to_VF <- as.numeric(VF1$dist_to_VF)

temp <- VF2 %>% select(dist_to_VF_1, dist_to_VF_2,dist_to_VF)


# VF1 <- VF1 %>% mutate(dist_to_VF = map_dbl(dist_to_VF, ~ as.numeric(.x[1])))
# VF2 <- VF2 %>% mutate(dist_to_VF = map_dbl(dist_to_VF, ~ as.numeric(.x[2])))
# VF3 <- VF3 %>% mutate(dist_to_VF = map_dbl(dist_to_VF, ~ as.numeric(.x[2])))

VF1 <- VF1 %>% mutate(dist_to_VF = unlist(dist_to_VF))
VF2 <- VF2 %>% mutate(dist_to_VF = unlist(dist_to_VF))


VF1 <- VF1 %>% mutate(dist_to_VF = as.vector(dist_to_VF))
VF2 <- VF2 %>% mutate(dist_to_VF = as.vector(dist_to_VF))

VF2 <- VF2 %>% mutate(dist_to_VF = as.numeric(VF2$dist_to_VF))
VF3 <- VF3 %>%  unnest(dist_to_VF)
VF4 <- VF4 %>%  unnest(dist_to_VF)
VF5 <- VF5 %>%  unnest(dist_to_VF)
VF6 <- VF6 %>%  unnest(dist_to_VF)

str(VF1)

Pinnaroo2022_all_data <- rbind(VF1, VF2, VF3, VF4, VF5, VF6)

Pinnaroo2022_all_data <- rbind(VF1, VF2)


write.csv(Pinnaroo2022_all_data, "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/Pinnaroo2022_all_data.csv")
saveRDS( Pinnaroo2022_all_data, "W:/VF/2024/animal behaviour data/Pinnaroo2022/data_for_plots/Pinnaroo2022_all_data.rds")     


unique(VF1$date)
unique(VF2$date)
