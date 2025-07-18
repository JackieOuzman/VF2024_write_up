## step for the paper and plots


data_source <- "W:/VF/2024/animal behaviour data/Pinnaroo2021/data_prep/"



VF1 <-  readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2021/data_prep/VF1_step5c.rds") 
VF2 <-  readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2021/data_prep/VF2_step5c.rds") 
VF3 <-  readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2021/data_prep/VF3_step5c.rds")
VF4 <-  readRDS("W:/VF/2024/animal behaviour data/Pinnaroo2021/data_prep/VF4_step5c.rds")




VF1 <- VF1 %>% mutate(VF = "VF1")
VF2 <- VF2 %>% mutate(VF = "VF2")
VF3 <- VF3 %>% mutate(VF = "VF3")
VF4 <- VF4 %>% mutate(VF = "VF4")



Pinnaroo2021_all_data <- rbind(VF1, VF2, VF3, VF4)


write.csv(Pinnaroo2021_all_data, "W:/VF/2024/animal behaviour data/Pinnaroo2021/data_for_plots/Pinnaroo2021_all_data.csv")
saveRDS( Pinnaroo2021_all_data, "W:/VF/2024/animal behaviour data/Pinnaroo2021/data_for_plots/Pinnaroo2021_all_data.rds")     


unique(VF1$date)
unique(VF2$date)
