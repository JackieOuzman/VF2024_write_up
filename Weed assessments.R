library(tidyverse)
library(readxl)
library(car)


weed_pre_post_trial <- read_excel("W:/VF/2024/weed assessments/merged weed data.xlsx",
                                     sheet = "merged weed data")


## tidy up df
#LP 2020
str(weed_pre_post_trial)
unique(weed_pre_post_trial$Site)

LP <- weed_pre_post_trial %>% filter(Site == "Long Plain")
Pin_2021 <- weed_pre_post_trial %>% filter(Site == "Pinnaroo 2021")
Pin_2022 <- weed_pre_post_trial %>% filter(Site == "Pinnaroo 2022")


###############################################################################
### descriptive stats
###############################################################################
DS_LP_timing_VF <- LP %>% 
  group_by(Timing, VF_name )%>%
  #group_by( Timing)%>%
  summarize(
    mean_value = mean(Total_weed_count, na.rm = TRUE),
    median_value = median(Total_weed_count, na.rm = TRUE),
    sd_value = sd(Total_weed_count, na.rm = TRUE),
    count = n() # Count of observations in each group
  )

DS_LP_timing
DS_LP_timing_VF

names(Pin_2021)
str(Pin_2021)

DS_Pin2021_timing <- Pin_2021 %>% 
  group_by( Timing)%>%
  summarize(
    mean_value = mean(Brome plants, na.rm = TRUE),
    median_value = median(Brome plants, na.rm = TRUE),
    sd_value = sd(Brome plants, na.rm = TRUE),
    count = n() # Count of observations in each group
  )

DS_Pin2021_timing_VF <- Pin_2021 %>% 
  group_by(Timing, VF_name )%>%
 
  summarize(
    mean_value = mean(Brome plants, na.rm = TRUE),
    median_value = median(Brome plants, na.rm = TRUE),
    sd_value = sd(Brome plants, na.rm = TRUE),
    count = n() # Count of observations in each group
  )

DS_Pin2021_timing
DS_Pin2021_timing_VF






###############################################################################
## LONG PLAIN
###############################################################################
## TOTAL WEED COUNT
###############################################################################



# Averages total weed counts # before the trial LONG PLAIN
LP_Summary_total_weed_count_pre_trial <- LP %>%
  filter(Timing != "during trial") %>% 
  #filter(Timing != "pre trial") %>% 
  filter(Timing != "post trial") %>%
  group_by(VF_name, Timing)%>%
  summarise(mean_Total_weed_count =  mean(`Total_weed_count`, na.rm = TRUE),
            Total_weed_count_SD =    sd(`Total_weed_count`, na.rm = TRUE),
            Total_weed_count_SE =    Total_weed_count_SD / sqrt(n()))

LP_Summary_total_weed_count_pre_trial

LP_Summary_total_weed_count_post_trial <- LP %>%
  filter(Timing != "during trial") %>% 
  filter(Timing != "pre trial") %>% 
  #filter(Timing != "post trial") %>%
  group_by(VF_name, Timing)%>%
  summarise(mean_Total_weed_count =  mean(`Total_weed_count`, na.rm = TRUE),
            Total_weed_count_SD =    sd(`Total_weed_count`, na.rm = TRUE),
            Total_weed_count_SE =    Total_weed_count_SD / sqrt(n()))

LP_Summary_total_weed_count_post_trial



# SUM total weed counts # before the trial LONG PLAIN
LP_SUM_Summary_total_weed_count_pre_trial <- LP %>%
  filter(Timing != "during trial") %>% 
  #filter(Timing != "pre trial") %>% 
  filter(Timing != "post trial") %>%
  group_by(VF_name, Timing)%>%
  summarise(sum_Total_weed_count =  sum(`Total_weed_count`, na.rm = TRUE),
            Total_weed_count_SD =    sd(`Total_weed_count`, na.rm = TRUE),
            Total_weed_count_SE =    Total_weed_count_SD / sqrt(n()))

LP_SUM_Summary_total_weed_count_pre_trial

LP_SUM_Summary_total_weed_count_post_trial <- LP %>%
  filter(Timing != "during trial") %>% 
  filter(Timing != "pre trial") %>% 
  #filter(Timing != "post trial") %>%
  group_by(VF_name, Timing)%>%
  summarise(sum_Total_weed_count =  sum(`Total_weed_count`, na.rm = TRUE),
            Total_weed_count_SD =    sd(`Total_weed_count`, na.rm = TRUE),
            Total_weed_count_SE =    Total_weed_count_SD / sqrt(n()))

LP_SUM_Summary_total_weed_count_post_trial







# ANOVA

#LP 2020
str(LP)
LP_Pre <- LP %>%  filter(Timing == "pre trial")

ANOVA_LP_total_weed_pre_trail <- aov(Total_weed_count ~ VF_name , data = LP_Pre)

summary(ANOVA_LP_total_weed_pre_trail)
TukeyHSD(ANOVA_LP_total_weed_pre_trail)

## P value = 0.712 The p-value is greater than the significance level 0.1 (or 0.05, 0.01, 0.001)
#we can conclude that the total weed count in the subpaddocks ARE NOT significant different.
#Tukey HSD indicates that all subpaddocks are NOT different from one another

### do we have equal variance?
leveneTest(Total_weed_count ~ VF_name , data = LP_Pre)

#From the output above we can see that the p-value IS GREATER than the significance level of 0.05. 
#This means that there IS NO evidence to suggest that the variance across groups is statistically significantly different. 
#Therefore, we can assume the homogeneity of variances in the different treatment groups.
# equal variance

LP_Post <- LP %>%  filter(Timing == "post trial")

ANOVA_LP_total_weed_post_trail <- aov(Total_weed_count ~ VF_name , data = LP_Post)

summary(ANOVA_LP_total_weed_post_trail)
TukeyHSD(ANOVA_LP_total_weed_post_trail)

## P value = 0.0631 The p-value is greater than the significance level 0.1 (or 0.05, 0.01, 0.001)
#we can conclude that the total weed count in the subpaddocks ARE NOT significant different.
#Tukey HSD indicates that all subpaddocks are NOT different from one another

### do we have equal variance?
leveneTest(Total_weed_count ~ VF_name , data = LP_Post)


#From the output above we can see that the p-value IS GREATER than the significance level of 0.05. 
#This means that there IS NO evidence to suggest that the variance across groups is statistically significantly different. 
#Therefore, we can assume the homogeneity of variances in the different treatment groups.
# equal variance




###############################################################################
## LONG PLAIN
###############################################################################
## Ryegrass plants
###############################################################################

str(LP)
# Averages Ryegrass plants # before the trial LONG PLAIN
LP_Summary_Ryegrass_plant_pre_trial <- LP %>%
  filter(Timing != "during trial") %>% 
  #filter(Timing != "pre trial") %>% 
  filter(Timing != "post trial") %>%
  group_by(VF_name, Timing)%>%
  summarise(mean_Ryegrass_plants_count =  mean(`Ryegrass plants`, na.rm = TRUE),
            Total_Ryegrass_plants_count_SD =    sd(`Ryegrass plants`, na.rm = TRUE),
            Total_Ryegrass_plants_count_SE =    Total_Ryegrass_plants_count_SD / sqrt(n()))

LP_Summary_Ryegrass_plant_pre_trial

LP_Summary_Ryegrass_plant_post_trial <- LP %>%
  filter(Timing != "during trial") %>% 
  filter(Timing != "pre trial") %>% 
  #filter(Timing != "post trial") %>%
  group_by(VF_name, Timing)%>%
  summarise(mean_Ryegrass_plants_count =  mean(`Ryegrass plants`, na.rm = TRUE),
            Total_Ryegrass_plants_count_SD =    sd(`Ryegrass plants`, na.rm = TRUE),
            Total_Ryegrass_plants_count_SE =    Total_Ryegrass_plants_count_SD / sqrt(n()))



ANOVA_LP_RG_count_pre_trail <- aov(`Ryegrass plants` ~ VF_name , data = LP_Pre)

summary(ANOVA_LP_RG_count_pre_trail)
TukeyHSD(ANOVA_LP_RG_count_pre_trail)

## P value = 0.725 The p-value is greater than the significance level 0.1 (or 0.05, 0.01, 0.001)
#we can conclude that the total weed count in the subpaddocks ARE NOT significant different.
#Tukey HSD indicates that all subpaddocks are NOT different from one another

### do we have equal variance?
leveneTest(`Ryegrass plants` ~ VF_name , data = LP_Pre)

#From the output above we can see that the p-value IS GREATER than the significance level of 0.05. 
#This means that there IS NO evidence to suggest that the variance across groups is statistically significantly different. 
#Therefore, we can assume the homogeneity of variances in the different treatment groups.
# equal variance



ANOVA_LP_RG_count_post_trail <- aov(`Ryegrass plants` ~ VF_name , data = LP_Post)

summary(ANOVA_LP_RG_count_post_trail)
TukeyHSD(ANOVA_LP_RG_count_post_trail)

## P value = 0.0226 The p-value is greater than the significance level 0.05 (or 0.05, 0.01, 0.001)
#we can conclude that the total weed count in the subpaddocks ARE NOT significant different.
#Tukey HSD indicates that all subpaddocks are NOT different from one another

### do we have equal variance?
leveneTest(`Ryegrass plants` ~ VF_name , data = LP_Post)


#From the output above we can see that the p-value IS GREATER than the significance level of 0.05. 
#This means that there IS NO evidence to suggest that the variance across groups is statistically significantly different. 
#Therefore, we can assume the homogeneity of variances in the different treatment groups.
# equal variance



###############################################################################
## LONG PLAIN
###############################################################################
## Ryegrass_Tillers
###############################################################################

str(LP)
LP$Ryegrass_Tillers <- as.double(LP$Ryegrass_Tillers)


LP_Summary_Ryegrass_Till_post_trial <- LP %>%
  filter(Timing != "during trial") %>% 
  filter(Timing != "pre trial") %>% 
  #filter(Timing != "post trial") %>%
  group_by(VF_name, Timing)%>%
  summarise(mean_Ryegrass_Till_count =  mean(`Ryegrass_Tillers`, na.rm = TRUE),
            Total_Ryegrass_Till_count_SD =    sd(`Ryegrass_Tillers`, na.rm = TRUE),
            Total_Ryegrass_plants_count_SE =    Total_Ryegrass_Till_count_SD / sqrt(n()))

LP_Summary_Ryegrass_Till_post_trial

ANOVA_LP_RG_Till_post_trail <- aov(`Ryegrass_Tillers` ~ VF_name , data = LP_Post)

summary(ANOVA_LP_RG_Till_post_trail)
TukeyHSD(ANOVA_LP_RG_Till_post_trail)

## P value = 0.048 The p-value is less than the significance level 0.05 (or 0.05, 0.01, 0.001)
#we can conclude that the total weed count in the subpaddocks ARE NOT significant different.
#Tukey HSD indicates that all subpaddocks are NOT different from one another

### do we have equal variance?
leveneTest(`Ryegrass_Tillers` ~ VF_name , data = LP_Post)


#From the output above we can see that the p-value IS GREATER than the significance level of 0.05. 
#This means that there IS NO evidence to suggest that the variance across groups is statistically significantly different. 
#Therefore, we can assume the homogeneity of variances in the different treatment groups.
# equal variance












###############################################################################
## Pinnaroo 2021
###############################################################################
## Brome plants
###############################################################################
str(Pin_2021)


LP_Summary_Brome_plant_post_trial <- Pin_2021 %>%
  filter(Timing != "during trial") %>% 
  filter(Timing != "pre trial") %>% 
  #filter(Timing != "post trial") %>%
  group_by(VF_name, Timing)%>%
  summarise(mean_Brome_plants_count =  mean(`Brome plants`, na.rm = TRUE),
            Total_Brome_plants_count_SD =    sd(`Brome plants`, na.rm = TRUE),
            Total_Brome_plants_count_SE =    Total_Brome_plants_count_SD / sqrt(n()))

LP_Summary_Brome_plant_post_trial

ANOVA_Pin2021_Brome_pre_trail <- aov(`Brome plants` ~ VF_name , data = Pin_2021)

summary(ANOVA_Pin2021_Brome_pre_trail)
TukeyHSD(ANOVA_Pin2021_Brome_pre_trail)

## P value = 0.302 The p-value is greater than the significance level 0.1 (or 0.05, 0.01, 0.001)
#we can conclude that the total broome in the subpaddocks ARE NOT significant different.
#Tukey HSD indicates that all subpaddocks are NOT different from one another

### do we have equal variance?
leveneTest(`Brome plants` ~ VF_name , data = Pin_2021)

#From the output above we can see that the p-value IS GREATER than the significance level of 0.05. 
#This means that there IS NO evidence to suggest that the variance across groups is statistically significantly different. 
#Therefore, we can assume the homogeneity of variances in the different treatment groups.
# equal variance



###############################################################################
## Pinnaroo 2022
###############################################################################
## Total weeds 
###############################################################################
str(Pin_2022)



# Averages total weed counts # before the trial Pin 2022
Pin2022_Summary_total_weed_count_pre_trial <- Pin_2022 %>%
  filter(Timing != "during trial") %>% 
  #filter(Timing != "pre trial") %>% 
  filter(Timing != "post trial") %>%
  group_by(VF_name, Timing)%>%
  summarise(mean_Total_weed_count =  mean(`Total_weed_count`, na.rm = TRUE),
            Total_weed_count_SD =    sd(`Total_weed_count`, na.rm = TRUE),
            Total_weed_count_SE =    Total_weed_count_SD / sqrt(n()))

Pin2022_Summary_total_weed_count_pre_trial

Pin2022_Summary_total_weed_count_post_trial <- Pin_2022 %>%
  filter(Timing != "during trial") %>% 
  filter(Timing != "pre trial") %>% 
  #filter(Timing != "post trial") %>%
  group_by(VF_name, Timing)%>%
  summarise(mean_Total_weed_count =  mean(`Total_weed_count`, na.rm = TRUE),
            Total_weed_count_SD =    sd(`Total_weed_count`, na.rm = TRUE),
            Total_weed_count_SE =    Total_weed_count_SD / sqrt(n()))

Pin2022_Summary_total_weed_count_post_trial

Pin2022_pre <- Pin_2022 %>% filter(Timing== "pre trial")
Pin2022_post <- Pin_2022 %>% filter(Timing== "post trial")

ANOVA_Pin2021_Total_pre_trail <- aov(`Total_weed_count` ~ VF_name , data = Pin2022_pre)

summary(ANOVA_Pin2021_Total_pre_trail)
TukeyHSD(ANOVA_Pin2021_Total_pre_trail)

## P value = 0.414 The p-value is greater than the significance level 0.1 (or 0.05, 0.01, 0.001)
#we can conclude that the total broome in the subpaddocks ARE NOT significant different.
#Tukey HSD indicates that all subpaddocks are NOT different from one another

### do we have equal variance?
leveneTest(`Total_weed_count` ~ VF_name , data = Pin2022_pre)

#From the output above we can see that the p-value IS GREATER than the significance level of 0.05. 
#This means that there IS NO evidence to suggest that the variance across groups is statistically significantly different. 
#Therefore, we can assume the homogeneity of variances in the different treatment groups.
# equal variance


ANOVA_Pin2021_Total_post_trail <- aov(`Total_weed_count` ~ VF_name , data = Pin2022_post)

summary(ANOVA_Pin2021_Total_post_trail)
TukeyHSD(ANOVA_Pin2021_Total_post_trail)

## P value = 0.0371 The p-value is less than the significance level 0.05 (or 0.05, 0.01, 0.001)
#we can conclude that the total broome in the subpaddocks ARE NOT significant different.
#Tukey HSD indicates that all subpaddocks are NOT different from one another

### do we have equal variance?
leveneTest(`Total_weed_count` ~ VF_name , data = Pin2022_post)

#From the output above we can see that the p-value IS GREATER than the significance level of 0.05. 
#This means that there IS NO evidence to suggest that the variance across groups is statistically significantly different. 
#Therefore, we can assume the homogeneity of variances in the different treatment groups.
# equal variance


###############################################################################
## Pinnaroo 2022
###############################################################################
## Ryegrass plants
###############################################################################
str(Pin_2022)



# Averages total weed counts # before the trial Pin 2022
Pin2022_Summary_RG_count_pre_trial <- Pin_2022 %>%
  filter(Timing != "during trial") %>% 
  #filter(Timing != "pre trial") %>% 
  filter(Timing != "post trial") %>%
  group_by(VF_name, Timing)%>%
  summarise(mean_RG_count =  mean(`Ryegrass plants`, na.rm = TRUE),
            Total_RG_count_SD =    sd(`Ryegrass plants`, na.rm = TRUE),
            Total_RG_count_SE =    Total_RG_count_SD / sqrt(n()))

Pin2022_Summary_RG_count_pre_trial

Pin2022_Summary_RG_count_post_trial <- Pin_2022 %>%
  filter(Timing != "during trial") %>% 
  filter(Timing != "pre trial") %>% 
  #filter(Timing != "post trial") %>%
  group_by(VF_name, Timing)%>%
  summarise(mean_RG_count =  mean(`Ryegrass plants`, na.rm = TRUE),
            Total_RG_count_SD =    sd(`Ryegrass plants`, na.rm = TRUE),
            Total_RG_count_SE =    Total_RG_count_SD / sqrt(n()))

Pin2022_Summary_RG_count_post_trial



ANOVA_Pin2021_RG_pre_trail <- aov(`Ryegrass plants` ~ VF_name , data = Pin2022_pre)

summary(ANOVA_Pin2021_RG_pre_trail)
TukeyHSD(ANOVA_Pin2021_RG_pre_trail)

## P value = 0.0.26 The p-value is greater than the significance level 0.1 (or 0.05, 0.01, 0.001)
#we can conclude that the total RG in the subpaddocks ARE NOT significant different.
#Tukey HSD indicates that all subpaddocks are NOT different from one another

### do we have equal variance?
leveneTest(`Ryegrass plants` ~ VF_name , data = Pin2022_pre)

#From the output above we can see that the p-value IS GREATER than the significance level of 0.05. 
#This means that there IS NO evidence to suggest that the variance across groups is statistically significantly different. 
#Therefore, we can assume the homogeneity of variances in the different treatment groups.
# equal variance


ANOVA_Pin2021_RG_post_trail <- aov(`Ryegrass plants` ~ VF_name , data = Pin2022_post)

summary(ANOVA_Pin2021_RG_post_trail)
TukeyHSD(ANOVA_Pin2021_RG_post_trail)

## P value = 0.0371 The p-value is less than the significance level 0.05 (or 0.05, 0.01, 0.001)
#we can conclude that the total broome in the subpaddocks ARE NOT significant different.
#Tukey HSD indicates that all subpaddocks are NOT different from one another

### do we have equal variance?
leveneTest(`Total_weed_count` ~ VF_name , data = Pin2022_post)

#From the output above we can see that the p-value IS GREATER than the significance level of 0.05. 
#This means that there IS NO evidence to suggest that the variance across groups is statistically significantly different. 
#Therefore, we can assume the homogeneity of variances in the different treatment groups.
# equal variance