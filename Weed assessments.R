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



# Averages total weed counts # before the trial all trails
Summary_total_weed_count_pre_trial <- weed_pre_post_trial %>%
  filter(Timing != "during trial") %>% 
  #filter(Timing != "pre trial") %>% 
  filter(Timing != "post trial") %>%
  group_by(VF_name, Timing)%>%
  summarise(mean_Total_weed_count =  mean(`Total_weed_count`, na.rm = TRUE),
            Total_weed_count_SD =    sd(`Total_weed_count`, na.rm = TRUE),
            Total_weed_count_SE =    Total_weed_count_SD / sqrt(n()))

Summary_total_weed_count_pre_trial

Summary_total_weed_count_post_trial <- weed_pre_post_trial %>%
  filter(Timing != "during trial") %>% 
  filter(Timing != "pre trial") %>% 
  #filter(Timing != "post trial") %>%
  group_by(VF_name, Timing)%>%
  summarise(mean_Total_weed_count =  mean(`Total_weed_count`, na.rm = TRUE),
            Total_weed_count_SD =    sd(`Total_weed_count`, na.rm = TRUE),
            Total_weed_count_SE =    Total_weed_count_SD / sqrt(n()))

Summary_total_weed_count_post_trial


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
