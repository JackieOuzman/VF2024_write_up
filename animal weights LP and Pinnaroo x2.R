library(tidyverse)
library(readxl)

### animals weights for 3 trials analysis and any plots


aniamal_wts <- read_excel("W:/VF/2024/animal weights/All Pinnaroo trials and LP animal wts.xlsx",
                          sheet = "merged")
names(aniamal_wts)

aniamal_wts <- aniamal_wts %>%
  select(
    "ID",
    "Site",
    "Mob Name",
    "Animal Name",
    "Weight at start no collar",
    "Weight at end with collar" ,
    "collar wt kg"   ,
    "Weight at end with no collar",
    "Weight gain"  ,
    "Daily weight gain"     ,
    "% gain"
  )

#### I think this one should be removed from 2022 dataset M52

aniamal_wts <- aniamal_wts %>%
  filter(ID != 98)



##### count of animals per group
aniamal_wts_summary_count <- aniamal_wts %>% 
  group_by(Site, 
           `Mob Name`
           ) %>% 
  summarise(count = n())
aniamal_wts_summary_count



##### aver weight gain of animals per all/ trial / group
str(aniamal_wts)
#all
aniamal_wts_summary_wt_gain_all <- aniamal_wts %>%
  group_by(
    #Site,
    #`Mob Name` 
    )%>%
  summarise(mean_wt_gain =  mean(`Weight gain`, na.rm = TRUE),
            wt_gain_SD =    sd(`Weight gain`, na.rm = TRUE),
            wt_gain_SE =    wt_gain_SD / sqrt(n()),
            
            mean_Daily_wt_gain =  mean(`Daily weight gain`, na.rm = TRUE),
            Daily_wt_gain_SD =   sd(`Daily weight gain`, na.rm = TRUE),
            Daily_wt_gain_SE =    Daily_wt_gain_SD / sqrt(n())
  )
aniamal_wts_summary_wt_gain_all


#Trial
aniamal_wts_summary_wt_gain_trial <- aniamal_wts %>%
  group_by(
    Site,
    #`Mob Name` 
  )%>%
  summarise(mean_wt_gain =  mean(`Weight gain`, na.rm = TRUE),
            wt_gain_SD =    sd(`Weight gain`, na.rm = TRUE),
            wt_gain_SE =    wt_gain_SD / sqrt(n()),
            
            mean_Daily_wt_gain =  mean(`Daily weight gain`, na.rm = TRUE),
            Daily_wt_gain_SD =   sd(`Daily weight gain`, na.rm = TRUE),
            Daily_wt_gain_SE =    Daily_wt_gain_SD / sqrt(n())
  )
aniamal_wts_summary_wt_gain_trial


#Trial and Mob
aniamal_wts_summary_wt_gain_trial_mob <- aniamal_wts %>%
  group_by(
    Site,
    `Mob Name` 
  )%>%
  summarise(mean_wt_gain =  mean(`Weight gain`, na.rm = TRUE),
            wt_gain_SD =    sd(`Weight gain`, na.rm = TRUE),
            wt_gain_SE =    wt_gain_SD / sqrt(n()),
            
            mean_Daily_wt_gain =  mean(`Daily weight gain`, na.rm = TRUE),
            Daily_wt_gain_SD =   sd(`Daily weight gain`, na.rm = TRUE),
            Daily_wt_gain_SE =    Daily_wt_gain_SD / sqrt(n())
  )
aniamal_wts_summary_wt_gain_trial_mob

view(aniamal_wts_summary_wt_gain_trial_mob)

#Trial
aniamal_wts_summary_wt_gain_trial <- aniamal_wts %>%
  group_by(
    Site,
    #`Mob Name` 
  )%>%
  summarise(mean_wt_gain =  mean(`Weight gain`, na.rm = TRUE),
            mean_Daily_wt_gain =  mean(`Daily weight gain`, na.rm = TRUE))
aniamal_wts_summary_wt_gain_trial


# Mob
aniamal_wts_summary_wt_gain_mob <- aniamal_wts %>%
  group_by(
    #Site,
    `Mob Name` 
  )%>%
  summarise(mean_wt_gain =  mean(`Weight gain`, na.rm = TRUE),
            wt_gain_SD =    sd(`Weight gain`, na.rm = TRUE),
            wt_gain_SE =    wt_gain_SD / sqrt(n()),
            
            mean_Daily_wt_gain =  mean(`Daily weight gain`, na.rm = TRUE),
            Daily_wt_gain_SD =   sd(`Daily weight gain`, na.rm = TRUE),
            Daily_wt_gain_SE =    Daily_wt_gain_SD / sqrt(n())
            )
aniamal_wts_summary_wt_gain_mob


###############################################################################

### ANOVA ####################################################################


# animal_wt 


str(aniamal_wts)
unique(aniamal_wts$Site)

aniamal_wts_LP <- aniamal_wts %>% filter(Site == "Long Plain" )

animal_wts_aov_LP <- aov(`Daily weight gain` ~ `Mob Name` , data = aniamal_wts_LP)

summary(animal_wts_aov_LP)

## P value = 0.382 The p-value is greater than the significance level 0.1 (or 0.05, 0.01, 0.001)
#we can conclude that there are NO significant differences between the groups 



aniamal_wts_2022Pin <- aniamal_wts %>% filter(Site == "Pinnaroo 2022" )

animal_wts_aov_2022Pin <- aov(`Daily weight gain` ~ `Mob Name` , data = aniamal_wts_2022Pin)

summary(animal_wts_aov_2022Pin)

## P value = 0.78 The p-value is greater than the significance level 0.1 (or 0.05, 0.01, 0.001)
#we can conclude that there are NO significant differences between the groups 


