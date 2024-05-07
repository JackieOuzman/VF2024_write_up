library(tidyverse)
library(readxl)
library(car)

### LP pasture assessments 
# I have only created a biomass map for post trial, we dont have biomass cut for the pre trial.
# I have converted Damian biomass cuts to kg/ha with the assumption of the size (I need to check).
# The correlation and R2 are poor and this could relate to the lack of vegeation, other signals are no better.
# we should consider if this data is good enough to present.
# the VF 1 shows as having lowest biomass.
# buffer dosent have higher biomass than grazed parts of the paddock.
# we can’t assume different parts of the paddock at a fixed time is due to grazing.


LP_biomass_post_trail <- read.csv("W:/VF/2024/pasture assessments/LP_biomass_grids/LP_Biomass_post_frm_phen.csv")

Pinnaroo_2021_biomass_post_trail <- read.csv("W:/VF/2024/pasture assessments/Pinnaroo_2021/Pin2021_Bimass_post_from_dist__join.csv")

Pinnaroo_2022_biomass_post_trail <- read.csv("W:/VF/2024/pasture assessments/Pinnaroo_2022/Pin2022_Biomass_post_frm_LAI_join.csv")

################################################################################
## tidy up df
#LP 2020
str(LP_biomass_post_trail)
LP_biomass_post_trail <- LP_biomass_post_trail %>% select(FID, VF_name,  grid_code) %>% 
  rename(biomass = grid_code)
str(LP_biomass_post_trail)
unique(LP_biomass_post_trail$VF_name)

#2021 Pinnaroo
str(Pinnaroo_2021_biomass_post_trail)
Pinnaroo_2021_biomass_post_trail <- Pinnaroo_2021_biomass_post_trail %>% select(FID, AREA_GEO,  grid_code) %>% 
  rename(biomass = grid_code)

unique(Pinnaroo_2021_biomass_post_trail$AREA_GEO)

Pinnaroo_2021_biomass_post_trail <-
  Pinnaroo_2021_biomass_post_trail %>%
  mutate(VF_name =  case_when(AREA_GEO > 0 ~ "VF",
                           AREA_GEO == 0.0 ~ "Buffer"))
str(Pinnaroo_2021_biomass_post_trail)

#2022 Pinnaroo
str(Pinnaroo_2022_biomass_post_trail)
Pinnaroo_2022_biomass_post_trail <- Pinnaroo_2022_biomass_post_trail %>% select(FID, Name,  grid_code) %>% 
  rename(biomass = grid_code,
         VF_name = Name)
str(Pinnaroo_2022_biomass_post_trail)

#################################################################################

# ANOVA

#LP 2020
str(LP_biomass_post_trail)

ANOVA_LP_biomass_post_trail <- aov(biomass ~ VF_name , data = LP_biomass_post_trail)

summary(ANOVA_LP_biomass_post_trail)
TukeyHSD(ANOVA_LP_biomass_post_trail)

## P value = 2e-16 The p-value is greater than the significance level 0.1 (or 0.05, 0.01, 0.001)
#we can conclude that the biomass in the subpaddocks ARE significant different.
#Tukey HSD indicates that all subpaddocks are different from one another
#But I think this is because we have soooo many obervation point!

### do we have equal variance?
leveneTest(biomass ~ VF_name , data = LP_biomass_post_trail)

#From the output above we can see that the p-value IS less than the significance level of 0.05. 
#This means that there IS evidence to suggest that the variance across groups is statistically significantly different. 
#Therefore, we can't assume the homogeneity of variances in the different treatment groups.
# not equal variance

# Non-parametric alternative to one-way ANOVA test

kruskal.test(biomass ~ VF_name , data = LP_biomass_post_trail)


# Averages
Summary_LP_biomass_post_trail <- LP_biomass_post_trail %>%
  group_by(VF_name)%>%
  summarise(mean_Biomass =  mean(biomass, na.rm = TRUE),
            Biomass_SD =    sd(biomass, na.rm = TRUE),
            Biomass_SE =    Biomass_SD / sqrt(n()))

Summary_LP_biomass_post_trail




#2021 Pinnaroo

str(Pinnaroo_2021_biomass_post_trail)

ANOVA_Pinnaroo_2021_biomass_post_trail <- aov(biomass ~ VF_name , data = Pinnaroo_2021_biomass_post_trail)

summary(ANOVA_Pinnaroo_2021_biomass_post_trail)
TukeyHSD(ANOVA_Pinnaroo_2021_biomass_post_trail)

## P value = 2e-16 The p-value is greater than the significance level 0.1 (or 0.05, 0.01, 0.001)
#we can conclude that the biomass in the subpaddocks ARE significant different.
#Tukey HSD indicates that  subpaddocks are different from one another
#But I think this is because we have soooo many obervation point!

### do we have equal variance?
leveneTest(biomass ~ VF_name , data = Pinnaroo_2021_biomass_post_trail)

#From the output above we can see that the p-value IS less than the significance level of 0.05. 
#This means that there IS evidence to suggest that the variance across groups is statistically significantly different. 
#Therefore, we can't assume the homogeneity of variances in the different treatment groups.
# not equal variance

# Non-parametric alternative to one-way ANOVA test

kruskal.test(biomass ~ VF_name , data = Pinnaroo_2021_biomass_post_trail)

# Averages
Summary_Pinnaroo_2021_biomass_post_trail <- Pinnaroo_2021_biomass_post_trail %>%
  group_by(VF_name)%>%
  summarise(mean_Biomass =  mean(biomass, na.rm = TRUE),
            Biomass_SD =    sd(biomass, na.rm = TRUE),
            Biomass_SE =    Biomass_SD / sqrt(n()))

Summary_Pinnaroo_2021_biomass_post_trail



#2022 Pinnaroo

str(Pinnaroo_2022_biomass_post_trail)

ANOVA_Pinnaroo_2022_biomass_post_trail <- aov(biomass ~ VF_name , data = Pinnaroo_2022_biomass_post_trail)

summary(ANOVA_Pinnaroo_2022_biomass_post_trail)
TukeyHSD(ANOVA_Pinnaroo_2022_biomass_post_trail)

## P value = 2e-16 The p-value is greater than the significance level 0.1 (or 0.05, 0.01, 0.001)
#we can conclude that the biomass in the subpaddocks ARE significant different.
#Tukey HSD indicates that  subpaddocks are different from one another
#But I think this is because we have soooo many obervation point!

### do we have equal variance?
leveneTest(biomass ~ VF_name , data = Pinnaroo_2022_biomass_post_trail)

#From the output above we can see that the p-value IS less than the significance level of 0.05. 
#This means that there IS evidence to suggest that the variance across groups is statistically significantly different. 
#Therefore, we can't assume the homogeneity of variances in the different treatment groups.
# not equal variance

# Non-parametric alternative to one-way ANOVA test

kruskal.test(biomass ~ VF_name , data = Pinnaroo_2022_biomass_post_trail)

# Averages
Summary_Pinnaroo_2022_biomass_post_trail <- Pinnaroo_2022_biomass_post_trail %>%
  group_by(VF_name)%>%
  summarise(mean_Biomass =  mean(biomass, na.rm = TRUE),
            Biomass_SD =    sd(biomass, na.rm = TRUE),
            Biomass_SE =    Biomass_SD / sqrt(n()))

Summary_Pinnaroo_2022_biomass_post_trail

