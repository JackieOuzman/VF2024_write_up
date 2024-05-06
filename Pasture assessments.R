library(tidyverse)
library(readxl)

### LP pasture assessments 
# I have only created a biomass map for post trial, we dont have biomass cut for the pre trial.
# I have converted Damian biomass cuts to kg/ha with the assumption of the size (I need to check).
# The correlation and R2 are poor and this could relate to the lack of vegeation, other signals are no better.
# we should consider if this data is good enough to present.
# the VF 1 shows as having lowest biomass.
# buffer dosent have higher biomass than grazed parts of the paddock.
# we can’t assume different parts of the paddock at a fixed time is due to grazing.


LP_biomass_post_trail <- read.csv("W:/VF/2024/pasture assessments/LP_biomass_grids/LP_Biomass_post_frm_phen.csv")