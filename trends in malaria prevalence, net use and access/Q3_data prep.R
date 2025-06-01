### Urban Nets Q3- Preparing data for analysis ###
# Data cleaning, including identifying survey months and
# subsetting the largest urban center in each country.
# Author: Colleen Leonard
# Date last edited: 9/26/2023

library(readxl)
library(plyr)
library(tidyverse)
library(ggplot2)
library(janitor)
library(haven)
library(survey)
library(skimr)
library(labelled)

### Functions
#Identify the interview months
interview_months <- function(survey, num){ 
  survey %>% 
  filter(hv023== num) %>% 
  tabyl(hv006, hv007) }

# Identify clusters in Largest urban center per country/ survey

# Angola- subsetting to just clusters in Luanda
print_labels(AO_2015_dhs$hv023) #Stratification
print_labels(AO_2011_mis$hv023) 
## hv023= 7 (luanda-urban)

#Interview months- Angola, 2015= Oct- Dec, 2016= Jan- Apr
AO_2015_dhs %>% 
  filter(hv023== 7) %>% 
  tabyl(hv006, hv007)
AO_2011_mis %>% 
  filter(hv023== 7) %>% 
  tabyl(hv006, hv007)

# Benin-
print_labels(BJ_2017_dhs$hv023) #Cotonou overlaps with the Littoral department completely
## hv023= 15 (Littoral, urban)
#Interview months
BJ_2017_dhs %>% 
  filter(hv023== 15) %>% 
  tabyl(hv006, hv007)
BJ_2012_dhs %>% 
  filter(hv023== 15) %>% 
  tabyl(hv006, hv007)

# Burkina Faso
print_labels(BF_2021_dhs$hv023)
## hv023= 5 (Centre urbain)

#for 2010- combine hv024 and hv025
BF_201_dhs %>% tabyl(hv023)
BF_2010_dhs %>% tabyl(hv024, hv025) #matches number in hv023= 5 
#Interview months
BF_2017_dhs %>% 
  filter(hv023== 5) %>% 
  tabyl(hv006, hv007)
BF_2014_dhs %>% 
  filter(hv023== 5) %>% 
  tabyl(hv006, hv007) 
BF_2010_dhs %>% 
  filter(hv023== 5) %>% 
  tabyl(hv006, hv007) #5, 6, 8, 12 2010

# Burundi
print_labels(BU_2016_dhs$hv023) #33= bujumbura mairie - urban
print_labels(BU_2012_mis$hv023) #hv023 is NA, combine hv024 and hv025
print_labels(BU_2012_mis$hv024)
BU_2012_mis %>% tabyl(hv024, hv025) #hv024= 1 (bujumbura mairie - urban)
#Interview months
BU_2016_dhs %>% 
  filter(hv023== 33) %>% 
  tabyl(hv006, hv007)
BU_2012_mis %>% 
  filter(hv024== 1) %>% 
  tabyl(hv006, hv007)

# Cameroon
print_labels(CM_2018_dhs$hv023) #hv023= 5
print_labels(CM_2011_dhs$hv024)
CM_2011_dhs %>% tabyl(hv024, hv025) #hv024= 3
CM_2011_dhs %>% 
  filter(hv024== 3) %>% 
  tabyl(hv006, hv007)

#Congo, DR- subsetting to just clusters in Kinshasha
print_labels(CD_2013_dhs$hv024) #hv024= 1 
CD_2013_dhs %>% 
  filter(hv024== 1) %>% 
  tabyl(hv023)  #all in the same hv023

#Cote d'Ivore- subsetting to just clusters in Abidjan
print_labels(CI_2012_dhs$hv023) #hv023= 21
print_labels(CI_2021_dhs$hv024) #hv023= 1
CI_2021_dhs %>% tabyl(hv023, hv025) 

#Gambia
print_labels(GM_2019_dhs$hv023) #hv023= 1
print_labels(GM_2013_dhs$hv024)
GM_2013_dhs %>% tabyl(hv024, hv025) #hv024= 1
#Interview months in largest urban center
interview_months(GM_2019_dhs, 1)

# Ghana
print_labels(GH_2014_dhs$hv023) #hv023= 5
print_labels(GH_2016_mis$hv024)
GH_2016_mis %>% tabyl(hv024, hv025) #matches hv023= 5
GH_2016_mis %>% tabyl(hv023)
print_labels(GH_2019_mis$hv023)
#Interview months in largest urban center
interview_months(GH_2014_dhs, 5)

# Guinea
print_labels(GN_2021_mis$hv023) #hv023= 3
print_labels(GN_2012_dhs$hv023) 
#Interview months in largest urban center
interview_months(GN_2012_dhs, 3)



# Kenya
print_labels(KE_2020_mis$hv023) #hv023= 1
print_labels(KE_2015_mis$hv023) #hv023= 1
print_labels(KE_2015_mis$hv024)
KE_2015_mis %>% tabyl(hv024, hv025)  #matches hv023= 1
KE_2015_mis %>% tabyl(hv023)


# Liberia
print_labels(LB_2011_mis$hv023) #hv023= 1
print_labels(LB_2016_mis$hv023) #hv023= 1
#Interview months in largest urban center
interview_months(LB_2011_mis, 1)

# Madagascar- Antananarivo city was not included in the MIS surveys because it's altitude is above 1,500m
#and has no malaria there
print_labels(MD_2013_mis$hv024) #The largest city is not included.
MD_2013_mis %>% tabyl(hv024, hv025) 


# Malawi- requested GPS data from DHS
print_labels(MW_2017_mis$hv024) 
print_labels(MW_2014_mis$hv023) #hv023= 23 (Lilongwe- urban)
# Malawi 2012---
#DHSCLUST= 50, 57-70 (hv001) from QGIS
var_label(MW_2012_mis$hv001) #cluster number
MW_2012_mis <- MW_2012_mis %>% 
  mutate(Lilongwe_city= if_else((hv001 >= 57 & hv001 <= 70) | hv001== 50, 1, 0))

# Malawi 2017---
#DHSCLUST= 85-100 (hv001) from QGIS
MW_2017_mis <- MW_2017_mis %>% 
  mutate(Lilongwe_city= if_else(hv001 >= 85 & hv001 < 101, 1, 0))
MW_2014_mis %>% 
  dplyr::filter(hv023== 23) %>% 
  tabyl(hv023, hv001)

#Interview months
MW_2017_mis %>% 
  filter(hv025== 1) %>% 
  tabyl(hv006, hv007)
MW_2014_mis %>% 
  filter(hv025== 1) %>% 
  tabyl(hv006, hv007)
MW_2012_mis %>% 
  filter(hv025== 1) %>% 
  tabyl(hv006, hv007)

# Mali- 
print_labels(ML_2021_dhs$hv023) #hv023= 17
print_labels(ML_2018_dhs$hv023) #hv023= 1
print_labels(ML_2015_mis$hv023) #hv023= 11
print_labels(ML_2012_dhs$hv023) #hv023= 11
#Interview months in largest urban center
interview_months(ML_2021_dhs, 17)
interview_months(ML_2018_dhs, 1)
interview_months(ML_2015_mis, 11)
interview_months(ML_2012_dhs, 11)

# Mauritania subsetting to just clusters in Nouakchott
print_labels(MR_2020_dhs$hv024) #combine north, west and south
MR_2020_dhs <- MR_2020_dhs %>% 
  mutate(nouakchott= if_else(hv024 >= 12, 1, 0))

print_labels(MR_2020_dhs_hr$hv024) #combine north, west and south
MR_2020_dhs_hr <- MR_2020_dhs_hr %>% 
  mutate(nouakchott= if_else(hv024 >= 12, 1, 0))

# Mozambique
print_labels(MZ_2011_dhs$hv023) #hv023= 21
print_labels(MZ_2015_dhs$hv023) #hv023= 21
print_labels(MZ_2018_dhs$hv023) #hv023= 21
interview_months(MZ_2011_dhs, 21) #Interview months in Maputo city- urban

# Nigeria
print_labels(NG_2021_mis$hv023) #hv023= 49
print_labels(NG_2018_dhs$hv023) #hv023= 65
print_labels(NG_2015_mis$hv023) #hv023= 71
print_labels(NG_2010_mis$hv023) 
NG_2010_mis %>% tabyl(hv024, hv025)
interview_months(NG_2021_mis, 49) #Interview months Lagos
interview_months(NG_2018_dhs, 65) 
interview_months(NG_2015_mis, 71) 

#DHSCLUST= 205- 217 (hv001) from QGIS
NG_2010_mis <- NG_2010_mis %>% 
  mutate(Lagos= if_else(hv001 >= 205 & hv001 < 218, 1, 0))
NG_2010_mis %>% filter(Lagos==1) %>% 
  tabyl(hv006, hv007)

# Rwanda
print_labels(RW_2019_dhs$hv024) #hv024= 1
print_labels(RW_2017_mis$hv023) #hv023= 1
print_labels(RW_2015_dhs$hv024)  #hv024= 1
print_labels(RW_2010_mis$hv024) #hv024= 1
interview_months(RW_2017_mis, 1) 
#survey months
RW_2015_dhs %>% filter(hv024== 1) %>% 
  tabyl(hv006, hv007)

# Senegal
print_labels(SN_2020_mis$hv023) #hv023= 1
print_labels(SN_2017_dhs$hv023) 
print_labels(SN_2016_dhs$hv023)  
print_labels(SN_2015_dhs$hv023) 
print_labels(SN_2014_dhs$hv023) 
print_labels(SN_2012_dhs$hv023) 
print_labels(SN_2010_dhs$hv024) ##hv024== 1 and hv025==1 
SN_2010_dhs %>% tabyl(hv024, hv025) #Dakar, urban= hv024= 1 AND hv025= 1
interview_months(SN_2020_mis, 1)
interview_months(SN_2012_dhs, 1)
SN_2010_dhs %>% filter(hv024== 1 & hv025== 1) %>% 
  tabyl(hv006, hv007)
SN_2010_dhs <- SN_2010_dhs %>% 
  mutate(dakar= if_else(hv024== 1 & hv025== 1, 1, 0))

#In 2020, Senegal did Not test for malaria in Dakar
SN_2020_mis %>% 
  tabyl(hv023, hml35)

# Tanzania
print_labels(TZ_2017_mis$hv023) #hv023= 7
print_labels(TZ_2015_dhs$hv023) #hv023= 13
print_labels(TZ_2012_dhs$hv023)  #hv023= 13 
interview_months(TZ_2017_mis, 7)
interview_months(TZ_2012_dhs, 13)

# TOGO
print_labels(TG_2013_dhs$hv024) #hv024= 1
print_labels(TG_2017_mis$hv023) #hv023= 1

TG_2013_dhs %>% tabyl(hv023)
TG_2013_dhs %>% tabyl(hv024, hv025)
interview_months(TG_2017_mis, 1)
TG_2013_dhs %>% filter(hv024==1) %>% 
  tabyl(hv006, hv007)

# UGANDA
print_labels(UG_2018_mis$hv023) #hv023= 1
print_labels(UG_2016_dhs$hv023) #hv023= 1
print_labels(UG_2014_mis$hv023)
print_labels(UG_2014_mis$hv024) #hv024= 4

UG_2014_mis %>% tabyl(hv024, hv025)

interview_months(UG_2016_dhs, 1)
UG_2014_mis %>% filter(hv024==4) %>% 
  tabyl(hv006, hv007)



