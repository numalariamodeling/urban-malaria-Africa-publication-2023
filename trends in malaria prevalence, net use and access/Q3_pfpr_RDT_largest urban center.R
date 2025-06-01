### PfPR by largest urban center and all urban areas

library(readxl)
library(haven)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(janitor)
library(haven)
library(survey)
library(skimr)
library(labelled)


#Read in file paths
user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "NU-malaria-team Dropbox")
data_dir <- file.path(NuDir, "data", 'urban_malaria_net_ownership_data', 'Extracted_csv')


# PfPR by RDT for largest urban center per country/ survey

#### Functions ####
#survey design function 
svydesign.fun <- function(filename){
  svydesign(id= ~id,
            strata=~strat,nest=T, 
            weights= ~wt, data=filename)
}
#survey estimates generating functions  
result_mean<- function(var, design) {
  p_est<-svymean(formula=make.formula(var), FUN=svymean, design, vartype= "ci", na.rm=T, influence= TRUE)
}

estim_mean <- function(df, col){
  svy_mal <- svydesign.fun(df)
  design <- subset(svy_mal, hv024== 1) #must manually change hv023 number each time! 
  clu_est <- result_mean(col, design) 
}

#### Calculate value for PfPR by RDT ####

dhs <- list(RW_2010_mis, RW_2015_dhs) #Update for each country
dhs <- list(MW_2012_mis, MW_2017_mis, MW_2014_mis)

dhs <- dhs %>% map(~mutate(., wt=hv005/1000000,
                           strat=hv022,
                           id=hv021, num_p=1,
                           net_use = ifelse(hml12 %in% c(1:3), 1,0),
                          # ml_micro_pos= case_when( #Positive for malaria by microscopy in children 6-59 months
                             #hv042== 1 & hv103==1 & hc1 %in% c(6:59) & (hml32== 0 | hml32== 6) ~ 0,
                            # hv042== 1 & hv103==1 & hc1 %in% c(6:59) & hml32== 1 ~ 1), 
                          # ml_micro_test= if_else(is.na(ml_micro_pos), 0, 1), #NA= not tested
                           ml_rdt_pos= case_when( #Positive for malaria by RDT in children 6-59 months
                             hv042== 1 & hv103==1 & hc1 %in% c(6:59) & hml35== 0 ~ 0,
                             hv042== 1 & hv103==1 & hc1 %in% c(6:59) & hml35== 1 ~ 1),
                           ml_rdt_test= if_else(is.na(ml_rdt_pos), 0, 1)))


MW_12 <- dhs[[1]]
MW_17 <- dhs[[2]]
MW_14 <- dhs[[3]]

options(survey.lonely.psu="adjust") 
svydat <- svydesign.fun(MW_14) # Re-do for each survey

## PfPR by RDT in Largest Urban center
tab1 <- svyby(~ml_rdt_pos, ~(hv023 +hv025), design= subset(svydat, hv025==1), svymean, vartype=c("se", "ci"), na.rm= TRUE) %>% 
  filter(hv023== 23) %>% # Update filter for each survey
  mutate(ml_rdt_pos= ml_rdt_pos*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab1


## PfPR by RDT in all Urban areas excluding the largest urban center

tab1 <- svyby(~ml_rdt_pos, ~hv025, design= subset(svydat, hv023 !=23), svymean, vartype=c("se", "ci"), na.rm= TRUE) %>% 
  filter(hv025==1) %>%
  mutate(ml_rdt_pos= ml_rdt_pos*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab1


# Number of children (6- 59 months) positive for malaria by RDT by State (in largest urban state)
df1 <- dhs %>% map(~dplyr::select(.,country_year, hv023, ml_rdt_pos)) %>% map(~filter(., hv023== 1  & ml_rdt_pos== 1)) %>% 
  map(~dplyr::group_by(., country_year)) %>% 
  map(~tabyl(.,country_year,  ml_rdt_pos)) %>% 
  plyr::ldply() 



# Number of children (6- 59 months) tested for RDT by State (in largest urban state)
df2 <- dhs %>% map(~dplyr::select(.,country_year, hv023, ml_rdt_test)) %>% map(~filter(., hv023== 1 & ml_rdt_test== 1)) %>% 
  map(~dplyr::group_by(., country_year)) %>% 
  map(~tabyl(.,country_year,  ml_rdt_test)) %>% 
  plyr::ldply() 


# COUNRIES WITH OTHER CONSIDERATIONS FALLING OUTSIDE THE ABOVE CODE

#BURUNDI- did not collect hv042 (remove from consideration of malaria positivity)
dhs <- list(BU_2012_mis, BU_2016_dhs) #Update for each country
dhs <- dhs %>% map(~mutate(., wt=hv005/1000000,
                           strat=hv022,
                           id=hv021, num_p=1,
                           ml_micro_pos= case_when( #Positive for malaria by microscopy in children 6-59 months
                           hv103==1 & hc1 %in% c(6:59) & (hml32== 0 | hml32== 6) ~ 0,
                            hv103==1 & hc1 %in% c(6:59) & hml32== 1 ~ 1), 
                           ml_micro_test= if_else(is.na(ml_micro_pos), 0, 1), #NA= not tested
                           ml_rdt_pos= case_when( #Positive for malaria by RDT in children 6-59 months
                            hv103==1 & hc1 %in% c(6:59) & hml35== 0 ~ 0,
                            hv103==1 & hc1 %in% c(6:59) & hml35== 1 ~ 1),
                           ml_rdt_test= if_else(is.na(ml_rdt_pos), 0, 1)))

BU_12 <- dhs[[1]]
BU_16 <- dhs[[2]]

# Number of children (6- 59 months) positive for malaria by microscopy by State (in largest urban state)
BU_12 %>% filter(hv024== 1) %>%  #Update hv023 # per Survey
  tabyl(hv024, ml_micro_pos)

# Number of children (6- 59 months) tested for microscopy by State (in largest urban state)
BU_12 %>% filter(hv024== 1) %>% 
  tabyl(ml_micro_test)


# CAMEROON- 
summary(CM_2011_dhs$hml32) # regular RDT variable is missing
summary(CM_2011_dhs$hml35) # Micro-specific data is missing
summary(CM_2018_dhs$hml35) # 2018- does have regular RDT data (hml35)

var_label(CM_2011_dhs$hml33) #hml33= result of malaria measurements, just whether it was measured or not
val_labels(CM_2011_dhs$hml33) 
val_labels(CM_2011_dhs$sh418) #sh418= Result of RDT (country-specific variable)
CM_2011_dhs <- CM_2011_dhs %>% mutate(wt=hv005/1000000,
                                      strat=hv022,
                                      id=hv021,
                                      pf_rdt= case_when(sh418==1 | sh418==2  | sh418== 3 ~1,
                                                         sh418== 4 ~0),
                                      ml_rdt_test= if_else(is.na(pf_rdt), 0, 1))

CM_2011_dhs %>% tabyl(pf_rdt, sh418) #created new var for Pf. positive by RDT
#For Cameroon
svydat <- svydesign.fun(CM_2011_dhs) # Re-do for each survey
tab3 <- svyby(~pf_rdt, ~hv024, svydat, svymean, vartype=c("ci"), na.rm= TRUE) %>% 
  filter(hv024==3) %>% 
  mutate(pf_rdt= pf_rdt*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab3
#PfPr in all urban minus largest urban center
tab4 <- svyby(~pf_rdt, ~hv025, design= subset(svydat, hv024 !=3), svymean, vartype=c("ci"), na.rm= TRUE) %>% 
  filter(hv025==1) %>% 
  mutate(pf_rdt= pf_rdt*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab4 

# Number tested by State
CM_2011_dhs %>% filter(hv024==3) %>%  #Update hv023 # per Survey
  tabyl(hv024, ml_rdt_test)
# Number positive by State
CM_2011_dhs %>% filter(hv024==3) %>%  #Update hv023 # per Survey
  tabyl(hv024, pf_rdt)



GM_2013 %>% tabyl(ml_micro_test, hv024) #just confirming that kids were tested by micro in Gambia since PfPR= 0
GN_2018_dhs %>% tabyl(ml_rdt_test, hv023) #No micro or RDT tests were done in Guinea in 2018
KE_2020 %>% tabyl(ml_rdt_test, hv023) 
NG_2015_mis %>% tabyl(hml32, hv023) #No positive micro tests in Lagos in 2015
RW_2015_dhs %>% tabyl(hml32, hv024)
RW_2015 %>% tabyl(ml_micro_pos)
SN_16 %>% tabyl(hml32, hv023) #just confirming tests were done- all negative in Dakar 
SN_15 %>% tabyl(hml32, hv023)
SN_12 %>% tabyl(hml32, hv023)
UG_2016 %>% tabyl(hml32)

#For Malawi- 2017 and 2012- run by cluster number
svydat <- svydesign.fun(MW_2017) 
tab <- svyby(~ml_rdt_pos, ~Blantyre_city, svydat, svymean, vartype=c("ci"), na.rm= TRUE) %>% 
  filter(Blantyre_city== 1) %>% # Update filter for each survey
  mutate(ml_rdt_pos= ml_rdt_pos*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab
# All urban areas except Blantyre
tab <- svyby(~ml_rdt_pos, ~(Blantyre_city + hv025), svydat, svymean, vartype=c("ci"), na.rm= TRUE) %>% 
  filter(Blantyre_city== 0) %>% # Update filter for each survey
  mutate(ml_rdt_pos= ml_rdt_pos*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab


# MADAGASCAR- 2011 & 2013 MISSING STRATA- can combine hv024 and hv025 to create strat
MD_2011 <- MD_2011 %>% group_by(hv024,hv025) %>% 
  mutate(strat = cur_group_id())

svydat <- svydesign.fun(MD_2013) 
#For Madagascar- run by cluster number
tab <- svyby(~ml_rdt_pos, ~Antananarivo_city, svydat, svymean, vartype=c("ci"), na.rm= TRUE) %>% 
  filter(Antananarivo_city== 1) %>% # Update filter for each survey
  mutate(ml_rdt_pos= ml_rdt_pos*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab
MD_2011_mis %>% 
  tabyl(Antananarivo_city, hv025)

#All urban areas less Antananarivo_city

tab <- svyby(~ml_rdt_pos, ~(Antananarivo_city +hv025), svydat, svymean, vartype=c("ci"), na.rm= TRUE) %>% 
  filter(Antananarivo_city== 0) %>% # Update filter for each survey
  mutate(ml_rdt_pos= ml_rdt_pos*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab


# Nigeria- 2010
svydat <- svydesign.fun(NG_2010)
tab1 <- svyby(~ml_rdt_pos, ~Lagos, svydat, svymean, vartype=c("ci"), na.rm= TRUE) %>% 
  filter(Lagos== 1) %>% 
  mutate(ml_rdt_pos= ml_rdt_pos*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab1
tab2 <- svyby(~ml_rdt_pos, ~(Lagos + hv025), design= svydat, svymean, vartype=c("ci"), na.rm= TRUE) %>% 
  filter(Lagos== 0) %>% 
  mutate(ml_rdt_pos= ml_rdt_pos*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab2

# Number positive
NG_2010 %>% filter(Lagos== 1) %>%  #Update hv023 # per Survey
  tabyl(ml_micro_pos)
# Number tested
NG_2010 %>% filter(Lagos== 1) %>%  #Update hv023 # per Survey
  tabyl(ml_micro_test)

## Rwanda 2017 survey is missing hc1 (child's age in months) and hv042 (household selected for hemoglobin).
#will use hml16a in place of hc1, will forgo hv042 variable in ml_micro_pos calculation.
var_label(RW_2017_mis$hml16a) #missing variable hc1 (child's age in months), but hml16a (age in months for children) can be used instead

RW_2017_mis <- RW_2017_mis %>% 
  mutate(
         wt=hv005/1000000,strat=hv022,
         id=hv021,
         urban = case_when(hv025== 2~ 0,
                           hv025== 1~ 1),
         ml_micro_pos= case_when(
           hv103==1 & hml16a %in% c(6:59) & (hml32== 0 | hml32== 6) ~ 0,
           hv103==1 & hml16a %in% c(6:59) & hml32== 1 ~ 1), 
         ml_micro_test= if_else(is.na(ml_micro_pos), 0, 1),
         ml_rdt_pos= case_when( #Positive for malaria by RDT in children 6-59 months
           hv103==1 & hml16a%in% c(6:59) & hml35== 0 ~ 0,
           hv103==1 & hml16a %in% c(6:59) & hml35== 1 ~ 1),
         ml_rdt_test= if_else(is.na(ml_rdt_pos), 0, 1))

svydat <- svydesign.fun(RW_2017_mis) 
tab1 <- svyby(~ml_rdt_pos, ~hv023, svydat, svymean, vartype=c("ci"), na.rm= TRUE) %>% 
  filter(hv023==1) %>% 
  mutate(ml_rdt_pos= ml_rdt_pos*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab1
RW_2017_mis %>% tabyl(hv023, hv025) #hv023= kigali (Urban)

tab2 <- svyby(~ml_rdt_pos, ~hv025, design= subset(svydat, hv023 !=1), svymean, vartype=c("ci"), na.rm= TRUE) %>% 
  filter(hv025==1) %>% 
  mutate(ml_rdt_pos= ml_rdt_pos*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab2

# Number positive
RW_2017_mis %>% filter(hv023==1) %>%  #Update hv023 # per Survey
  tabyl(ml_rdt_pos)
# Number tested
RW_2017_mis %>% filter(hv023==1) %>%  #Update hv023 # per Survey
  tabyl(ml_rdt_test)

#PfPR by RDT
dhs <- list(CM_2011_dhs)
CM_2018 <- dhs[[1]]
svydat <- svydesign.fun(CM_2018) # Re-do for each survey
tab2 <- svyby(~ml_rdt_pos, ~hv023, svydat, svymean, vartype=c("ci"), na.rm= TRUE) %>% 
  filter(hv023==17) %>% # Update filter for each survey
  mutate(ml_rdt_pos= ml_rdt_pos*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab2
SN_2020_mis %>% tabyl(hml35, hv023) #SN 2020 MIS didn't test by RDT in Dakar at all.


#For Madagascar- 2021
svydat <- svydesign.fun(MD_2021) # Re-do for each survey
tab4 <- svyby(~ml_rdt_pos, ~hv024, svydat, svymean, vartype=c("ci"), na.rm= TRUE) %>% 
  filter(hv024==10) %>% # Update filter for each survey
  mutate(ml_rdt_pos= ml_rdt_pos*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab4

# Senegal 2010
svydat <- svydesign.fun(SN_2010) 
tab <- svyby(~ml_rdt_pos, ~dakar, svydat, svymean, vartype=c("ci"), na.rm= TRUE) %>% 
  filter(dakar== 1) %>% # Update filter for each survey
  mutate(ml_rdt_pos= ml_rdt_pos*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab
# All urban areas except Dakar
tab <- svyby(~ml_rdt_pos, ~(dakar + hv025), svydat, svymean, vartype=c("ci"), na.rm= TRUE) %>% 
  filter(dakar== 0) %>% # Update filter for each survey
  mutate(ml_rdt_pos= ml_rdt_pos*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab

#loading required data: Pre-estimated PFPR data was downloaded from the DHS statcompiler site (for all urban areas)
# PfPR data for city/largest urban center was calculated as indicated above.
# Zambia values were added from Malaria operation plan since they reported MIS data
pfpr_df <- read.csv(file.path(data_dir,"pfpr_urban_city.csv"), header = T, sep = ',') %>% 
  filter(Category != "Total") %>% mutate(test_result = Value/100)  



#Net use by children selected for hemoglobin
#Create a dataset of only children under 5 selected for hemoglobin, child slept there last night and have result for test
pfpr_df <- dhs %>% map(~filter(., hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59))) #& hml16 <59 & hml32 %in% c(0, 1,6)
pfpr_df<- pfpr_df %>%  map(~mutate(., net_use_child = net_use))

vars <- 'net_use_child'


#For largest city- CHANGE CITYNUM MANUALLY in estim_mean function. Sometimes different number/ variable for each survey.
## Housing quality
for (i in 1:length(vars)){
  col <- list(vars[i])
  by <- list('country_year')
  df <- pfpr_df %>% 
    map(~drop_na(.,vars[i]))
  df <- pmap(list(df,col,by), estim_prop)
  df <- plyr::ldply(df)
  df[, vars[i]]<- df[, vars[i]]*100
  write.csv(df, file = file.path(filep, paste0(vars[i], '_SN', "_07072023.csv")), row.names= FALSE)
}
