### PfPR by wealth quintile in all urban areas

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

library(viridis)
library(ggrepel)
library(scales)
library(zoo)

#Read in file paths
user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "NU-malaria-team Dropbox")
data_dir <- file.path(NuDir, "data", 'urban_malaria_net_ownership_data', 'Extracted_csv')


# PfPR by RDT by wealth quintile for urban areas

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

#### Calculate value for PfPR by RDT (Every survey captures PfPR by RDT but not all have microscopy results) ####

dhs <- list(AO_2011_mis, AO_2015_dhs) #Update for each country
dhs <- list(MD_2016_mis, MD_2021_dhs)
dhs <- list(NG_2010_mis, NG_2015_mis, NG_2018_dhs, NG_2021_mis)
dhs <- list(UG_2014_mis, UG_2016_dhs, UG_2018_mis)

dhs <- list(SN_2010_dhs, SN_2012_dhs, SN_2014_dhs, SN_2015_dhs, SN_2016_dhs, SN_2017_dhs, SN_2020_mis)

dhs <- dhs %>% map(~mutate(., wt=hv005/1000000,
                           strat=hv022,
                           id=hv021, num_p=1,
                           ml_rdt_pos= case_when( #Positive for malaria by RDT in children 6-59 months
                             hv042== 1 & hv103==1 & hc1 %in% c(6:59) & hml35== 0 ~ 0,
                             hv042== 1 & hv103==1 & hc1 %in% c(6:59) & hml35== 1 ~ 1),
                           ml_rdt_test= if_else(is.na(ml_rdt_pos), 0, 1),
                           wealth = ifelse(hv270 <4, 0, 1)))

SN_10 <- dhs[[1]]
SN_12 <- dhs[[2]]
SN_14 <- dhs[[3]]
SN_15 <- dhs[[4]]
SN_16 <- dhs[[5]]
SN_17 <- dhs[[6]]
SN_20 <- dhs[[7]]



options(survey.lonely.psu="adjust") 
svydat <- svydesign.fun(UG_18) # Re-do for each survey

## PfPR by RDT by wealth (4th and 5th wealthiest quintiles vs. lower)
tab1 <- svyby(~ml_rdt_pos, ~(wealth + hv025), design= subset(svydat, hv025==1), svymean, vartype=c("se", "ci"), na.rm= TRUE) %>% 
  mutate(ml_rdt_pos= ml_rdt_pos*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab1

svyby(~ml_rdt_pos, ~wealth+hv025, svydat, na = TRUE, svymean)
#barplot(tab2, beside=TRUE,legend=TRUE)

# Save in file ur_pfpr_wealth.xlsx


# COUNRIES WITH OTHER CONSIDERATIONS FALLING OUTSIDE THE ABOVE CODE

#BURUNDI- did not collect hv042 (remove from consideration of malaria positivity)
dhs <- list(BU_2012_mis, BU_2016_dhs) #Update for each country
dhs <- dhs %>% map(~mutate(., wt=hv005/1000000,
                           strat=hv022,
                           id=hv021, num_p=1,
                           ml_rdt_pos= case_when( #Positive for malaria by RDT in children 6-59 months
                            hv103==1 & hc1 %in% c(6:59) & hml35== 0 ~ 0,
                            hv103==1 & hc1 %in% c(6:59) & hml35== 1 ~ 1),
                           ml_rdt_test= if_else(is.na(ml_rdt_pos), 0, 1),
                           wealth = ifelse(hv270 <4, 0, 1)))

BU_12 <- dhs[[1]]
BU_16 <- dhs[[2]]

svydat <- svydesign.fun(BU_16) # Re-do for each survey

## PfPR by wealth category and urban/ rural
svyby(~ml_rdt_pos, ~wealth+hv025, svydat, na = TRUE, svymean)

# CAMEROON- 
summary(CM_2011_dhs$hml32) # regular RDT variable is missing
summary(CM_2018_dhs$hml35) # 2018- does have regular RDT data (hml35)

var_label(CM_2011_dhs$hml33) #hml33= result of malaria measurements, just whether it was measured or not
val_labels(CM_2011_dhs$hml33) 
val_labels(CM_2011_dhs$sh418) #sh418= Result of RDT (country-specific variable)
CM_2011_dhs <- CM_2011_dhs %>% mutate(wt=hv005/1000000,
                                      strat=hv022,
                                      id=hv021,
                                      pf_rdt= case_when(sh418==1 | sh418==2  | sh418== 3 ~1,
                                                         sh418== 4 ~0),
                                      ml_rdt_test= if_else(is.na(pf_rdt), 0, 1),
                                      wealth = ifelse(hv270 <4, 0, 1))

CM_2011_dhs %>% tabyl(pf_rdt, sh418) #created new var for Pf. positive by RDT
#For Cameroon
svydat <- svydesign.fun(CM_2011_dhs) # Re-do for each survey

svyby(~pf_rdt, ~wealth+hv025, svydat, na = TRUE, svymean) # malaria positive by wealth and urban/rural



# MADAGASCAR- 2011 & 2013 MISSING STRATA- can combine hv024 and hv025 to create strat
MD_2013 <- MD_2013_mis %>% group_by(hv024,hv025) %>% 
  mutate(strat = cur_group_id(),
         wt=hv005/1000000,
         id=hv021,
         ml_rdt_pos= case_when( #Positive for malaria by RDT in children 6-59 months
           hv042== 1 & hv103==1 & hc1 %in% c(6:59) & hml35== 0 ~ 0,
           hv042== 1 & hv103==1 & hc1 %in% c(6:59) & hml35== 1 ~ 1),
         ml_rdt_test= if_else(is.na(ml_rdt_pos), 0, 1),
         wealth = ifelse(hv270 <4, 0, 1))

svydat <- svydesign.fun(MD_2011) 
svydat <- svydesign.fun(MD_2013) 
#For Madagascar- 2011 and 2013
svyby(~ml_rdt_pos, ~wealth+hv025, svydat, na = TRUE, svymean)



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
         ml_rdt_pos= case_when( #Positive for malaria by RDT in children 6-59 months
           hv103==1 & hml16a%in% c(6:59) & hml35== 0 ~ 0,
           hv103==1 & hml16a %in% c(6:59) & hml35== 1 ~ 1),
         ml_rdt_test= if_else(is.na(ml_rdt_pos), 0, 1),
         wealth = ifelse(hv270 <4, 0, 1))

svydat <- svydesign.fun(RW_2017_mis) 
svyby(~ml_rdt_pos, ~wealth+hv025, svydat, na = TRUE, svymean)






