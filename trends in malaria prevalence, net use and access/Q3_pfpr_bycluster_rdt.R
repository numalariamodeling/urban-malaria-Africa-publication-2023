### PfPR by Cluster in all urban areas as measured by RDT

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
Out <- file.path(data_dir, "Objective_3", "PfPR_RDT")
Out_num <- file.path(data_dir, "Objective_3", "num_pos_RDT")

# PfPR by RDT per cluster for each survey

#### Functions ####
source("data_extractor_functions.R")

#survey design function 
svydesign.fun <- function(filename){
  svydesign(id= ~id,
            strata=~strat,nest=T, 
            weights= ~wt, data=filename)
}

estim_mean <- function(df, col, by){
  svy_mal <- svydesign.fun(df)
  design <- subset(svy_mal, hv025== 1) #subsetting to urban areas only
  clu_est <- result.mean(col, by, design) 
}

#### Calculate value for PfPR by RDT ####
dhs <- list(NG_2010_mis, NG_2015_mis, NG_2018_dhs, NG_2021_mis) #DONE
dhs <- list(AO_2011_mis, AO_2015_dhs, BF_2010_dhs, BF_2014_dhs, BF_2017_dhs) #DONE
dhs <- list(BJ_2012_dhs, BJ_2017_dhs) #DONE
dhs <- list(CM_2018_dhs) #DONE
dhs <- list(CM_2018_dhs, GM_2013_dhs, GM_2019_dhs, GH_2014_dhs, GH_2016_mis, GH_2019_mis, GN_2012_dhs, GN_2021_mis) #DONE 
dhs <- list(KE_2015_mis, KE_2020_mis, LB_2011_mis, LB_2016_mis, MD_2016_mis, MD_2021_dhs) #DONE

dhs <- list(MD_2011, MD_2013) #DONE
dhs <- list(ML_2012_dhs, ML_2015_mis, ML_2018_dhs, ML_2021_dhs, MW_2012_mis, MW_2014_mis, MW_2017_mis,
            MZ_2011_dhs, MZ_2015_dhs, MZ_2018_dhs)#DONE
dhs <- list(RW_2010_mis, RW_2015_dhs, RW_2019_dhs) #DONE
dhs <- list(SN_2010_dhs, SN_2012_dhs, SN_2014_dhs, SN_2015_dhs, SN_2016_dhs, SN_2017_dhs, SN_2020_mis ) 
dhs <- list(TZ_2012_dhs,TZ_2015_dhs, TZ_2017_mis, TG_2013_dhs, TG_2017_mis, UG_2014_mis, UG_2016_dhs, UG_2018_mis)
dhs <- list(CD_2013_dhs, CI_2012_dhs, MR_2020_dhs)

dhs <- dhs %>% map(~mutate(., wt=hv005/1000000,
                           strat=hv022,
                           id=hv021, num_p=1,
                           #net_use = ifelse(hml12 %in% c(1:3), 1,0),
                           ml_rdt_pos= case_when( #Positive for malaria by RDT in children 6-59 months
                             hv042== 1 & hv103==1 & hc1 %in% c(6:59) & hml35== 0 ~ 0,
                             hv042== 1 & hv103==1 & hc1 %in% c(6:59) & hml35== 1 ~ 1),
                           ml_rdt_test= if_else(is.na(ml_rdt_pos), 0, 1))) %>% 
  map(~dplyr::select(.,wt, strat, id, num_p, country_year, hv042, hc1, hv103, hv024, hv025, hv001, ml_rdt_pos, ml_rdt_test ))


options(survey.lonely.psu="adjust") 
TZ_2012 <- dhs[[1]]

## PfPR by RDT

tab1 <- svyby(~ml_rdt_pos, ~hv001, svydat, svymean, vartype=c("se", "ci"), na.rm= TRUE) %>% 
  mutate(ml_rdt_pos= ml_rdt_pos*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab1

#generate proportion PfPR (weighted) by cluster  
vars <- c('ml_rdt_pos')

for (i in 1:length(vars)) {
  col <- list(vars[i])
  by <- list('hv001')
  df <- dhs %>% 
    map(~drop_na(.,vars[i]))
  df <-  pmap(list(df,col,by), estim_mean)
  df_pfpr <- plyr::ldply(df)
  write.csv(df_pfpr, file =file.path(Out, "PfPR_CI_CD_MR_DHS_12_20_23.csv"))
}


# Number of children (6- 59 months) positive for malaria by RDT
for (i in 1:length(vars)) {
  col <- list(vars[i])
  df <- dhs %>% 
    map(~drop_na(.,vars[i]))
  
  df1 <- df %>% map(~filter(., hv025== 1)) %>% 
   map(~dplyr::group_by(., country_year, hv001)) %>% 
    map(~tabyl(., hv001, ml_rdt_pos)) %>%  
    map(~rename(., "num_pos_rdt"= "1", "num_neg_rdt"= "0"))
  df1 <- plyr::ldply(df1) 
  
  write.csv(df1, file =file.path(Out_num, "numRDT_pos_CI_CD_MR_DHS_12_20_23.csv"))
}


# Number of children (6- 59 months) tested for RDT by cluster
df2 <- dhs %>% map(~filter(., hv025== 1)) %>% 
  map(~dplyr::group_by(., country_year, hv001)) %>% 
  map(~tabyl(., hv001, ml_rdt_test)) %>% 
  map(~rename(., "num_tested_rdt"= "1", "num_tested_rdt"= "0")) %>% 
  plyr::ldply() 




# COUNRIES WITH OTHER CONSIDERATIONS FALLING OUTSIDE THE ABOVE CODE

#BURUNDI- did not collect hv042 (remove from consideration of malaria positivity)
dhs <- list(BU_2012_mis, BU_2016_dhs) 
dhs <- dhs %>% map(~mutate(., wt=hv005/1000000,
                           strat=hv022,
                           id=hv021, num_p=1,
                           ml_rdt_pos= case_when( #Positive for malaria by RDT in children 6-59 months
                            hv103==1 & hc1 %in% c(6:59) & hml35== 0 ~ 0,
                            hv103==1 & hc1 %in% c(6:59) & hml35== 1 ~ 1),
                           ml_rdt_test= if_else(is.na(ml_rdt_pos), 0, 1)))


# CAMEROON- 
summary(CM_2011_dhs$hml32) # regular RDT variable is missing

var_label(CM_2011_dhs$hml33) #hml33= result of malaria measurements, just whether it was measured or not
val_labels(CM_2011_dhs$hml33) 
val_labels(CM_2011_dhs$sh418) #sh418= Result of RDT (country-specific variable)
CM_2011_dhs <- CM_2011_dhs %>% mutate(wt=hv005/1000000,
                                      strat=hv022,
                                      id=hv021,
                                      ml_rdt_pos= case_when(sh418==1 | sh418==2  | sh418== 3 ~1,
                                                         sh418== 4 ~0),
                                      ml_rdt_test= if_else(is.na(ml_rdt_pos), 0, 1))

CM_2011_dhs %>% tabyl(ml_rdt_pos, sh418) #created new var for Pf. positive by RDT

vars <- c('ml_rdt_pos')
dhs1 <- CM_2011_dhs
dhs1 <- RW_2017_mis
#For single survey
for (i in 1:length(vars)) {
  col <- list(vars[i])
  by <- list('hv001')
  df <- dhs1 %>% 
    drop_na(vars[i])
  df <- estim_mean(df,col,by)
  #df_pfpr <- plyr::ldply(df)
  write.csv(df, file =file.path(Out, "PfPR_RW_2017_DHS_07_23_23.csv"))
}

# Number of children (6- 59 months) positive for malaria by RDT
for (i in 1:length(vars)) {
  col <- list(vars[i])
  df <- dhs1 %>% 
    drop_na(vars[i])
  
  df1 <- df %>% filter(hv025== 1) %>% 
    dplyr::group_by(country_year, hv001) %>% 
    tabyl(hv001, ml_rdt_pos) %>% 
    rename("num_pos_rdt"= "1", "num_neg_rdt"= "0")
  #df1 <- plyr::ldply(df1) 
  
  write.csv(df1, file =file.path(Out_num, "numRDT_pos_RW_2017_DHS_07_23_23.csv"))
}


# MADAGASCAR- 2011 & 2013 MISSING STRATA- can combine hv024 and hv025 to create strat
MD_2011 <- MD_2011_mis %>% group_by(hv024,hv025) %>% 
  mutate(strat = cur_group_id())
MD_2013 <- MD_2013_mis %>% group_by(hv024,hv025) %>% 
  mutate(strat = cur_group_id())

MD_2011 %>% filter(hv025==1) %>% 
  tabyl(hv024)



## Rwanda 2017 survey is missing hc1 (child's age in months) and hv042 (household selected for hemoglobin).
#will use hml16a in place of hc1, will forgo hv042 variable in ml_rdt_pos calculation.
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
         ml_rdt_test= if_else(is.na(ml_rdt_pos), 0, 1))

svydat <- svydesign.fun(RW_2017_mis) 
tab1 <- svyby(~ml_rdt_pos, ~hv023, svydat, svymean, vartype=c("ci"), na.rm= TRUE) %>% 
  filter(hv023==1) %>% 
  mutate(ml_rdt_pos= ml_rdt_pos*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab1

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


