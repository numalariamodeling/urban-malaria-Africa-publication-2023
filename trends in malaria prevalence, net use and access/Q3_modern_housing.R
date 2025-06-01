### Urban Nets Q3- Housing quality indicator  ###
#Author: Colleen Leonard
#Date last edited: 4/21/2023

library(readxl)
library(plyr)
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
ProjectDir <- file.path(NuDir, "data", 'urban_malaria_net_ownership_data', 'DHS')
data_dir <- file.path(NuDir, "data", 'urban_malaria_net_ownership_data', 'Extracted_csv')
Out <- file.path(data_dir, "Objective_3", "modern_housing")


# -----------------------------------------
### Required functions and settings
## -----------------------------------------
source("functions/data_extractor_functions.R")


## --------------------------------------
### Read in DHS and MIS files (PR data) 
## --------------------------------------

source("read_DHS_data.R")
mem_used() #total size of all objects in memory

#Read in datasets as a lists
dhs <- list(BJ_2012_dhs, BJ_2017_dhs, BF_2010_dhs, BF_2014_dhs, BF_2017_dhs, BU_2012_mis, BU_2016_dhs)

dhs <- list(CM_2011_dhs, CM_2018_dhs, GM_2013_dhs, GM_2019_dhs, GH_2014_dhs, GH_2016_mis, GH_2019_mis,
             GN_2012_dhs, GN_2021_mis)

dhs <- list(KE_2020_mis)
dhs <- list(LB_2011_mis, LB_2016_mis)
dhs <- list(MD_2016_mis, MD_2021_dhs, ML_2012_dhs, ML_2015_mis, ML_2018_dhs, ML_2021_dhs, 
            MW_2012_mis, MW_2014_mis, MW_2017_mis, MZ_2011_dhs, MZ_2015_dhs, MZ_2018_dhs)
dhs <- list(NG_2015_mis, NG_2018_dhs) #DONE
dhs <- list(RW_2010_mis, RW_2015_dhs, RW_2017_mis) #DONE

dhs <- list(SN_2012_dhs,SN_2014_dhs, SN_2015_dhs, SN_2016_dhs, 
            SN_2017_dhs, SN_2020_mis, TG_2013_dhs, TG_2017_mis, TZ_2012_dhs, TZ_2015_dhs, TZ_2017_mis, UG_2014_mis, UG_2016_dhs, UG_2018_mis)
dhs <- list(BF_2021_dhs)

dhs <- list(CD_2013_dhs, CI_2012_dhs, MR_2020_dhs)

#Data cleaning
dhs <- dhs %>% map(~mutate(., wealth = ifelse(hv270 <4, 0, 1),
                           floor_type = ifelse(hv213 >= 98, NA, ifelse(hv213 %in% c(30, 31, 32, 33, 34, 35, 36, 37),1, 0)),
                           wall_type = ifelse(hv214 >= 98, NA , ifelse (hv214 %in% c(30, 31, 32, 33, 34,35, 37, 38),1, 0)),
                           roof_type = ifelse(hv215 >= 98, NA, ifelse(hv215 %in% c(30, 31, 33, 34,35),1, 0)),
                           housing_q = ifelse(floor_type == 1 & wall_type == 1 & roof_type == 1,1, 0), #indicator var for house quality
                           
                           net_use = ifelse(hml12 %in% c(1,2), 1,0),
                           wt=hv005/1000000,strat=hv022,
                           id=hv021, num_p=1,
                           age = ifelse(hv105 >= 98, NA, hv105),
                           age_cat = ifelse(age <18, 1, 0),
                           household_size = hv013,
                           region = hv024, 
                           interview_month = hv006)) %>% 
  map(~dplyr::select(.,wt, strat, id, country_year, wealth, hv042, hv103, hv023, hv024, hv025, hv001, household_size, 
                     floor_type, wall_type, roof_type, housing_q, region))


BF_2021 <- dhs[[1]]

#Survey Design object
options(survey.lonely.psu="adjust") 
svydat <- svydesign(data=NG_dhs_2010, strata=~strat, id=~id, weights=~wt, nest= T)
summary(svydat) #239 clusters, 12 strata

# Calculate proportion with modern housing and proportion of cluster that is considered wealthy
#generate proportion PfPR (weighted) by cluster  

#Run once per country group
dhs <- list(BF_2021)
vars <- c('wealth', 'housing_q')
country_name <- "_CD_CI_MR"

for (i in 1:length(vars)) {
  col <- list(vars[i])
  by <- list('hv001')
  df <- dhs %>% 
    map(~drop_na(.,vars[i]))
  df <-  pmap(list(df,col,by), estim_mean)
  df_all <- plyr::ldply(df)
  
  df_dhs <- dhs %>% map(~dplyr::filter(., hv025==1)) %>% 
    map(~group_by(., country_year, hv001)) %>% 
    map(~dplyr::select(., country_year, hv001)) %>% 
    map(~slice(.,1)) #get list of country/ cluster numbers 
  df_all_2 <- plyr::ldply(df_dhs)
  df_final <- cbind(df_all_2, df_all)
  
  write.csv(df_final, file =file.path(Out, paste0(as.character(vars[i]), country_name, "_12_20_23.csv")))
}
SN_2010 %>% filter(hv025==1) %>% 
  tabyl(hv001)
RW_2019_dhs %>% filter(hv025==1) %>% 
  tabyl(hv214, wall_type)


#### Exploratory Analysis- Correlations ####

#Example with Angola, 2011 
AO_mis_2011 <- dhs[[1]] #2011 MIS
AO_mis_2015 <- dhs[[2]] 

#Survey design
options(survey.lonely.psu="adjust") # this option allows admin units with only one cluster to be analyzed
svydat <- svydesign.fun(AO_mis_2011) # Re-do for each survey

#### QA- checking different housing codes for some countries ####

#Angola categories- different labels for wall and roof
print_labels(AO_2015_dhs$hv214) #wall- 31-34= finished, 35= wood
print_labels(AO_2015_dhs$hv213) #all 31-34= finished
print_labels(AO_2015_dhs$hv215) #36= tiles= finished

AO_2011_mis <- AO_2011_mis %>% 
  mutate(wealth = ifelse(hv270 <4, 0, 1),
        floor_type = ifelse(hv213 >= 98, NA, ifelse(hv213 %in% c(30, 31, 32, 33, 34, 35),1, 0)),
         wall_type = ifelse(hv214 >= 98, NA , ifelse (hv214 %in% c(31, 32),1, 0)),
         roof_type = ifelse(hv215 >= 98, NA, ifelse(hv215 %in% c(31, 33, 34),1, 0)),
         housing_q = ifelse(floor_type == 1 & wall_type == 1 & roof_type == 1,1, 0),
         wt=hv005/1000000,strat=hv022,
         id=hv021,
         urban = case_when(hv025== 2~ 0,
                           hv025== 1~ 1))

AO_2015_dhs <- AO_2015_dhs %>% 
  mutate(wealth = ifelse(hv270 <4, 0, 1),
        floor_type = ifelse(hv213 >= 98, NA, ifelse(hv213 %in% c(30, 31, 32, 33, 34),1, 0)),
         wall_type = ifelse(hv214 >= 98, NA , ifelse (hv214 %in% c(31, 32, 33, 34),1, 0)),
         roof_type = ifelse(hv215 >= 98, NA, ifelse(hv215 %in% c(31, 33, 34,35, 36),1, 0)),
         housing_q = ifelse(floor_type == 1 & wall_type == 1 & roof_type == 1,1, 0),
         wt=hv005/1000000,strat=hv022,
         id=hv021, urban = case_when(hv025== 2~ 0,
                                     hv025== 1~ 1))

dhs <- list(AO_2011_mis, AO_2015_dhs)
AO_2011_mis %>% filter(hv025==1) %>% 
  tabyl(hv001, wealth)


#Kenya
KE_mis_2015 <- dhs[[1]]

print_labels(KE_2015_mis$hv213) #31- 35= finished, same as usual
print_labels(KE_2015_mis$hv214) #finished walls= 31,32,33, 34, 35, usual
print_labels(KE_2020_mis$hv215) #roof, finished= 21, 31, 32, 33, wood not listed as option

KE_2015_mis <- KE_2015_mis %>% 
  mutate(wealth = ifelse(hv270 <4, 0, 1),
         floor_type = ifelse(hv213 >= 98, NA, ifelse(hv213 %in% c(30, 31, 32, 33, 34, 35),1, 0)),
         wall_type = ifelse(hv214 >= 98, NA , ifelse (hv214 %in% c(31, 32, 33, 34, 35),1, 0)),
         roof_type = ifelse(hv215 >= 98, NA, ifelse(hv215 %in% c(21, 31, 32, 33),1, 0)),
         housing_q = ifelse(floor_type == 1 & wall_type == 1 & roof_type == 1,1, 0),
         wt=hv005/1000000,strat=hv022,
         id=hv021, urban = case_when(hv025== 2~ 0,
                                     hv025== 1~ 1))

#Nigeria, 2010
print_labels(NG_2015_mis$hv215)
#hv214= 33 (wood planks/shingles= excluded from improved walls)
NG_2010 <- NG_2010_mis %>% 
  mutate(wealth = ifelse(hv270 <4, 0, 1),
         floor_type = ifelse(hv213 >= 98, NA, ifelse(hv213 %in% c(30, 31, 32, 33, 34, 35, 36, 37),1, 0)),
         wall_type = ifelse(hv214 >= 98, NA , ifelse (hv214 %in% c(30, 31, 32),1, 0)),
         roof_type = ifelse(hv215 >= 98, NA, ifelse(hv215 %in% c(30, 31, 34, 35),1, 0)),
         housing_q = ifelse(floor_type == 1 & wall_type == 1 & roof_type == 1,1, 0), 
         wt=hv005/1000000,strat=hv022,
         id=hv021, num_p=1,
         region = hv024, 
         interview_month = hv006, 
         urban = case_when(hv025== 2~ 0,
                           hv025== 1~ 1))

#Nigeria, 2021
print_labels(NG_2021_mis$hv215) #hv215= 37 (asbestos= good roofing)
NG_2021 <- NG_2021_mis %>% 
  mutate(wealth = ifelse(hv270 <4, 0, 1),
         floor_type = ifelse(hv213 >= 98, NA, ifelse(hv213 %in% c(30, 31, 32, 33, 34, 35, 36, 37),1, 0)),
         wall_type = ifelse(hv214 >= 98, NA , ifelse (hv214 %in% c(30, 31, 32, 33, 34,35, 37, 38),1, 0)),
         roof_type = ifelse(hv215 >= 98, NA, ifelse(hv215 %in% c(30, 31, 33, 34,35, 37),1, 0)),
         housing_q = ifelse(floor_type == 1 & wall_type == 1 & roof_type == 1,1, 0), 
         wt=hv005/1000000,strat=hv022,
         id=hv021, num_p=1,
         region = hv024, 
         interview_month = hv006, 
         urban = case_when(hv025== 2~ 0,
                           hv025== 1~ 1))


## Rwanda- 2019 hv214= 36= Covered adobe which is considered improved walls
#hv214= 31= tree trunks (not improved)
print_labels(RW_2019_dhs$hv214) 
RW_2019_dhs <- RW_2019_dhs %>% 
  mutate(wealth = ifelse(hv270 <4, 0, 1),
         floor_type = ifelse(hv213 >= 98, NA, ifelse(hv213 %in% c(30, 31, 32, 33, 34, 35, 36),1, 0)),
         wall_type = ifelse(hv214 >= 98, NA , ifelse (hv214 %in% c(30, 32, 33, 34, 35, 36),1, 0)),
         roof_type = ifelse(hv215 >= 98, NA, ifelse(hv215 %in% c(30, 31, 32, 33, 34,35),1, 0)),
         housing_q = ifelse(floor_type == 1 & wall_type == 1 & roof_type == 1,1, 0), 
         wt=hv005/1000000,strat=hv022,
         id=hv021,
         urban = case_when(hv025== 2~ 0,
                           hv025== 1~ 1),
         ml_micro_pos= case_when(
           hv103==1 & hml16a %in% c(6:59) & (hml32== 0 | hml32== 6) ~ 0,
           hv103==1 & hml16a %in% c(6:59) & hml32== 1 ~ 1), 
         ml_micro_test= if_else(is.na(ml_micro_pos), 0, 1))



## Madagascar's 2011 & 2013 MIS surveys- missing hv022 and hv023 (strata)
# Solution: can combine hv024 and hv025 per DHS staff member response. 

MD_2011 <- MD_2011_mis %>% group_by(hv024,hv025) %>% 
  dplyr::mutate(strat = cur_group_id())
MD_2013 <- MD_2013_mis %>% group_by(hv024,hv025) %>% 
  dplyr::mutate(strat = cur_group_id())

dhs <- list(MD_2011, MD_2013)

dhs <- dhs %>% map(~mutate(., wealth = ifelse(hv270 <4, 0, 1), floor_type = ifelse(hv213 >= 98, NA, ifelse(hv213 %in% c(30, 31, 32, 33, 34, 35, 36),1, 0)),
                           wall_type = ifelse(hv214 >= 98, NA , ifelse (hv214 %in% c(30, 31, 32, 33, 34,35),1, 0)),
                           roof_type = ifelse(hv215 >= 98, NA, ifelse(hv215 %in% c(30, 31, 33, 34,35),1, 0)),
                           housing_q = ifelse(floor_type == 1 & wall_type == 1 & roof_type == 1,1, 0), 
                           wt=hv005/1000000,
                           id=hv021,
                           urban = case_when(hv025== 2~ 0,
                                             hv025== 1~ 1),
                           ml_micro_pos= case_when(
                             hv103==1 & hml16a %in% c(6:59) & (hml32== 0 | hml32== 6) ~ 0,
                             hv103==1 & hml16a %in% c(6:59) & hml32== 1 ~ 1), 
                           ml_micro_test= if_else(is.na(ml_micro_pos), 0, 1)))

MD_2011 <- dhs[[1]]
MD_2013 <- dhs[[2]]
options(survey.lonely.psu="adjust") 
svydat <- svydesign.fun(MD_2016)
svydat <- svydesign.fun(MD_2011)
#For Madagascar- run by cluster number
tab1 <- svyby(~housing_q, ~Antananarivo_city, svydat, svymean, vartype=c("ci"), na.rm= TRUE) %>% 
  filter(Antananarivo_city== 1) %>% # Update filter for each survey
  mutate(housing_q= housing_q*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab1
MD_2013 %>% tabyl(Antananarivo_city)
#Investigating why modern housing would only be 24% for urban areas in Rwanda in 2019?
RW_2019_dhs <- dhs[[1]]
RW_2019_dhs %>% 
  tabyl(wall_type, hv214)


#Senegal, 2010
SN_2010 <- SN_2010_dhs %>% 
  mutate(wealth = ifelse(hv270 <4, 0, 1), floor_type = ifelse(hv213 >= 98, NA, ifelse(hv213 %in% c(30, 31, 32, 33, 34, 35, 36, 37),1, 0)),
            wall_type = ifelse(hv214 >= 98, NA , ifelse (hv214 %in% c(30, 31, 32, 33, 34,35, 37, 38),1, 0)),
            roof_type = ifelse(hv215 >= 98, NA, ifelse(hv215 %in% c(30, 31, 33, 34,35),1, 0)),
            housing_q = ifelse(floor_type == 1 & wall_type == 1 & roof_type == 1,1, 0), #indicator var for house quality
            wt=hv005/1000000,strat=hv022,
            id=hv021, num_p=1,
            region = hv024, 
            interview_month = hv006, 
            urban = case_when(hv025== 2~ 0,
                              hv025== 1~ 1))

print_labels(SN_2010_dhs$hv215)
#Uganda, 2014
print_labels(UG_2014_mis$hv215) #Finished roof= 31, 33, 34, 35, 37 (asbestos) 



#### Correlations ####
#read in file paths
TableDir <- file.path(NuDir, "data", 'urban_malaria_net_ownership_data', 'Extracted_csv')

pfpr_housing <- read_xlsx(file.path(TableDir, "pfpr_modernHH.xlsx"))

pfpr_H <- pfpr_housing %>% 
  rename(pfPR= `pfPR (urban areas)`, 
         modernHH= `proportion with modern Housing`)

#modern HH- largest urban center only
## Working Example- Benin and Burkina Faso
Dir <- file.path(Drive, "OneDrive/Documents/1. Northwestern Malaria Modeling Group/Urban_nets")
table1 <- read_xlsx(file.path(Dir, "Table1_largestcity.xlsx"))
tab <- table1 %>% 
  dplyr::filter(grepl('BJ|BF', country_year))

plot1 <- ggplot(tab, aes(x= year_survey2, y= PfPR, group= country, color= country)) + 
  geom_point()

plot1 +  
  geom_line()+
  labs(x= "Survey year", y= "Malaria Prevalence by Microscopy (children 6- 59 months)")+
  theme(plot.title = element_text(face= "bold", size= 12, hjust= 0.5, color= "black"))+
  theme_classic()


#Test correlation using Kendall Rank correlation (non-parametic test)
pfpr_H <- pfpr_H %>% 
  filter(!is.na(pfPR) & !is.na(modernHH)) #Madagascar removed because missing PfPR


#Kendall's tau- 
#a tau of 0.35 or higher is a strong correlation. 
cor.test(pfpr_H$pfPR, pfpr_H$modernHH, method= "kendall") #tau= -0.267, p-value= 0.003

### Scatterplot (linear reg) for PfPR by country (urban areas only)

plot1 <- ggplot(pfpr_H, aes(x= modernHH, y= pfPR)) + 
  geom_point()

plot1 +  
  geom_smooth(method = loess)+
  labs(x= "Proportion of population with modern housing", y= "Malaria Prevalence by Microscopy (children 6- 59 months)")+
  theme(plot.title = element_text(face= "bold", size= 12, hjust= 0.5, color= "black"))+
  theme_classic()

#Plot of proportion of urban Areas with modern housing by country and survey year
plot_housing <- ggplot(pfpr_H, aes(x= year_survey2, y= modernHH, group= country, color= country))

plot_housing +
  geom_line()+ geom_point()+
  theme_minimal()+
  scale_x_continuous(breaks= c(2010, 2012, 2014, 2016, 2018, 2020))+
  labs(x= "Survey year", y= "Proportion of population with modern housing (%)",
       title= "Proportion of urban population with modern housing")


#### Functions  ####

#survey design function 
svydesign.fun <- function(filename){
  svydesign(id= ~id,
            strata=~strat,nest=T, 
            weights= ~wt, data=filename)
}
#survey estimates generating functions  
result.mean<- function(var, var1, design) {
  p_est<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svymean, design, vartype= "ci", na.rm=T, influence= TRUE)
}

estim_mean <- function(df, col, by){
  svy_mal <- svydesign.fun(df)
  design <- subset(svy_mal, hv025== 1) #subsetting to urban areas only
  clu_est <- result.mean(col, by, design) 
}


#transform cluster proportions
transform_cluster_prop <- function(df,urb){
    df2 <- df %>% 
    filter(urban== urb) %>% 
    tabyl(id, housing_q, show_na= F) %>% 
    adorn_percentages("row")
    df2[,-2] 
  }


