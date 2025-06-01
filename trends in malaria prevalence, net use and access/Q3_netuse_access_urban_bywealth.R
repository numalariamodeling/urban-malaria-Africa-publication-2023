### ITN access and use by wealth quintile in all urban areas

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
library(ggpubr)

#Read in file paths
user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "NU-malaria-team Dropbox")
data_dir <- file.path(NuDir, "data", 'urban_malaria_net_ownership_data', 'Extracted_csv')



#### Functions ####
#survey design function 
svydesign.fun <- function(filename){
  svydesign(id= ~id,
            strata=~strat,nest=T, 
            weights= ~wt, data=filename)
}


###### Calculate Net Access by wealth quintile for urban areas ######

dhs <- list(AO_2011_mis_hr, AO_2015_dhs_hr) #Update for each country
dhs <- list(CM_2011_dhs_hr, CM_2018_dhs_hr, CD_2013_dhs_hr, CI_2012_dhs_hr, CI_2021_dhs_hr)
dhs <- list(KE_2015_mis_hr, KE_2020_mis_hr, LB_2011_mis_hr, LB_2016_mis_hr)
dhs <- list(MD_2016_mis_hr, MD_2021_dhs_hr)
dhs <- list(MD_2011, MD_2013)
dhs <- list(MW_2012_mis_hr, MW_2014_mis_hr, MW_2017_mis_hr)
dhs <- list(ML_2012_dhs_hr, ML_2015_mis_hr, ML_2018_dhs_hr, ML_2021_dhs_hr)
dhs <- list(MR_2020_dhs_hr)
dhs <- list(MZ_2011_dhs_hr, MZ_2015_dhs_hr, MZ_2018_dhs_hr)
dhs <- list(NG_2010_mis_hr, NG_2015_mis_hr, NG_2018_dhs_hr, NG_2021_mis_hr)
dhs <- list(RW_2010_mis_hr,RW_2015_dhs_hr, RW_2017_mis_hr, RW_2019_dhs_hr)

dhs <- list(SN_2010_dhs_hr, SN_2012_dhs_hr, SN_2014_dhs_hr, SN_2015_dhs_hr, SN_2016_dhs_hr, SN_2017_dhs_hr, SN_2020_mis_hr)
dhs <- list(TZ_2012_dhs_hr, TZ_2015_dhs_hr, TZ_2017_mis_hr)
dhs <- list(TG_2013_dhs_hr, TG_2017_mis_hr)
dhs <- list(UG_2014_mis_hr, UG_2016_dhs_hr, UG_2018_mis_hr)

dhs <- dhs %>% 
   map(~mutate(., wt=hv005/1000000,
                           strat=hv022,
                           id=hv021, num_p=1,
                           potuse = 2 * rowSums(dplyr::select(., contains('hml10')),na.rm=T),
                           slept_night = rowSums(dplyr::select(., contains('hv103')), na.rm=T), 
                           potuse2 = ifelse(potuse/slept_night > 1, slept_night, potuse),
                           access = potuse2/slept_night,
                           # ml_rdt_pos= case_when( #Positive for malaria by RDT in children 6-59 months
                           #   hv042== 1 & hv103==1 & hc1 %in% c(6:59) & hml35== 0 ~ 0,
                           #   hv042== 1 & hv103==1 & hc1 %in% c(6:59) & hml35== 1 ~ 1),
                           # ml_rdt_test= if_else(is.na(ml_rdt_pos), 0, 1),
                           wealth = ifelse(hv270 <4, 0, 1)))


UG1 <- dhs[[1]]
UG2 <- dhs[[2]]
UG3 <- dhs[[3]]

SN_10 <- dhs[[1]]
SN_12 <- dhs[[2]]
SN_14 <- dhs[[3]]
SN_15 <- dhs[[4]]
SN_16 <- dhs[[5]]
SN_17 <- dhs[[6]]
SN_20 <- dhs[[7]]


options(survey.lonely.psu="adjust") 
svydat <- svydesign.fun(UG3) # Re-do for each survey

## Net Access by wealth (4th and 5th wealthiest quintiles vs. lower)
tab1 <- svyby(~access, ~(wealth + hv025), design= subset(svydat, hv025==1), svymean, vartype=c("se", "ci"), na.rm= TRUE)
tab1

## Overall- by urban/ rural
svyby(~access, ~hv025, design=svydat, svymean, vartype=c("se", "ci"), na.rm= TRUE)

# Save in file ur_ITN_access_wealth.xlsx


#### Calculate Net Use by wealth quintile for urban areas ######
## Use the PR files ##

dhs <- list(TG_2013_dhs, TG_2017_mis, UG_2014_mis, UG_2016_dhs, UG_2018_mis
            ) #Update for each country

print_labels(CM_2011_dhs$hml12)

dhs1 <- dhs %>% 
  map(~dplyr::filter(., hv103 == 1)) %>%  #individuals who slept in the HH night before
  map(~dplyr::select(., hhid, hv001, hml12, hv005, 
                     hv103, hv013, hv021, hv022, hv023, hv024, hv025, hv103, hv270)) %>% 
  map(~mutate(., 
              net_use =ifelse(hml12 %in% c(1,2), 1, 0), #slept under an ITN the night before
              wt=hv005/1000000,
              strat=hv022,
              id=hv021,
              wealth = ifelse(hv270 <4, 0, 1)
              ))

TG1 <- dhs1[[1]]
TG2 <- dhs1[[2]]
UG1 <- dhs1[[3]]

UG2 <- dhs1[[4]]
UG3 <- dhs1[[5]]


rm(RW_2010_mis, RW_2015_dhs, RW_2017_mis, RW_2019_dhs)

options(survey.lonely.psu="adjust") 
svydat <- svydesign.fun(UG3) # Re-do for each survey

## Net USE by wealth (4th and 5th wealthiest quintiles vs. lower)
tab1 <- svyby(~net_use, ~(wealth + hv025), design= subset(svydat, hv025==1), svymean, vartype=c("se", "ci"), na.rm= TRUE)
tab1

# Save in file ur_ITN_use_wealth.xlsx




## COUNRIES WITH OTHER CONSIDERATIONS FALLING OUTSIDE THE ABOVE CODE ##

# MADAGASCAR- 2011 & 2013 MISSING STRATA- can combine hv024 and hv025 to create strat
MD_2011 <- MD_2011_mis_hr %>% group_by(hv024,hv025) %>% 
  mutate(strat = cur_group_id()) %>% 
  ungroup()

MD_2013 <- MD_2013_mis_hr %>% group_by(hv024,hv025) %>% 
  mutate(strat = cur_group_id()) %>% 
  ungroup()

dhs <- list(MD_2011, MD_2013)

dhs <- dhs %>% 
  map(~mutate(., wt=hv005/1000000,
              #strat=hv022,
              id=hv021, num_p=1,
              potuse = 2 * rowSums(dplyr::select(., contains('hml10')),na.rm=T),
              slept_night = rowSums(dplyr::select(., contains('hv103')), na.rm=T), 
              potuse2 = ifelse(potuse/slept_night > 1, slept_night, potuse),
              access = potuse2/slept_night,
              wealth = ifelse(hv270 <4, 0, 1)))
MD_2011 <- dhs[[1]]
MD_2013 <- dhs[[2]]

svydat <- svydesign.fun(MD_2011) 
svydat <- svydesign.fun(MD_2013) 

#For Madagascar- 2011 and 2013
svyby(~access, ~(wealth + hv025), design= subset(svydat, hv025==1), svymean, vartype=c("se", "ci"), na.rm= TRUE)

# Net use
MD_2011 <- MD_2011_mis %>% group_by(hv024,hv025) %>% 
  mutate(strat = cur_group_id()) %>% 
  ungroup()

MD_2013 <- MD_2013_mis %>% group_by(hv024,hv025) %>% 
  mutate(strat = cur_group_id()) %>% 
  ungroup()

dhs <- list(MD_2011, MD_2013)

##### Plot the graphs of ITN use and access by Country by wealth #####

#Change variables between ITN Access and use
fill.funw <- function(dat){
  dat %>% arrange(country, year_survey2) %>% 
    mutate(use_dfill = na.locf0(use_weal, fromLast = TRUE)) %>%
    mutate(use_ufill = zoo::na.locf0(use_weal, fromLast = FALSE)) %>%
    mutate(use_lifill = na.approx(use_weal, na.rm = FALSE)) %>%
    mutate(name_text = paste(country, year_survey2, sep=" ")) %>%
    mutate(final_interp_use= use_lifill) %>% 
    filter(year_survey2 >= 2010) %>% 
    mutate(nas = sum(is.na(use_weal))) 
}

fill.fun_nw <- function(dat){
  dat %>% arrange(country, year_survey2) %>% 
    mutate(use_dfill = na.locf0(use_nw, fromLast = TRUE)) %>%
    mutate(use_ufill = zoo::na.locf0(use_nw, fromLast = FALSE)) %>%
    mutate(use_lifill = na.approx(use_nw, na.rm = FALSE)) %>%
    mutate(name_text = paste(country, year_survey2, sep=" ")) %>%
    mutate(final_interp_use= use_lifill) %>% 
    filter(year_survey2 >= 2010) %>% 
    mutate(nas = sum(is.na(use_nw))) 
}

# Read in final ITN access in urban areas by Wealth quintile dataset

access_ur_wealth <- read_excel(file.path(data_dir,"ur_ITN_access_wealth.xlsx"))
ITN_use_ur_wealth <- read_excel(file.path(data_dir,"ur_ITN_use_wealth.xlsx"))

#splitting the df
df_w_list <- split(access_ur_wealth, with(access_ur_wealth, interaction(country)), drop = TRUE)

# ITN access by wealth
cntry_interp_list <- lapply(df_w_list, fill.funw)
cntry_interp_list2 <- lapply(df_w_list, fill.fun_nw)

#Urban ITN access- wealthy
final_ur_wealthy_df <- bind_rows(cntry_interp_list) %>% 
  mutate(final_access_ur_weal= final_interp_access) %>% 
  select(country, year_survey2, SubregionName, SurveyName, Indicator, 
         name_text, nas, access_weal, final_access_ur_weal)

#Urban ITN access- not wealthy category
final_ur_nwealth_df <- bind_rows(cntry_interp_list2) %>% 
  mutate(final_access_ur_nw= final_interp_access) %>% 
  select(country, year_survey2, SubregionName, SurveyName, Indicator, 
         name_text, nas, access_nw, final_access_ur_nw)

df_wealthq <- left_join(final_ur_wealthy_df, final_ur_nwealth_df, by= c("country", "year_survey2")) %>% 
  select(country, year_survey2, SubregionName.x, SurveyName.x, name_text.x, access_weal, final_access_ur_weal,
         access_nw, final_access_ur_nw)

# Save csv
write.csv(df_wealthq, file.path(data_dir, "ur_access_wealth_interp.csv")) #final ITN access in urban areas by wealth quintile (wealthy (4/5) vs. not)

# ITN use
df_w_list <- split(ITN_use_ur_wealth, with(ITN_use_ur_wealth, interaction(country)), drop = TRUE)

# ITN access by wealth
cntry_interp_list <- lapply(df_w_list, fill.funw)
cntry_interp_list2 <- lapply(df_w_list, fill.fun_nw)

#Urban ITN use- wealthy
final_ur_wealthy_df <- bind_rows(cntry_interp_list) %>% 
  mutate(final_use_ur_weal= final_interp_use) %>% 
  select(country, year_survey2, SubregionName, SurveyName, Indicator, 
         name_text, nas, use_weal, final_use_ur_weal)

#Urban ITN use- not wealthy category
final_ur_nwealth_df <- bind_rows(cntry_interp_list2) %>% 
  mutate(final_use_ur_nw= final_interp_use) %>% 
  select(country, year_survey2, SubregionName, SurveyName, Indicator, 
         name_text, nas, use_nw, final_use_ur_nw)

df_wealthq_use <- left_join(final_ur_wealthy_df, final_ur_nwealth_df, by= c("country", "year_survey2")) %>% 
  select(country, year_survey2, SubregionName.x, SurveyName.x, name_text.x, use_weal, final_use_ur_weal,
         use_nw, final_use_ur_nw)

# Save csv
write.csv(df_wealthq_use, file.path(data_dir, "ur_use_wealth_interp.csv")) #final ITN Use in urban areas by wealth quintile (wealthy (4/5) vs. not)


# Create Plots

#Read in dataframes if needed
df_wealthq_use <- read.csv(file.path(data_dir, "ur_use_wealth_interp.csv"))
df_wealthq_access <- read.csv(file.path(data_dir, "ur_access_wealth_interp.csv"))

country_w_list <- split(df_wealthq_access[], with(df_wealthq_access[], interaction(country)), drop = TRUE) 


# plots by wealth category
access_trend_w.fun <- function(df){
  ggplot(df) +
    geom_line(aes(x = year_survey2, y= final_access_ur_nw), color = "#ef8a62") +
    geom_point(aes(x = year_survey2, y= access_nw), color = "#ef8a62", size = 1) +
    
    geom_line(aes(x = year_survey2, y= final_access_ur_weal), color = "royalblue1") +
    geom_point(aes(x = year_survey2, y= access_weal), color = "royalblue1", size = 1) +
    facet_wrap(~country, strip.position = "top", scales= "free") +
    theme_minimal()+
    ylab(" ") + xlab(" ") +
    #ylim(0, 0.25)+
    xlim(2010, 2022)+
    theme(axis.text.x = element_text(angle = 45, hjust= 0.9), legend.title= element_blank(),
          legend.position = c(0.9, 0))+
    
    scale_x_continuous(breaks= pretty_breaks())+
    expand_limits(y= 0.05)+
    scale_y_continuous(limits= c(0, NA), labels=label_number(accuracy= 0.01))
}

itn_annotate.fun <- function(plots_unannoted) {
  to_annotate <- ggarrange(plotlist=plots_unannoted)
  annotate_figure(to_annotate, bottom= text_grob("Year", color = "black", size = 14),
                  right = c(text_grob("Wealthy households", color= "royalblue1", hjust= 1,vjust= 0.2, size=11),
                            text_grob("Not wealthy households", color= "#ef8a62", hjust= 1,vjust= 0.1, size=11)),
                  left = text_grob("ITN access", color = "black", size = 14,  rot = 90)
  )
}


plot_use_all <- lapply(country_w_list, use_trend_w.fun)
plot_access_all <- lapply(country_w_list, access_trend_w.fun)

itn_annotate.fun(plot_use_all) # ITN use in Urban areas by wealth quintile
itn_annotate.fun(plot_access_all) # ITN access in Urban areas by wealth quintile

