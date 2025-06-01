### Urban Nets Q3- Proportion of ITNs per wealth quintile in Urban areas  ###
#Author: Colleen Leonard
#Date last edited: 3/06/2024

library(readxl)
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)
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
Out <- file.path(data_dir, "Objective_3", "ITNs_by_wealthq")


#FUNCTIONS FOR HERE

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


result.prop<- function(var, var1, design) {
  p_est<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svymean, design, svyciprop, method ='logit', levels=0.95, vartype= "se", na.rm=T, influence = TRUE)
}

result.mean<- function(var, var1, design) {
  p_est<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svymean, design, vartype= "ci", na.rm=T, influence = TRUE)
}
result.total <- function(var, var1, design) {
  p_est<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svytotal, design, na.rm=T)
}

estim_total <- function(df, col, by){
  svy_mal <- svydesign.fun(df)
  design <- subset(svy_mal, hv025== 1 ) #subsetting to urban areas only 
  clu_est <- result.total(col, by, design)
}

## -------------------------------------------------------------------------
### Read in HR data and compute ITN variables   
## -------------------------------------------------------------------------
# read in data 
source("read_DHS_data.R")

dhs <- list(AO_2011_mis_hr, AO_2015_dhs_hr)
dhs <- list(BJ_2012_dhs_hr, BJ_2017_dhs_hr, BF_2010_dhs_hr, BF_2014_dhs_hr, BF_2017_dhs_hr, BF_2021_dhs_hr, BU_2012_mis_hr, BU_2016_dhs_hr,
            CD_2013_dhs_hr, CI_2012_dhs_hr, CI_2021_dhs_hr, CM_2011_dhs_hr, CM_2018_dhs_hr) 
#DONE

dhs <- list(GH_2014_dhs_hr, GH_2016_mis_hr, GH_2019_mis_hr, GM_2013_dhs_hr, GM_2019_dhs_hr, GN_2012_dhs_hr, 
            GN_2021_mis_hr, KE_2015_mis_hr, KE_2020_mis_hr, LB_2011_mis_hr, LB_2016_mis_hr) 

dhs <- list(ML_2012_dhs_hr, ML_2015_mis_hr, ML_2018_dhs_hr, ML_2021_dhs_hr, MR_2020_dhs_hr, 
            MW_2012_mis_hr, MW_2014_mis_hr, MW_2017_mis_hr, MZ_2011_dhs_hr, MZ_2015_dhs_hr, MZ_2018_dhs_hr,
            NG_2010_mis_hr, NG_2015_mis_hr, NG_2018_dhs_hr, NG_2021_mis_hr, 
            RW_2010_mis_hr, RW_2015_dhs_hr, RW_2017_mis_hr, RW_2019_dhs_hr, SN_2010_dhs_hr, 
            SN_2012_dhs_hr, SN_2014_dhs_hr, SN_2015_dhs_hr, SN_2016_dhs_hr, SN_2017_dhs_hr, SN_2020_mis_hr)

dhs <- list(TZ_2012_dhs_hr, TZ_2015_dhs_hr, TZ_2017_mis_hr, TG_2013_dhs_hr, TG_2017_mis_hr, 
            UG_2014_mis_hr, UG_2016_dhs_hr, UG_2018_mis_hr)

dhs <- list(MD_2016_mis_hr, MD_2021_dhs_hr)

options(survey.lonely.psu="adjust") 

#computes household-level number of nets 
dhs1 <- dhs %>% 
  map(~dplyr::select(., hhid, hv001, hv015, starts_with('hml10'), hv005, hv023, hv024, hv025, country_year,
                     starts_with('hv103'), hv013, hv021, hv022, hv270)) %>% 
  map(~mutate(., sum_itns = rowSums(dplyr::select(., contains('hml10')),na.rm=T),
              potuse = 2 * rowSums(dplyr::select(., contains('hml10')),na.rm=T),
              slept_night = rowSums(dplyr::select(., contains('hv103')), na.rm=T), 
              potuse2 = ifelse(potuse/slept_night > 1, slept_night, potuse),
              access = potuse2/slept_night,
              wealth = ifelse(hv270 <4, 0, 1),
              wt=hv005/1000000, 
              strat=hv022,
              id=hv021)) %>% 
  map(~dplyr::select(., hhid, hv001, hv015, hv023, hv024, hv025, hv270, country_year, access, wealth, sum_itns, wt, strat, id))

# ADD MADAGASCAR- NEED to recalculate Strata
dhs <- list(MD_2011_mis_hr, MD_2013_mis_hr) 

# MADAGASCAR- 2011 & 2013 MISSING STRATA- can combine hv024 and hv025 to create strat
dhs1 <- dhs %>% 
  map(~dplyr::select(., hhid, hv001, hv015, starts_with('hml10'), hv005, hv024, hv025, country_year, 
                     starts_with('hv103'), hv013, hv021, hv022, hv270)) %>% 
  map(~mutate(., sum_itns = rowSums(dplyr::select(., contains('hml10')),na.rm=T), 
              potuse = 2 * rowSums(dplyr::select(., contains('hml10')),na.rm=T),
              slept_night = rowSums(dplyr::select(., contains('hv103')), na.rm=T), 
              potuse2 = ifelse(potuse/slept_night > 1, slept_night, potuse),
              access = potuse2/slept_night,
              wt=hv005/1000000, 
              #strat=hv022,
              id=hv021))

MD_2011 <- dhs1[[1]] %>% group_by(hv024,hv025) %>% 
  dplyr::mutate(strat = cur_group_id())
MD_2013 <- dhs1[[2]] %>% group_by(hv024,hv025) %>% 
  dplyr::mutate(strat = cur_group_id())

dhs1 <- list(MD_2011, MD_2013)

#df= dhs1
#col= variable (sum_itns or hv015= 1 for every HH- completed interview)
#by= wealthquintile (hv270) or "wealth"

#Average number of nets by wealth quintile 
vars <- c('sum_itns')
for (i in 1:length(vars)) {
  col <- list(vars[i])
  by <- list('hv270')
  df <- dhs1 %>% 
    map(~drop_na(.,vars[i]))
  df <-  pmap(list(df,col,by), estim_mean)
  df_itns <- plyr::ldply(df)
  write.csv(df_itns, file =file.path(Out, "avgITNS_wealth_AO11_DHS_3.03.23.csv"))
}


# Merge files of average ITNs by wealth quintile

#Read in Number of children positive for Pf. by RDT
setwd(Out)
df_itns <- list.files(path = Out, pattern = glob2rx("*.csv$")) %>% 
  map(~read_csv(.))

df_itn <- plyr::ldply(df_itns) %>% 
  dplyr::rename("avg_itns"= "sum_itns")

df_itns_byw <- df_itn %>% 
  group_by(country_year) %>% 
  dplyr::mutate(prop_itnbywealth= avg_itns/ sum(avg_itns)) %>% 
  select(-c(ci_l, ci_u))

#write.csv(df_itns_byw, file =file.path(Out, "ITNs_bywealth_3.06.24.csv"))

df_itns_byw <- read.csv(file.path(Out, "ITNs_bywealth_3.06.24.csv")) %>% 
  mutate(country= if_else(country== "Democratic Republic of the Congo", "Congo Democratic Republic", country))


#Read in number of ITNs distributed per year (in urban areas= final_ur_nets)
options(scipen= 999)
itn_distro <- read.csv(file.path(data_dir, "final_interp_df.csv")) %>% 
  dplyr::select(country, year_survey2, name_text, SubregionName, final_ur_nets) %>% 
  dplyr::filter(year_survey2 >2010)

#Only keep the countries in df_itns_byw
country_list <- df_itns_byw[[3]]
itn_distr <- itn_distro %>% 
  dplyr::filter(country %in% country_list)


# Replicate each row 5 times (original row + 4 additional rows) to allow room for wealth quintile variable
expanded_df <- itn_distr %>%
  group_by(country, year_survey2) %>%
  slice(rep(1:n(), each = 5)) %>%
  ungroup()  
  
expanded_df2 <- expanded_df %>% 
  mutate(hv270= rep(c(1,2,3,4,5), times= nrow(expanded_df)/5))

itns_by_wealth <- expanded_df2 %>% 
  left_join(df_itns_byw, by= c("country"= "country", "year_survey2"= "year", "hv270"))


#### LINEAR INTERPOLATION OF PROPORTION OF NETS PER WEALTH Q IN URBAN AREAS ####
#spliting df
itns_country_list <- split(itns_by_wealth, with(itns_by_wealth, interaction(country)), drop = TRUE)

fill.function <- function(dat){
  dat %>% arrange(hv270, name_text) %>% mutate(itn_p_dfill = na.locf0(prop_itnbywealth)) %>% 
     mutate(itn_p_ufill = na.locf0(prop_itnbywealth, fromLast = TRUE)) %>%
     mutate(itn_p_lifill = na.approx(prop_itnbywealth, na.rm = FALSE))
}

interp_list <- lapply(itns_country_list, fill.function)

#Approx. Number of nets by wealth quintile

final_interp_dfw <- bind_rows(interp_list) %>% 
  mutate(final_interp_prop = ifelse(is.na(itn_p_lifill), itn_p_dfill, itn_p_lifill)) %>% #if missing, fill with value from prior yrs
  mutate(final_interp_prop = ifelse(is.na(final_interp_prop), itn_p_ufill, final_interp_prop)) %>% #if missing, fill with value from future yrs
  mutate(ur_nets_w= final_interp_prop*final_ur_nets)

write.csv(final_interp_dfw, file =file.path(Out, "ITNs_bywealth_allyears_3.10.24.csv"))

summary_data <- final_interp_dfw %>% 
  dplyr::group_by(hv270) %>% 
  summarize(total_itns_byw= round(sum(ur_nets_w)/1000000, 1))

summary_data <- summary_data %>% 
  mutate(wealth= c("Lowest", "Second", "Middle", "Fourth", "Highest"))

# Convert wealth to factor and specify the level order
summary_data$wealth <- factor(summary_data$wealth, levels = c("Lowest", "Second", "Middle", "Fourth", "Highest"))

##Plot summary data- number of ITNs distributed per wealth quintile in Sub-Saharan Africa 2011- 2021

ggplot(data= summary_data, aes(wealth, total_itns_byw, fill= wealth))+
  geom_col()+
  geom_text(aes(label= total_itns_byw), vjust=3, color= "gray3", size= 5)+
  scale_fill_manual(values= c("steelblue", "steelblue", "steelblue", "steelblue", "steelblue"))+
  labs(x= "Weatlh quintile", y= "", title= "ITNs distributed in urban areas by wealth quintile 2011- 2021, millions")+
  theme_minimal()+
  theme(legend.position= "none")


#Read in distribution of nets and urban slum population file

ur_nets_pop <- read_excel(file.path(data_dir, "ur_nets_pop.xlsx")) %>% 
  mutate(Prop_covered2= round(Prop_covered2*100,1)) %>% 
  filter(year >= 2011)

#Net coverage for urban slum population- 2011- 2021
ggplot(data= ur_nets_pop, aes(factor(year), Prop_covered2))+
  geom_col()+
  #geom_text(aes(label= Prop_covered2), vjust=3, color= "black", size= 5)+
  scale_y_continuous(limits= c(0,118), labels = label_percent(scale = 1))+
  labs(x= "Year", y= "", title= "ITN Coverage if all ITNs in urban areas were distributed to the population living in urban slums \n in sub-Saharan Africa 2011- 2021")+
  theme_minimal()+
  theme(legend.position= "none")

