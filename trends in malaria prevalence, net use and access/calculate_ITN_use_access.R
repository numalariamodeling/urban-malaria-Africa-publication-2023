# Calculate ITN use and access per cluster

# -----------------------------------------
### Required functions and settings
## -----------------------------------------
#Read in file paths
user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "NU-malaria-team Dropbox", "data", 'urban_malaria_net_ownership_data')
DataDir <- file.path(NuDir,  'DHS')
MISDataDir <- file.path(NuDir,'MIS')

Out <- file.path(NuDir,'Extracted_csv/Objective_3')

library(tidyverse)
library(survey)
library(haven)
library(dplyr)

source("data_extractor_functions.R")
options(survey.lonely.psu="adjust") # this option allows admin units with only one cluster to be analyzed

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

estim_prop <- function(df, col, by){
  svy_mal <- svydesign.fun(df)
  design <- subset(svy_mal, hv025== 1 ) #subsetting to urban areas only 
  clu_est <- result.prop(col, by, design)
}

cdf_hist = function(df, fill,color, x, xlab, bins){
  hist=ggplot(df, aes(x =.data[[x]]))+geom_histogram(alpha = 0.4, position="identity", bins=bins)
  max_y=max(ggplot_build(hist)$data[[1]]$count)
  ggplot(df, aes(.data[[x]]))+
    geom_histogram(fill=fill, color= color, alpha = 0.4, position="identity", bins = bins) +
    stat_ecdf(aes_(y =bquote(..y..* .(max_y)), color =color))+
    scale_y_continuous(name= 'Count', sec.axis=sec_axis(trans = ~./max_y, name = 'Cumulative percent', labels = function(x) format(x *100, digits=2, nsmall=0)))+
    theme_manuscript()+theme(legend.position = 'none')+
    xlab(xlab)
}

hist = function(df, fill,color, x, xlab, bins){
  hist=ggplot(df, aes(x =.data[[x]]))+ geom_histogram(alpha = 0.8, position="identity", bins=bins)
  max_y=max(ggplot_build(hist)$data[[1]]$count)
  ggplot(df, aes(.data[[x]]))+
    geom_histogram(fill=fill, color= color, alpha = 0.8, position="identity", bins = bins) +
    #stat_ecdf(aes_(y =bquote(..y..* .(max_y)), color =color))+
    scale_y_continuous(name= 'Count')+
    theme_manuscript()+theme(legend.position = 'none')+
    xlab(xlab)
}

theme_manuscript <- function(){
  theme_bw() + 
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 12, color = "black"), 
          axis.text.y = element_text(size = 12, color = "black"),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size =12),
          legend.title=element_text(size=12, colour = 'black'),
          legend.text =element_text(size = 12, colour = 'black'),
          legend.key.height = unit(1, "cm"))
}


## -------------------------------------------------------------------------
### Read in HR and PR data and compute ITN variables   
## -------------------------------------------------------------------------
# read in data 
source("read_DHS_data.R")
#Nigeria- DONE 
dhs <- list(NG_2010_mis_hr, NG_2015_mis_hr, NG_2018_dhs_hr, NG_2021_mis_hr) #Update list
dhs <- list(AO_2011_mis_hr, AO_2015_dhs_hr, BJ_2012_dhs_hr, BJ_2017_dhs_hr)
dhs <- list(BF_2010_dhs_hr, BF_2014_dhs_hr, BF_2017_dhs_hr, BU_2012_mis_hr, BU_2016_dhs_hr) 

dhs <- list(GM_2013_dhs_hr, GM_2019_dhs_hr)
dhs <- list(GH_2014_dhs_hr, GH_2016_mis_hr, GH_2019_mis_hr, GN_2012_dhs_hr, GN_2021_mis_hr) 

dhs <- list(KE_2015_mis_hr, KE_2020_mis_hr, LB_2011_mis_hr, LB_2016_mis_hr, MD_2021_dhs_hr)
dhs <- list(MD_2016_mis_hr)
dhs <- list(MW_2017_mis_hr, MW_2012_mis_hr) #done

dhs <- list(MW_2014_mis_hr, ML_2012_dhs_hr, ML_2015_mis_hr, ML_2018_dhs_hr, ML_2021_dhs_hr, MZ_2011_dhs_hr, MZ_2015_dhs_hr, MZ_2018_dhs_hr)
dhs <- list(NG_2021_mis_hr, NG_2018_dhs_hr, NG_2015_mis_hr)
dhs <- list(NG_2010_mis_hr)

dhs <- list(RW_2010_mis_hr, RW_2015_dhs_hr, RW_2017_mis_hr, RW_2019_dhs_hr)
dhs <- list(SN_2010_dhs_hr, SN_2012_dhs_hr, SN_2014_dhs_hr, SN_2015_dhs_hr, SN_2016_dhs_hr, SN_2017_dhs_hr, SN_2020_mis_hr) 
dhs <- list(SN_2010_dhs_hr) #DONE!

dhs <- list(TZ_2012_dhs_hr, TZ_2015_dhs_hr, TZ_2017_mis_hr, TG_2013_dhs_hr, TG_2017_mis_hr)
dhs <- list(UG_2014_mis_hr, UG_2016_dhs_hr, UG_2018_mis_hr)

dhs <- list(CD_2013_dhs_hr, CI_2012_dhs_hr, CI_2021_dhs_hr)
dhs <- list(BU_2012_mis_hr, BU_2016_dhs_hr, CM_2011_dhs_hr, CM_2018_dhs_hr) 
dhs <- list(BF_2021_dhs_hr)
dhs <- list(MR_2020_dhs_hr)

options(survey.lonely.psu="adjust") 

#computes household-level access 
dhs1 <- dhs %>% 
  map(~dplyr::select(., hhid, hv001, starts_with('hml10'), hv005, hv023, hv024, hv025, country_year,
                     starts_with('hv103'), hv013, hv021, hv022)) %>% 
  map(~mutate(., potuse = 2 * rowSums(dplyr::select(., contains('hml10')),na.rm=T),
              slept_night = rowSums(dplyr::select(., contains('hv103')), na.rm=T), 
              potuse2 = ifelse(potuse/slept_night > 1, slept_night, potuse),
              access = potuse2/slept_night,
              wt=hv005/1000000, 
              strat=hv022,
              id=hv021)) %>% 
  map(~dplyr::select(., hv001, hv023, hv024, hv025, country_year, access, wt, strat, id))

#generate mean access proportion by cluster  
vars <- c('access')
data_dir <- "C:/Users/Colleen/NU-malaria-team Dropbox/data/Urban_malaria_net_ownership_data/Extracted_csv"

Out <- file.path(data_dir, "Objective_3")

for (i in 1:length(vars)) {
  col <- list(vars[i])
  by <- list('hv001')
  df <- dhs1 %>% 
    map(~drop_na(.,vars[i]))
  df <-  pmap(list(df,col,by), estim_mean)
  df_access <- plyr::ldply(df)
  write.csv(df_access, file =file.path(Out, paste0(vars[i], "_MR_2020_2_15_24.csv")))
}

#Average net access per survey--- within Largest Urban Center

NG_2010 <- dhs1[[1]]
NG_2018 <- dhs1[[2]]
NG_2015 <- dhs1[[3]]

BF_2021 <- dhs1[[1]]

svydat <- svydesign.fun(BF_2021) # Re-do for each survey

tab1 <- svyby(~access, ~(hv023 + hv025), design= subset(svydat, hv025==1), svymean, vartype=c("se", "ci"), na.rm= TRUE)  %>% 
  filter(hv023== 5) %>%   # Update filter for each survey
  mutate(access= access*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab1

#Average net access per survey--- All urban areas minus Largest Urban Center

tab2 <- svyby(~access, ~hv025, design= subset(svydat, hv023 !=5), svymean, vartype=c("se", "ci"), na.rm= TRUE) %>% 
  filter(hv025==1) %>%
  mutate(access= access*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab2

#Average net access per survey--- All Urban vs. All Rural areas
tab3 <- svyby(~access, ~hv025, design= svydat, svymean, vartype=c("se", "ci"), na.rm= TRUE)  %>% 
  mutate(access= access*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab3


# MADAGASCAR- 2011 & 2013 MISSING STRATA- can combine hv024 and hv025 to create strat
dhs1 <- dhs %>% 
  map(~dplyr::select(., hhid, hv001, starts_with('hml10'), hv005, hv024, hv025, country_year, Antananarivo_city, 
                     starts_with('hv103'), hv013, hv021, hv022)) %>% 
  map(~mutate(., potuse = 2 * rowSums(dplyr::select(., contains('hml10')),na.rm=T),
              slept_night = rowSums(dplyr::select(., contains('hv103')), na.rm=T), 
              potuse2 = ifelse(potuse/slept_night > 1, slept_night, potuse),
              access = potuse2/slept_night,
              wt=hv005/1000000, 
              #strat=hv022,
              id=hv021))

MD_2011 <- dhs1[[1]] %>% group_by(hv024,hv025) %>% 
  mutate(strat = cur_group_id())
MD_2013 <- dhs1[[2]] %>% group_by(hv024,hv025) %>% 
  mutate(strat = cur_group_id())

dhs1 <- list(MD_2011, MD_2013) #re-run above code to produce csv file

tab1 <- svyby(~access, ~(Antananarivo_city +hv025), design= svydat, svymean, vartype=c("se", "ci"), na.rm= TRUE)  %>% 
  filter(Antananarivo_city== 1) %>%   # Update filter for each survey
  mutate(access= access*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab1
tab2 <- svyby(~access, ~hv025, design= subset(svydat, Antananarivo_city !=1), svymean, vartype=c("se", "ci"), na.rm= TRUE) %>% 
  filter(hv025==1) %>%
  mutate(access= access*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab2


# Use the PR files- Compute ITN use 

#create dataset for computing ITN use 
dhs <- list(NG_2010_mis, NG_2015_mis, NG_2018_dhs, NG_2021_mis) 
dhs <- list(AO_2011_mis, AO_2015_dhs, BJ_2012_dhs, BJ_2017_dhs) 
dhs <- list(BF_2010_dhs, BF_2014_dhs, BF_2017_dhs, BU_2012_mis, BU_2016_dhs)
dhs <- list(CM_2018_dhs, CM_2011_dhs, CD_2013_dhs, CI_2012_dhs)
dhs <- list(CI_2021_dhs) #DONE

dhs <- list(GM_2013_dhs, GM_2019_dhs, GH_2014_dhs, GH_2016_mis, GH_2019_mis, GN_2012_dhs, GN_2021_mis) 
dhs <- list(KE_2015_mis, KE_2020_mis, LB_2011_mis, LB_2016_mis)
dhs <- list(MD_2011_mis, MD_2013_mis) #DONE
dhs <- list(MW_2017_mis, MW_2012_mis) #DONE
dhs <- list(MW_2014_mis, ML_2012_dhs, ML_2015_mis, ML_2018_dhs, ML_2021_dhs)
dhs <- list(MR_2020_dhs)
dhs <- list(MZ_2011_dhs, MZ_2015_dhs, MZ_2018_dhs)
dhs <- list(NG_2015_mis, NG_2018_dhs, NG_2021_mis)
dhs <- list(RW_2010_mis, RW_2015_dhs, RW_2017_mis, RW_2019_dhs)
dhs <- list(SN_2010_dhs)

dhs <- list(SN_2010_dhs, SN_2012_dhs, SN_2014_dhs, SN_2015_dhs, SN_2016_dhs, SN_2017_dhs, SN_2020_mis) 
dhs <- list(TZ_2012_dhs, TZ_2015_dhs, TZ_2017_mis, TG_2013_dhs, TG_2017_mis, UG_2014_mis, UG_2016_dhs, UG_2018_mis) 
dhs <- list(CD_2013_dhs, CI_2012_dhs, CI_2021_dhs, MR_2020_dhs)

dhs1 <- dhs %>% map(~dplyr::filter(., hv103 == 1)) %>%  #individuals who slept in the HH night before
  map(~dplyr::select(., hhid, hv001, hml12, hv005, 
                     hv103, hv013, hv021, hv022, hv023, hv024, hv025, hv103)) %>% 
  map(~mutate(., net_use =ifelse(hml12 %in% c(1,2), 1, 0), #slept under an ITN the night before
              wt=hv005/1000000,
              strat=hv022,
              id=hv021))

# MADAGASCAR- 2011 & 2013 MISSING STRATA- can combine hv024 and hv025 to create strat
dhs1 <- dhs %>% map(~dplyr::filter(., hv103 == 1)) %>%  #individuals who slept in the HH night before
  map(~dplyr::select(., hhid, hv001, hml12, hv005, 
                     hv103, hv013, hv021, hv022, hv024, hv025, hv103)) %>% 
  map(~mutate(., net_use =ifelse(hml12 %in% c(1,2), 1, 0), #slept under an ITN the night before
              wt=hv005/1000000,
              #strat=hv022,
              id=hv021))
  
MD_2011 <- dhs1[[1]] %>% group_by(hv024,hv025) %>% 
  mutate(strat = cur_group_id())
MD_2013 <- dhs1[[2]] %>% group_by(hv024,hv025) %>% 
  mutate(strat = cur_group_id())

dhs1.2 <- list(MD_2011, MD_2013)

UG_2014_mis %>% 
  filter(hv025==1) %>% 
  tabyl(hv001)

#compute net use
vars <- c('net_use')

for (i in 1:length(vars)) {
  col <- list(vars[i])
  by <- list('hv001')
  df <- dhs1 %>% 
    map(~drop_na(.,vars[i]))
  df <-  pmap(list(df,col,by), estim_prop)
  df_use <- plyr::ldply(df)
  write.csv(df_use, file =file.path(Out, paste0(vars[i], "_BF_2021_DHS_2_15_24.csv")))
}

# Average net use in Largest Urban Center
TZ_2012 <- dhs1[[1]]
TZ_2015 <- dhs1[[2]]
TZ_2017 <- dhs1[[3]]
TG_2013 <- dhs1[[4]]
TG_2017 <- dhs1[[5]]
UG_2014 <- dhs1[[6]]
UG_2016 <- dhs1[[7]]
UG_2018 <- dhs1[[8]]

BF_2021 <- dhs1[[1]]
svydat <- svydesign.fun(BF_2021) # Re-do for each survey

tab1 <- svyby(~net_use, ~(hv023 + hv025), design= subset(svydat, hv025==1), svymean, vartype=c("se", "ci"), na.rm= TRUE)  %>% 
  filter(hv023== 5) %>%   # Update filter for each survey
  mutate(net_use= net_use*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab1

#Average net use per survey--- All urban areas minus Largest Urban Center

tab2 <- svyby(~net_use, ~hv025, design= subset(svydat, hv023 !=5), svymean, vartype=c("se", "ci"), na.rm= TRUE) %>% 
  filter(hv025==1) %>%
  mutate(net_use= net_use*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab2

#Average net use per survey--- All Urban vs. All Rural areas
tab3 <- svyby(~net_use, ~hv025, design= svydat, svymean, vartype=c("se", "ci"), na.rm= TRUE)  %>% 
  mutate(net_use= net_use*100, ci_l= ci_l*100, ci_u= ci_u*100)
tab3

#compute ITN use given access 

df_use <- df_use %>% mutate(year = str_split(country_year, "_", simplify = T)[, 2]) 
df_access <- df_access %>% mutate(year = str_split(country_year, "_", simplify = T)[, 2])

df_netU_access <- left_join(df_use,df_access, by=c('country_year', 'hv001'))

#read in the file
#df_netU_access <- read.csv(file.path(Out, "use_given_access", "netU_access.csv"))

df_netU_access$netU_access <- round(df_netU_access$net_use/df_netU_access$access * 100, 2) 
df_netU_access$netU_access2 <- ifelse(df_netU_access$netU_access > 100, 100,df_netU_access$netU_access)

df_netU_access_shrt <- df_netU_access %>% 
  dplyr::select(-c(se, ci_l, ci_u, netU_access))

#save dataframe of use given access
netuse <- file.path(data_dir, "Objective_3", "use_given_access")
write.csv(df_netU_access_shrt, file= file.path(netuse, "netU_access.csv"))
