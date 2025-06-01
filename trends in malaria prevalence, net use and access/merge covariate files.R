## Script to merge individual covariate files

library(tidyverse)
library(writexl)

#Filepaths
user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "NU-malaria-team Dropbox")
data_dir <- file.path(NuDir, "data", 'urban_malaria_net_ownership_data', 'Extracted_csv')

Out_pfpr <- file.path(data_dir, "Objective_3", "PfPR_RDT")
Out_num <- file.path(data_dir, "Objective_3", "num_pos_RDT")
Obj3 <- file.path(data_dir, "Objective_3")
Out_precip <- file.path(data_dir, "Objective_3", "Precip")
Out_EVI <- file.path(data_dir, "Objective_3", "EVI")
Out_humid <- file.path(data_dir, "Objective_3", "Relative humidity")
Out_housing <- file.path(data_dir, "Objective_3", "modern_housing")

setwd(Out_pfpr)
getwd()

#Read in PfPR by RDT
df_rdt <- list.files(path = Out_pfpr, pattern = glob2rx("*.csv$")) %>% 
  map(~read_csv(.))


df2 <- lapply(df_rdt, function(x){
  colnames(x)[1] <- "country_year"
  x 
})

df_pfpr <- plyr::ldply(df2) %>% 
  dplyr::select(-c("ci_l", "ci_u")) %>% 
  mutate(country= substr(country_year, 1, 2),
         year= substr(country_year, 4, 7))

#Read in Number of children positive for Pf. by RDT
df_numpos <- list.files(path = Out_num, pattern = glob2rx("*.csv$")) %>% 
  map(~read_csv(.))

df_num <- plyr::ldply(df_numpos) %>% 
  dplyr::select(-c("num_neg_rdt"))


setwd(Obj3)
#read in Net access by survey
df <- list.files(path = Obj3, pattern = glob2rx("access*.csv")) %>% 
  map(~read_csv(.))

df2 <- lapply(df, function(x){
  colnames(x)[1] <- "country_year"
  x 
})

df_access<- plyr::ldply(df2) %>% 
  dplyr::select(-c("ci_l", "ci_u"))

#read in Net use by survey
df <- list.files(path = Obj3, pattern = glob2rx("net_use*.csv")) %>% 
  map(~read_csv(.))

df2 <- lapply(df, function(x){
  colnames(x)[1] <- "country_year"
  x 
})

df_use <- plyr::ldply(df2) %>% 
  dplyr::select(-c("se"))

#read in Net use given access
df_use_access <- read.csv(file.path(data_dir, "Objective_3/use_given_access", "netU_access.csv")) %>% 
  mutate(netU_access= netU_access2/100) %>% 
  dplyr::select(-c("netU_access2"))

# Read in Precipitation data 

setwd(Out_precip)

df <- list.files(path = Out_precip, pattern = glob2rx("*.csv$")) %>% 
  map(~read_csv(.))
df_17 <- df[[17]]

df2 <- lapply(df, function(x){
  colnames(x)[1] <- "country_year"
  x 
})

#removing columns we don't need
df2[[1]] <- df2[[1]] %>% 
  dplyr::select(-c("hv024", "hv006", "month_lag", "District"))

df2[[2]] <- df2[[2]] %>% 
  dplyr::select(-c("hv024", "hv006", "month_lag", "Region"))

df2[[3]] <- df2[[3]] %>% 
  dplyr::select(-c("hv024", "hv006", "hv007", "month_lag", "Region"))
df2[[23]] <- df2[[23]] %>% 
  dplyr::select(-c("hv023", "hv006", "month_lag", "Province"))
df_sub <- df2[c(1:3, 23)]
  
df_buffer <- df2[4:22] #same columns throughout 

df_b <- lapply(df_buffer, function(x){
  colnames(x)[2] <- "Precip"
  x 
})
df_b2 <- lapply(df_b, function(x){
  x <- x %>% dplyr::select(c("country_year", "hv001", "Precip")) %>% 
    mutate(country_year= substr(country_year, 1, 7))
  x
})

df_new <- c(df_sub, df_b2)
df_precip <- plyr::ldply(df_new) %>% 
  mutate(precip= round(if_else(Precip== -9999, NA, Precip), 3)) %>%  #Change -9999 to NA
  dplyr::select(-c("Precip"))


# Read in EVI data 
setwd(Out_EVI)

df <- list.files(path = Out_EVI, pattern = glob2rx("*.csv$")) %>% 
  map(~read_csv(.))

df2 <- lapply(df, function(x){
  colnames(x)[1] <- "country_year"
  x 
})
df_buffer <- df2[1:19]

df_b <- lapply(df_buffer, function(x){
  colnames(x)[2] <- "EVI"
  x <- x %>% dplyr::select(c("country_year", "hv001", "EVI")) %>% 
    mutate(country_year= substr(country_year, 1, 7))
  x
})

df_subset <- df2[20:23]
df_sub <- lapply(df_subset, function(x){
  x <- x %>% dplyr::select(c("country_year", "hv001", "EVI"))
  x
})
df_new <- c(df_sub, df_b)

df_EVI <- plyr::ldply(df_new) %>% 
  mutate(EVI= round(EVI, 3))

# Read in Humidity data 
setwd(Out_humid)

df <- list.files(path = Out_humid, pattern = glob2rx("*.csv$")) %>% 
  map(~read_csv(.))

df2 <- lapply(df, function(x){
  colnames(x)[1] <- "country_year"
  x 
})

df_buffer <- df2[4:22]

df_b <- lapply(df_buffer, function(x){
  colnames(x)[2] <- "RH"
  x <- x %>% dplyr::select(c("country_year", "hv001", "RH")) %>% 
    mutate(country_year= substr(country_year, 1, 7))
  x
})

df_subset <- df2[c(1:3, 23)]
df_sub <- lapply(df_subset, function(x){
  x <- x %>% dplyr::select(c("country_year", "hv001", "RH"))
  x
})

df_new <- c(df_sub, df_b)
df_RH <- plyr::ldply(df_new) %>% 
  mutate(RH= round(RH/100, 3))

# Read in modern housing variable
setwd(Out_housing)
df_h <- list.files(path = Out_housing, pattern = glob2rx("housing*.csv")) %>% 
  map(~read_csv(.))

df_housing <- plyr::ldply(df_h) %>% 
  dplyr::select(-c("ci_l", "ci_u")) %>% 
  mutate(housing_q= round(housing_q, 3))

# Read in wealth variable
setwd(Out_wealth)
df_w <- list.files(path = Out_housing, pattern = glob2rx("wealth*.csv")) %>% 
  map(~read_csv(.))

df_wealth <- plyr::ldply(df_w) %>% 
  dplyr::select(-c("ci_l", "ci_u")) %>% 
  mutate(wealth= round(wealth, 3))

df_1 <- left_join(df_num, df_use_access, by= c("country_year", "hv001"))
df_2 <- left_join(df_1, df_housing, by= c("country_year", "hv001"))
df_3 <- left_join(df_2, df_wealth, by= c("country_year", "hv001"))
df_4 <- left_join(df_3, df_precip, by= c("country_year", "hv001"))
df_5 <- left_join(df_4, df_EVI, by= c("country_year", "hv001"))
df_6 <- left_join(df_5, df_pfpr, by= c("country_year", "hv001"))
df_all <- left_join(df_6, df_RH, by= c("country_year", "hv001"))

write_xlsx(df_all, path= file.path(Obj3, "All vars/df_all_covariates_012024.xlsx"))


