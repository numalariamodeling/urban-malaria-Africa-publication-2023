#This script plots trends in PfPR, and identified PfPR by RDT
## -----------------------------------------
## Author: Chilo Chiziba
## Edited by: Colleen Leonard
## -----------------------------------------

library(readxl)
library(tidyverse)
library(haven)
library(viridis)
library(ggplot2)
library(ggrepel)
library(scales)
library(zoo)

#Read in file paths
user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "NU-malaria-team Dropbox")
PopDir <- file.path(NuDir, "data", 'Urban_malaria_net_ownership_data')
data_dir <- file.path(PopDir, "Extracted_csv")

#functions 
source("functions/functions_plots.R")

# Loading required data: Loading itn data to get the last ten years and for unforeseen required analysis between itn and pfpr
nets_df <- read.csv(file.path(data_dir, "final_interp_df2021.csv"), header = T, sep = ',') %>%
  select(country, year_survey2, SubregionName)

# loading required data: Pre-estimated Malaria prevalence data for Urban areas was downloaded from the DHS statcompiler site
# Malaria prevalence data for largest urban centre (city) was calculated as shown in Q3_pfpr_RDT.R
pfpr_df2 <- read_excel(file.path(data_dir,"Statcompiler_pfpr_RDT", "urban_pfpr_RDT.xlsx")) %>% 
  mutate(urban_pfpr = Urban/100, city_pfpr= Largest_city/100, other_urb_pfpr= Urban_minus_city/100)  
# Malaria prevalence data for Urban vs. Rural
pfpr_df3 <- read_excel(file.path(data_dir,"Statcompiler_pfpr_RDT", "rural_urb_pfpr_RDT.xlsx")) %>% 
  mutate(urban_pfpr = Urban/100, rural_pfpr= Rural/100)  

# Read in Pfpr in urban areas by Wealth quintile 
pfpr_ur_wealth <- read_excel(file.path(data_dir,"ur_pfpr_wealth.xlsx"))

#titles
countries <- pfpr_df2 %>% distinct(Country)
countries <- countries$Country



####################################################
#Data merging and interpolation 
##########################################################


 #datamerge
df <- left_join(nets_df, pfpr_df2, by = c("country" = "Country", "year_survey2" = "SurveyYear"))
  
#spliting df
df_list <- split(df, with(df, interaction(country)), drop = TRUE)
df_w_list <- split(pfpr_ur_wealth, with(pfpr_ur_wealth, interaction(country)), drop = TRUE)

#Update function for 3 pfpr variables
  fill.fun <- function(dat){
    
    dat %>% arrange(country,year_survey2) %>% mutate(pfpr_dfill = na.locf0(other_urb_pfpr, fromLast = TRUE)) %>%
      mutate(pfpr_ufill = zoo::na.locf0(other_urb_pfpr, fromLast = FALSE)) %>%
      mutate(pfpr_lifill = na.approx(other_urb_pfpr, na.rm = FALSE)) %>%
      mutate(name_text = paste(country, year_survey2, sep=" ")) %>%
      mutate(final_interp_pfpr= pfpr_lifill) %>% 
      #mutate(final_interp_pfpr = ifelse(is.na(pfpr_lifill), pfpr_ufill, pfpr_lifill))%>%
     # mutate(final_interp_pfpr = ifelse(is.na(final_interp_pfpr), pfpr_dfill, final_interp_pfpr)) %>%
      filter(year_survey2 >= 2010) %>% mutate(nas = sum(is.na(other_urb_pfpr))) #you may change the year to 2010 if you are interested in >2010
  }

fill.funw <- function(dat){
    dat %>% arrange(country,year_survey2) %>% mutate(pfpr_dfill = na.locf0(pfpr_notwealthy, fromLast = TRUE)) %>%
      mutate(pfpr_ufill = zoo::na.locf0(pfpr_notwealthy, fromLast = FALSE)) %>%
      mutate(pfpr_lifill = na.approx(pfpr_notwealthy, na.rm = FALSE)) %>%
      mutate(name_text = paste(country, year_survey2, sep=" ")) %>%
      mutate(final_interp_pfpr= pfpr_lifill) %>% 
      filter(year_survey2 >= 2010) %>% mutate(nas = sum(is.na(pfpr_notwealthy))) 
  }
cntry_interp_list <- lapply(df_list, fill.fun)

cntry_interp_list <- cntry_interp_list %>% 
    map(~mutate(., final_interp_pfpr = final_interp_pfpr, other_urb_pfpr = other_urb_pfpr))

# Pfpr by wealth
cntry_interp_list2 <- lapply(df_w_list, fill.funw)

#Urban pfpr df
  final_urb_df <- bind_rows(cntry_interp_list) %>% 
    mutate(final_pfpr_urb= final_interp_pfpr) %>% 
    select(country, year_survey2, SubregionName, SurveyName, Indicator, urban_pfpr, 
           name_text, nas, final_pfpr_urb)
  
#City pfpr df
  final_city_df <- bind_rows(cntry_interp_list) %>% 
    mutate(final_pfpr_city= final_interp_pfpr) %>% 
    select(country, year_survey2,  city_pfpr, final_pfpr_city)
#All urban areas except largest urban center (city) pfpr df
  final_oth_df <- bind_rows(cntry_interp_list) %>% 
    mutate(final_pfpr_oth= final_interp_pfpr) %>% 
    select(country, year_survey2, other_urb_pfpr, final_pfpr_oth)

### By Wealth in urban areas ###

#Urban pfpr- wealthy
final_ur_wealthy_df <- bind_rows(cntry_interp_list2) %>% 
    mutate(final_pfpr_ur_weal= final_interp_pfpr) %>% 
    select(country, year_survey2, SubregionName, SurveyName, Indicator, urban_pfpr, 
           name_text, nas, pfpr_wealthy, final_pfpr_ur_weal)

#Urban pfpr- not wealthy category
final_ur_nwealth_df <- bind_rows(cntry_interp_list2) %>% 
  mutate(final_pfpr_ur_nweal= final_interp_pfpr) %>% 
  select(country, year_survey2, SubregionName, SurveyName, Indicator, urban_pfpr, 
         name_text, nas, pfpr_nwealthy, final_pfpr_ur_nweal)

# data cleaning
# Left join
df_1 <- left_join(final_urb_df, final_city_df, by= c("country", "year_survey2"))
df_all <- left_join(df_1, final_oth_df, by= c("country", "year_survey2"))

ur_city_pfpr <- df_all %>% 
  dplyr::group_by(country)%>%
  dplyr::mutate(tests = sum(!is.na(urban_pfpr)))

test.fun <- function(dat){
  
  dat %>% arrange(country,year_survey2) %>%
    mutate(tests = sum(!is.na(urban_pfpr)))
}

df_wealthq <- left_join(final_ur_wealthy_df, final_ur_nwealth_df, by= c("country", "year_survey2")) %>% 
  select(country, year_survey2, SubregionName.x, SurveyName.x, name_text.x, pfpr_wealthy, final_pfpr_ur_weal,
         pfpr_nwealthy, final_pfpr_ur_nweal)



#save csv
#write.csv(ur_city_pfpr, file.path(data_dir, "ur_city_pfpr5.csv")) #final RDT results by city, all urban areas, and all urban except the largest city
#write.csv(df_wealthq, file.path(data_dir, "ur_pfpr_wealth_interp.csv")) #final PfPR in urban areas by wealth quintile (wealthy (4/5) vs. not)

#Read in dataframe if needed
ur_city_pfpr <- read.csv(file.path(data_dir, "ur_city_pfpr5.csv"))
df_wealthq <- read.csv(file.path(data_dir, "ur_pfpr_wealth_interp.csv"))

#splitting df and ordering for plotting
region_list <- split(ur_city_pfpr[which(ur_city_pfpr$tests>0),], with(ur_city_pfpr[which(ur_city_pfpr$tests>0),], interaction(SubregionName)), drop = TRUE) 
country_list <- split(ur_city_pfpr[which(ur_city_pfpr$tests>0),], with(ur_city_pfpr[which(ur_city_pfpr$tests>0),], interaction(country)), drop = TRUE) 

cntry_list <- list()
for (i in 1:length(region_list)){
  splited_regions <- split(region_list[[i]], with(region_list[[i]], interaction(country)), drop = TRUE)
  cntry_list[[i]] <- lapply(splited_regions,test.fun)
}

country_w_list <- split(df_wealthq[], with(df_wealthq[], interaction(country)), drop = TRUE) 


plot_all <- lapply(country_list, pfpr_trend.fun)

plot_wealth_all <- lapply(country_w_list, pfpr_trend_w.fun)

setwd("C:/Users/Colleen/OneDrive/Documents/1. Northwestern Malaria Modeling Group/Urban_nets/Plots/FINAL")

pfpr_annotate.fun(plot_all)

pfpr_annotate.fun(plot_wealth_all) # pfpr in Urban areas by wealth quintile

#PRINT Plots by African Subregion
tiff(filename = "pfpr_urb_city_middleAFR.tiff", width= 2700, height= 1000, units= "px",
     compression = "lzw", res= 200)
pfpr_annotate.fun(p_region_2)
dev.off()

tiff(filename = "pfpr_urb_city_westernAFR.tiff", width= 2700, height= 1000, units= "px",
     compression = "lzw", res= 200)
pfpr_annotate.fun(p_region_3)
dev.off()


# Make one dataframe
# Concatenate all data frames using bind_rows()
combined_pfpr_urb <- bind_rows(country_list) %>% 
  dplyr::select(country, year_survey2, SubregionName, SurveyName, Indicator, urban_pfpr, name_text,
                city_pfpr, other_urb_pfpr)

combined_pfpr_urb <- combined_pfpr_urb  %>% 
  filter(!is.na(urban_pfpr)) %>% 
  mutate(period = if_else(year_survey2 < 2016, "bsl", "recent"))

# Pfpr urban only- Boxplots

# Create a boxplot
ggplot(combined_pfpr_urb, aes(x = period, y = urban_pfpr, fill = period)) +
  geom_boxplot(color= "black") +
  scale_x_discrete(labels = c("bsl" = "Baseline", "recent" = "Recent")) +  # Rename group names
  # stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), vjust = -1, color = "black") +  # Label median
  labs(title = "", x= "", y = "PfPR") +
  scale_fill_manual(values = c("lightblue", "cadetblue4")) +  # Custom fill colors for each group
  theme_minimal() + # Use minimal theme
  theme(legend.position = "none")

# Pfpr Rural only- Boxplots
# join rural df with previous dataset
pfpr_df4 <- left_join(combined_pfpr_urb, pfpr_df3, by = c("country" = "Country", "SurveyName"))


ggplot(pfpr_df4, aes(x = period, y = rural_pfpr, fill = period)) +
  geom_boxplot(color= "black") +
  scale_x_discrete(labels = c("bsl" = "Baseline", "recent" = "Recent")) +  # Rename group names
  # stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), vjust = -1, color = "black") +  # Label median
  labs(title = "", x= "", y = "PfPR") +
  scale_fill_manual(values = c("orange", "darkorange3")) +  # Custom fill colors for each group
  theme_minimal() + # Use minimal theme
  theme(legend.position = "none")
