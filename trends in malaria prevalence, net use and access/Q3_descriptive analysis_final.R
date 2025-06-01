### Urban Nets Q3- Trends in malaria prevalence in urban areas ###
# Examining trends from 2010 - 2021 using DHS and MIS survey data
## Descriptive analysis
# Author: Colleen Leonard
# Date last edited: 7/18/2023

library(readxl)
library(plyr)
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
NuDir <- file.path(Drive, "NU-malaria-team Dropbox", "data", 'urban_malaria_net_ownership_data')
data_dir <- file.path(NuDir, "Extracted_csv")
Out <- file.path(data_dir, "Objective_3", "PfPR_RDT")
Out_num <- file.path(data_dir, "Objective_3", "num_pos_RDT")
Obj3 <- file.path(data_dir, "Objective_3")
all_vars <- file.path(data_dir, "Objective_3", "All vars")

#### Read in in final Data file for analysis ####
df_all <- readxl::read_xlsx(file.path(all_vars,"df_all_covariates_021924.xlsx"))
countries_list <- readxl::read_xlsx(file.path(Obj3, "Countries_list.xlsx")) %>%  #read in list of countries and region
    select(-c(year))
df_all <- df_all %>% 
  left_join(countries_list, by= "country_year")

DF_all <- df_all %>% group_split(by= country) 

df_tested <- df_all %>% 
  filter(num_tested != 0)

colSums(df_tested==0) # number of clusters with zero positive 
summary(df_tested$num_pos_rdt)

nets <- read.csv(file.path(data_dir,"final_interp_df.csv"), header = T, sep = ',',check.names = F) %>%
  select(-c(1)) %>%
  mutate(year = year_survey2,
    year_survey2 = as.factor(year_survey2)) 

urb_pop <- read.csv(file.path(data_dir,"urb_pop_proportion.csv"), header = T, sep = ',',check.names = F) %>% 
  select(-c("Unit")) %>% 
  pivot_longer(cols= -Country, 
              names_to= "year", values_to= "urb_prop") %>% 
  mutate(year = as.factor(year)) 

nets_md_years <- read.csv(file.path(data_dir,"ITN_Mass_Distribution_Final_updt.csv"), header = T, sep = ',',check.names = F) %>% 
  filter(Year > 2010 & mass_dist== 1)
nets_md_years <- nets_md_years %>% 
  mutate(country= if_else(Country== "United Republic of Tanzania (Mainland)", "Tanzania", Country))



## Include countries with only 1 year of data. 

#### Summary stats ####

# Average number of children tested per cluster 
summary(df_all$num_tested)


#### Histograms ####
# Histogram of counts of malaria positive
p= pmap(list(DF_all, 'maroon', 'maroon', 'num_pos_rdt', "Number of children 6-59 months positive for malaria by RDT per cluster", 10), cdf_hist)
p[[1]] #for the first country
p[[2]]
p[[3]]

p= hist(df_all, 'maroon', 'maroon', 'net_use', "Proportion slept under a net last night per cluster", 50)
p  # really no trend

# Net access
p= hist(df_all, 'maroon', 'maroon', 'access', "Net access per cluster", 50)
p  #really no clear trend except net access is higher than net use

# Net use given access
p= hist(df_all, 'maroon', 'maroon', 'netU_access', "Net use given access per cluster", 50)
p  

# Number of children under 5 positive by RDT
p= hist(df_all, 'maroon', 'maroon', 'num_pos_rdt', "Number positive per cluster", 10)
p  

## Scatterplot of Proportion of nets in urban areas with proportion of the population living in urban areas

#number of urban nets by region
nets_2011 <- nets %>% 
  filter(year >= 2010 & !(country %in% c("Eswatini", "Sao-Tome-Principe"))) %>% #don't have DHS data for split
  dplyr::group_by(SubregionName) 


sum_nets <- nets_2011 %>% 
  dplyr::group_by(SubregionName) %>% 
  summarize(total_ur= sum(final_ur_nets),
            total_rur= sum(final_ru_nets), na.rm= TRUE)
print(sum_nets)

sum_nets_ur <- nets_2011 %>% 
  filter(final_ur_nets != 0) 

# Calculate for countries w/o urban/rural net distribution info
sum_nets_na <-  nets %>% 
  filter(year >= 2010) %>% 
  filter(final_ur_nets== 0) %>% 
  filter(country %in% c("Botswana","Central African Republic","Côte d'Ivoire", "Equatorial Guinea",
                        "Eritrea", "Ethiopia", "Guinea-Bissau","South Sudan")) %>% #Selecting countries with info on nets distributed, but no rural/urban info
  mutate(SubregionName= if_else(country== "Côte d'Ivoire", "Western Africa", SubregionName))

sum_nets_unsp <- sum_nets_na %>%  #unspecified b/t urban/rural distribution. 
  dplyr::group_by(SubregionName) %>% 
  summarize(total_nets_unsp= sum(total.net.distributed, na.rm= TRUE))
print(sum_nets_unsp)

df_itn <- nets %>% 
  dplyr::select(c(country, year_survey2, urb_itn_prop))

df1 <- urb_pop %>% 
  left_join(df_itn, by= c("Country"= "country", "year"= "year_survey2")) %>% 
  mutate(urb_prop= round(urb_prop/100, 2))

plot_1 <- ggplot(data= df1, aes(x= urb_prop, y= urb_itn_prop))

plot_1 +
  geom_point()+
  geom_abline(slope = 1, intercept = 0, color="red", linetype="dashed") +   #Add proportional line
 scale_x_continuous(limits = c(0,1))+
  #xlim= c(0, 1) +
  scale_y_continuous(limits = c(0,1))+ 
  theme_manuscript()+
  labs(x= "Proportion of population living in urban areas", y= "Proportion of ITNs in urban areas")

#### Box plots ####
## Nets distributed in urban areas ##

# merge nets_2011 with nets_md_years dataframe
nets_2011_md <- nets_md_years %>% 
  left_join(nets_2011, by= c("country", "Year"= "year")) %>% 
  mutate(period = if_else(Year < 2016, "bsl", "recent"))

options(scipen=999)
# Read in file of urban population per year
df_pop <- read_excel("C:/Users/Colleen/Documents/3. Graduate School/Dropbox/malaria- NU/population_2011-2020.xlsx")
df_pop_AFR <- df_pop %>% 
  mutate(Country= if_else(country== "Congo, Dem. Rep.", "Congo Democratic Republic", 
                          if_else(country== "Gambia, The", "Gambia", country))) %>% 
  filter(Country %in% nets_2011_md$country) 

# Transform from wide to long format.
df_pop_long1 <- df_pop_AFR %>%
  select(c(Country, starts_with("pop_ur"))) %>% 
  pivot_longer(
    cols = starts_with("pop_ur"),   
    names_to = "Year",           
    values_to = "Pop_urban",    
    names_prefix = "pop_ur_"     # Remove the "pop_ur" prefix from the year column
  )

df_pop_long2 <- df_pop_AFR %>%
  select(c(Country, starts_with("pop_rur"))) %>% 
  pivot_longer(
    cols = starts_with("pop_rur"),   
    names_to = "Year",           
    values_to = "Pop_rural",    
    names_prefix = "pop_rur_"     # Remove the "pop_ur" prefix from the year column
  )

df_pop_long <- df_pop_long1 %>% 
  left_join(df_pop_long2, by= c("Country", "Year"))

df_pop_long$Year <- as.integer(df_pop_long$Year)
df_pop_long$Pop_urban <- round(df_pop_long$Pop_urban, digits= 0)
df_pop_long$Pop_rural <- round(df_pop_long$Pop_rural, digits= 0)

# Per Capita- Just Divide final_ur_nets / population in Urban areas from excel file !!!
nets_per_cap <- nets_2011_md %>% 
  left_join(df_pop_long, by= c("country"= "Country", "Year")) %>% 
  filter(final_ur_nets != 0)  #remove any country/year pairs where final_ur_nets is 0

nets_per_cap2 <- nets_per_cap %>% 
  mutate(final_ur_nets_pc= final_ur_nets/Pop_urban,
         final_rur_nets_pc= final_ru_nets/Pop_rural)

# Boxplot of ITNs distributed in urban areas during MD years (in Millions)
ggplot(nets_2011_md, aes(x = period, y = final_ur_nets/1000000, fill = period)) +
  geom_boxplot(color= "black") +
  scale_x_discrete(labels = c("bsl" = "Baseline", "recent" = "Recent")) +  # Rename group names
  # stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), vjust = -1, color = "black") +  # Label median
  labs(title = "", x= "", y = "Number of ITNs distributed (M)") +
  scale_fill_manual(values = c("lightblue", "cadetblue4")) +  # Custom fill colors for each group
  theme_minimal() + # Use minimal theme
  theme(legend.position = "none")


# Boxplot of ITNs distributed in rural areas during MD years (in Millions)
ggplot(nets_2011_md, aes(x = period, y = final_ru_nets/1000000, fill = period)) +
  geom_boxplot(color= "black") +
  scale_x_continuous(labels = c("bsl" = "Baseline", "recent" = "Recent")) +  # Rename group names
#  scale_y_continuous(limits= c(0, 30)) +
  # stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), vjust = -1, color = "black") +  # Label median
  labs(title = "", x= "", y = "Number of ITNs distributed (M)") +
  scale_fill_manual(values = c("orange", "darkorange3")) +  # Custom fill colors for each group
  theme_minimal() + # Use minimal theme
  theme(legend.position = "none")

# Boxplot of ITNs distributed Per Capita in urban areas during MD years 
ggplot(nets_per_cap2, aes(x = period, y = final_ur_nets_pc, fill = period)) +
  geom_boxplot(color= "black") +
  scale_x_discrete(labels = c("bsl" = "Baseline", "recent" = "Recent")) + 
  # stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), vjust = -1, color = "black") +  # Label median
  labs(title = "", x= "", y = "Number of ITNs distributed per capita") +
  scale_fill_manual(values = c("lightblue", "cadetblue4")) +  # Custom fill colors for each group
  theme_minimal() + # Use minimal theme
  theme(legend.position = "none")

#ITNs distributed in rural areas Per Capita during MD years 
ggplot(nets_per_cap2, aes(x = period, y = final_rur_nets_pc, fill = period)) +
  geom_boxplot(color= "black") +
  scale_x_discrete(labels = c("bsl" = "Baseline", "recent" = "Recent")) + 
  #  scale_y_continuous(limits= c(0, 30)) +
  # stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), vjust = -1, color = "black") +  # Label median
  labs(title = "", x= "", y = "Number of ITNs distributed per capita") +
  scale_fill_manual(values = c("orange", "darkorange3")) +  # Custom fill colors for each group
  theme_minimal() + # Use minimal theme
  theme(legend.position = "none")

#### Bar charts ####

#Group by country_year
#df_cy <- df_all %>% dplyr::group_by(country_year) %>% 
#  dplyr::summarise(me_use = mean(net_use))
df_use <- read_xlsx(file.path(Obj3, "ITN_use_LargestUrbanCenter.xlsx"))
df_use2 <- df_use %>% 
  mutate(year= as.integer(SurveyYear)) %>% 
  dplyr::select(Country, year, use_largest_city, use_urban_minus_city, use_urban, use_rural)

#df_cy2 <- left_join(df_cy, countries_list, by= "country_year") #%>% 
 ## mutate(year= as.integer(year))

# Average net access
#df_access <- df_all %>% dplyr::group_by(country_year) %>% dplyr::summarise(me_access = mean(access))
df_access <- read_xlsx(file.path(Obj3, "ITN_access_LargestUrbanCenter.xlsx"))
df_access2 <- df_access %>% 
  mutate(year= as.integer(SurveyYear))


#Mean  net access during 2017-2021.
df_access2 %>% dplyr::filter(year >=2017) %>% 
  dplyr::summarize(mean= mean(access_urban, na.rm= TRUE))
#net use during 2017-2021.
df_use3 <- df_use2 %>% dplyr::filter(year >=2017) 

#### Calculate Net access in Largest Urban center, other urban areas, and rural areas ####

df_itn <- left_join(df_access2, df_use2, by= c("Country", "year")) %>% 
  mutate(period = if_else(year < 2016, "bsl", "recent"))
df_itn_minusMad <- df_itn %>% 
  filter(Country != "Madagascar") #Madagascar's largest urban center was not captured in the MIS.

write_xlsx(df_itn, path= file.path(Obj3, "Avg_netuse_access_per_country_year.xlsx"))

# Box plot of % net access in urban areas

ggplot(df_itn, aes(x = period, y = access_urban, fill = period)) +
  geom_boxplot(color= "black") +
  scale_x_discrete(labels = c("bsl" = "Baseline", "recent" = "Recent")) +  # Rename group names
  # stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), vjust = -1, color = "black") +  # Label median
  labs(title = "", x= "", y = "Net access (%)") +
  scale_fill_manual(values = c("lightblue", "cadetblue4")) +  # Custom fill colors for each group
  theme_minimal() + # Use minimal theme
  theme(legend.position = "none")

# Box plot of % net access in rural areas
ggplot(df_itn, aes(x = period, y = access_rural, fill = period)) +
  geom_boxplot(color= "black") +
  scale_x_discrete(labels = c("bsl" = "Baseline", "recent" = "Recent")) +  # Rename group names
  scale_y_continuous(limits= c(0,80))+
  # stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), vjust = -1, color = "black") +  # Label median
  labs(title = "", x= "", y = "Net access (%)") +
  scale_fill_manual(values = c("orange", "darkorange3")) +  # Custom fill colors for each group
  theme_minimal() + # Use minimal theme
  theme(legend.position = "none")


#Net access and use in one facet plot

#ITN plot Function
itn_plot <- function(df, access, use){
ggplot(data = df) + 
  geom_point(aes(x= year, y= .data[[access]], color= "net access"))+ 
  geom_line(aes(x= year, y= .data[[access]], color= "net access"))+
  geom_point(aes(year, .data[[use]], color= "net use"))+
  geom_line(aes(year, .data[[use]], color= "net use"))+
  facet_wrap( ~Country,  strip.position = "top", scales= "free")+
  xlim(2010, 2021) + ylim(0, 85)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust= 0.9), legend.title= element_blank(),
        legend.position = "bottom")+
  scale_color_manual(values= c("net access"= "darkblue", "net use"= "lightblue"))+
  xlab("Year")+ ylab("Average net access and use in urban areas (%)") 
}
itn_plot(df_itn, "access_urban", "use_urban")
itn_plot(df_itn, "access_rural", "use_rural")
itn_plot(df_itn_minusMad, "access_largest_city", "use_largest_city")
itn_plot(df_itn_minusMad, "access_urban_minus_city", "use_urban_minus_city")


#Facet wrap net access over time faceted by Country
plot <- trend_nets(df_access2, x= "year", yval= "me_access")
plot

#Combine net access and use dfs
df_new <- left_join(df_access2, avg_netuse, by= c("country_year")) %>% 
  dplyr::rename(country= country.x)

#Net access and use in one facet plot
ggplot(data = df_new) + 
  geom_point(aes(x= year, y= me_access, color= "net access"))+ 
  geom_line(aes(x= year, y= me_access, color= "net access"))+
  geom_point(aes(year, me_use, color= "net use"))+
  geom_line(aes(year, me_use, color= "net use"))+
  facet_wrap( ~country,  strip.position = "top", scales= "free")+
  xlim(2010, 2021) + ylim(0, 0.9)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust= 0.9), legend.title= element_blank(),
        legend.position = "bottom")+
  scale_color_manual(values= c("net access"= "salmon", "net use"= "cyan2"))+
  xlab("Year")+ ylab("Average net access and use per cluster in urban areas (%)") 


#### Scatterplots of independent variables and outcome (PfPR) ####

df2 <- df_all %>% 
  dplyr::select(c(country_year, country, year, ml_rdt_pos, hv001, access, net_use, netU_access)) %>% 
  rename("PfPR"= "ml_rdt_pos")


plot_1 <- ggplot(data= df2, aes(x= access, y= PfPR, group= country, color= country))

plot_1 +
  geom_point()+
  geom_smooth(method= "lm", se= FALSE)+
  theme_minimal()+
  scale_x_continuous()+
  labs(x= "ITN access", y= "PfPR by cluster (%)",
       title= "PfPR by net access per cluster by Country ")

#net use given access
NG_netU <- left_join(NG_pfpr, df_netU_access, by= c("country_year", "hv001")) %>% 
  rename("PfPR"= "ml_rdt_pos")


plot_NG <- ggplot(data= NG_netU, aes(x= netU_access2, y= PfPR, group= country_year, color= country_year))

plot_NG +
  geom_point()+
  geom_smooth(method= "lm", se= FALSE)+
  theme_minimal()+
  scale_x_continuous()+
  labs(x= "ITN use given access (%)", y= "PfPR by cluster (%)",
       title= "Nigeria- Proportion of urban population- PfPR by net use given access")


plot_2 <- ggplot(data= df2, aes(x= year, y= PfPR))

plot_2 +
  geom_point()+
  geom_smooth(method= "loess", se= FALSE, span =0.5)+
  theme_minimal()+
  scale_x_continuous()+
  labs(x= "Survey year", y= "PfPR by cluster (%)",
       title= "PfPR by year")



#### Correlations ####

#Kendall's tau- good to find correlation b/t 2 continuous variables.
#a tau of 0.30 or higher is a strong correlation. 

#Ex) All countries
#Net access and PfPR
cor.test(df_all$access, df_all$ml_rdt_pos, method= "kendall") #no corr

#Net use and PfPR
cor.test(df_all$net_use, df_all$ml_rdt_pos, method= "kendall") #no corr

#Net use given access and PfPR
cor.test(df_all$netU_access, df_all$ml_rdt_pos, method= "kendall")  #no corr

#wealth
cor.test(df_all$wealth, df_all$ml_rdt_pos, method= "kendall")  #corr= -0.2 

#housing quality
cor.test(df_all$housing_q, df_all$ml_rdt_pos, method= "kendall")  #corr= -0.1 

#precip
cor.test(df_all$precip, df_all$ml_rdt_pos, method= "kendall")  #corr= 0.19

#EVI
cor.test(df_all$EVI, df_all$ml_rdt_pos, method= "kendall")  #corr= 0.29

#Relative humidity
cor.test(df_all$RH, df_all$ml_rdt_pos, method= "kendall")  #corr= 0.12



#My functions
hist = function(df, fill, color, x, xlab, bins){
  hist=ggplot(df, aes(x =.data[[x]]))+geom_histogram(alpha = 0.4, position="identity", bins=bins)
  max_y=max(ggplot_build(hist)$data[[1]]$count)
  ggplot(df, aes(.data[[x]]))+
    geom_histogram(fill=fill, color= color, alpha = 0.4, position="identity", bins = bins) +
    #stat_ecdf(aes(y = bquote(after_stat(y)*.(max_y)), color =color))+
    scale_y_continuous(name= 'Count')+
    theme_manuscript()+theme(legend.position = 'none')+
    xlab(xlab)
}


cdf_hist = function(df, fill, color, x, xlab, bins){
  hist=ggplot(df, aes(x =.data[[x]]))+geom_histogram(alpha = 0.4, position="identity", bins=bins)
  max_y=max(ggplot_build(hist)$data[[1]]$count)
  ggplot(df, aes(.data[[x]]))+
    geom_histogram(fill=fill, color= color, alpha = 0.4, position="identity", bins = bins) +
    #stat_ecdf(aes(y = bquote(after_stat(y)*.(max_y)), color =color))+
    scale_y_continuous(name= 'Count')+
    theme_manuscript()+theme(legend.position = 'none')+
    xlab(xlab)
}


#plot theme
theme_manuscript <- function(){
  theme_bw() + 
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 12, color = "black"), 
          axis.text.y = element_text(size = 12, color = "black"),
          axis.title.x = element_text(size = 12),
          axis.title.y= element_text(size =12),
          legend.title=element_text(size=12, colour = 'black'),
          legend.text =element_text(size = 12, colour = 'black'),
          legend.key.height = unit(1, "cm"))
}
digits(0)
#Trend of urban net use over time
trend_nets <- function(df, x, yval){
  ggplot(data = df) + 
  geom_point(mapping= aes_string(x, yval))+ 
  geom_line(mapping= aes_string(x, yval))+
  facet_wrap( ~country,  strip.position = "top", scales= "free")+
    xlim(2010, 2021) + ylim(0, 0.9)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust= 0.9))+
  xlab("Year")+ ylab("Average net use per cluster in urban areas (%)") 
}

