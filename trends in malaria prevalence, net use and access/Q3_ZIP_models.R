#### Zero-inflated Poisson models ####
### Outcome= number of malaria cases per cluster among children 6- 59 months

library(tidyverse)
library(writexl)
library(glmmTMB)
library(splines)
library(ggeffects) 
library(scales)
library(expss)
library("DHARMa")

# Read in data file
#Filepaths
user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "NU-malaria-team Dropbox")
data_dir <- file.path(NuDir, "data", 'urban_malaria_net_ownership_data', 'Extracted_csv')
all_vars <- file.path(data_dir, "Objective_3", "All vars")
plots_out <- file.path(NuDir,"Projects/urban_malaria/urban_malaria_net_ownership/publication/plots in paper" )
setwd(all_vars)

df_all <- readxl::read_xlsx("df_all_covariates_021924.xlsx") %>% 
  mutate(country= substr(country_year, 1, 2))

results_dir <- file.path(NuDir, "projects/urban_malaria/urban_malaria_net_ownership/publication/plots in paper/")


#### Descriptive statistics- examine Variance within and between countries ####

df_all %>% 
  select(ml_rdt_pos) %>% 
  summarize(mean_pos= mean(ml_rdt_pos, na.rm= TRUE)) #average overall proportion positive is 0.12

summary(df_all$num_tested) #Some clusters have 0 tested.

#Drop clusters with no children tested for malaria
df_all <- df_all %>% 
  filter(num_tested != 0)

total_var <- df_all %>% 
  select(ml_rdt_pos) %>% 
  summarize(total_var= var(ml_rdt_pos, na.rm= TRUE))
total_var  #total variance= 0.044

between_var <- df_all %>% 
  group_by(country) %>% 
  mutate(m_pos= mean(ml_rdt_pos, na.rm= TRUE)) %>% 
  ungroup() %>% 
  summarize(between_var_pos= var(m_pos, na.rm= TRUE))
between_var #between country variance is 0.01 

ICC= between_var/ total_var
ICC # ICC= 0.21 

# Most of the variance is within country, but there is a good chunk of variance (21%) that varies Between countries.
# Use country as a random effect variable

#Examine within and between country variance

df_all <- df_all %>% 
  mutate(country= factor(country),
         precip_01= scales::rescale(precip, c(0, 1))) #rescale precip to be 0-1 to be on the same scale as other predictors. 
df_all <- df_all %>% 
  mutate(year= as.numeric(substr(country_year, 4, 7))- 2000, #year on similar scale 10-21
         log_num_tested= log(num_tested))
mean(df_all$log_num_tested)  # 2 

# Attach labels to year
add_val_lab(df_all$year) = num_lab("
            10 2010
            11 2011    
            12 2012  
            13 2013
            14 2014
            15 2015
            16 2016
            17 2017
            18 2018
            19 2019
            20 2020
            21 2021
")


#plot 
#density of malaria positivity by country
ggplot(df_all, aes(x= ml_rdt_pos, group = country, color = country)) +
  geom_density()  + labs(x = "Proportion positive by RDT")


## Examine linearity
ggplot(df_all, aes(x= access, y= ml_rdt_pos)) +
  geom_point()+
  geom_smooth(method = 'lm')

ggplot(df_all, aes(x= net_use, y= ml_rdt_pos)) +
  geom_point()+
  geom_smooth(method = 'loess', span= 0.5) #there seems to be no linear relationship

ggplot(df_all, aes(x= access, y= ml_rdt_pos)) +
  geom_point()+
  geom_smooth(method = 'loess', span= 0.5) #there seems to be no linear relationship

ggplot(df_all, aes(x= wealth, y= ml_rdt_pos)) +
  geom_point()+
  geom_smooth(method = 'lm')



####  Run the models ####
### Fully adjusted zero-inflation Poisson model

#using year as a continuous variable
#model 1-  Without splines
mod1 <- glmmTMB(num_pos_rdt~ net_use + access +
                  housing_q + wealth + precip_01 + EVI + RH + year +
                  offset(log_num_tested) +
                  (1 | country), data= df_all, ziformula=~1, family=poisson)

summary(mod1)
confint(mod1, level= 0.95) # confidence limits



# Diagnostics
simulationOutput <- simulateResiduals(fittedModel = mod1, n= 100, plot= F)
residuals <- testOutliers(simulationOutput, type= "bootstrap", nBoot= 100, plot= T)

pdf(paste0(results_dir, "residual_diagnostic_plots_03.11.24.pdf"))
plot(simulationOutput) #Q-Q plot
dev.off()



# effect plots
df1_pred <- ggeffects::ggpredict(mod1, terms= "year[all]", condition = c(log_num_tested= 2)) #condition- constant leave as the mean log number of children tested.
df1_pred   #Counts of children positive 
names(df1_pred)
col <- c("predicted", "conf.low", "conf.high")
df1_pred[,col] <- df1_pred[,col]/2   #To get the rates divide by 2 (constant above)

setwd(plots_out)

pdf('pred_rate_by_year.pdf')
plot(df1_pred)+
  labs(title = "")
dev.off()

getwd()


library(patchwork)
#Best plot that fits the data is with 7 df, 
#with shading 95% CI
ggplot(df1_pred, aes(x)) + 
  geom_line(aes(y= predicted), colour= "blue")+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.2)+ 
 # geom_point(data= avg_prop_pos_year, aes(year, avg_pos))+ #include actual data, pfPR 
  ylim(0,1)+
  xlab("Year")+
  scale_x_continuous(n.breaks=12, labels=c('2010', '2011', '2012', '2013', '2014', '2015', '2016',
                                           '2017', '2018', '2019', '2020', '2021'))+
  ylab("Expected test positivity rate")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))





# Actual Data
#Average Number positive per cluster 
count(df_all, "country_year")
clust_per_yr <- df_all %>% 
  group_by(year) %>% 
  count("country_year") #number of clusters per year

pos_per_clust <- function(row){ 
  n_pos_clust <- df_all %>% 
  group_by(year) %>% 
  mutate(n_pos_cluster= sum(num_pos_rdt)/ clust_per_yr[row, 3])
  n_pos_clust
}
n_pos_by_cls_2010 <- pos_per_clust(1)

#Average proportion positive by year. 
n_pos_clust <- df_all %>% 
  group_by(year) %>% 
  mutate(avg_pos_cluster = sum(num_pos_rdt)/ sum(num_tested))

avg_prop_pos_year <- tibble(year= 10:21, avg_pos= c(0.272, 0.182, 0.090, 0.054, 0.138, 0.093, 
                                      0.123, 0.090, 0.168, 0.038, 0.029, 0.143))




