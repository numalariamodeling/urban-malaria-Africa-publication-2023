
## -----------------------------------------
### Directories
## -----------------------------------------

user <- Sys.getenv("USERNAME")

if ("ido0493" %in% user) {
  user_path <- file.path("C:/Users", user)
  DriveDir <- file.path(user_path, "OneDrive - Northwestern University", "urban_malaria")
  PopDir <- file.path(DriveDir, "data", "Urban_malaria_net_ownership_data")
  ResultsDir <- file.path(PopDir, "Extracted_csv")
} else if  ("pc" %in% user) {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  DriveDir <- file.path(Drive, 'Northwestern University', 'Ifeoma Doreen Ozodiegwu - urban_malaria')
  PopDir <- file.path(DriveDir, "data", 'Urban_malaria_net_ownership_data')
  data_dir <- file.path(PopDir, "Extracted_csv")
  ResultsDir <- file.path(PopDir, "Extracted_csv")
  
  #temp
  #data_dir <- file.path(Drive, "Documents","Personal Onedrive", "OneDrive", "Extracted_csv")
}else if ("Colleen" %in% user) {
  
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  DriveDir <- file.path(Drive, 'NU-malaria-team Dropbox')
  PopDir <- file.path(DriveDir, "data", 'Urban_malaria_net_ownership_data')
  data_dir <- file.path(PopDir, "Extracted_csv")
  ResultsDir <- file.path(DriveDir, "projects", "urban_malaria", "urban_malaria_net_ownership", "plots", "plots_colleen")
  
}else{
  
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  DriveDir <- file.path(Drive, 'OneDrive - Northwestern University', 'urban_malaria')
  PopDir <- file.path(DriveDir, "data", 'Urban_malaria_net_ownership_data')
  data_dir <- file.path(PopDir, "Extracted_csv")
  ResultsDir <- file.path(PopDir, "Extracted_csv")
  
}

#functions 
source("functions/functions_plots.R")
setwd("C:/Users/Colleen/OneDrive/Documents/1. Northwestern Malaria Modeling Group/Urban_nets/Code")

#loading required data: act use data
rdt_act_cost_df <- read.csv(file.path(data_dir,"rdt_act_cost_updated.csv"), header = T, sep = ',') %>% 
  #mutate_all(na_if(x,""))%>% 
  mutate_at(c(2:6), as.numeric) %>% 
  mutate(unit_cost_rdt = rdt_cost/total_rdts) %>% mutate(unit_cost_act = act_cost/total_acts) %>% 
  left_join(ids, by =c("country" = "CountryName")) %>%
  mutate(SubregionName = ifelse(country == "Cabo Verde", "Western Africa", SubregionName)) %>% 
  mutate(SubregionName = ifelse(country == "Congo-B", "Middle Africa", SubregionName)) %>% 
  mutate(SubregionName = ifelse(country == "Guinea-Bissau", "Western Africa", SubregionName))%>% 
  mutate(SubregionName = ifelse(country == "Sierra-Leone", "Western Africa", SubregionName))%>% 
  mutate(SubregionName = ifelse(country == "South Sudan", "Eastern Africa", SubregionName))

####################################################
#Data merging and interpolation 
####################################################
#spliting df
df_list <- split(rdt_act_cost_df, with(rdt_act_cost_df, interaction(country)), drop = TRUE)

fill.act.rdt.fun <- function(dat){
    #total_acts 
  dat %>% arrange(country,costing_year) %>%
    mutate(total_acts_dfill = na.locf0(total_acts, fromLast = TRUE)) %>%
    mutate(total_acts_ufill = na.locf0(total_acts, fromLast = FALSE)) %>%
    mutate(total_acts_lifill = na.approx(total_acts, na.rm = FALSE)) %>%
    mutate(final_interp_total_acts = ifelse(is.na(total_acts_lifill), total_acts_ufill, total_acts_lifill))%>%
    mutate(final_interp_total_acts = ifelse(is.na(final_interp_total_acts), total_acts_dfill, final_interp_total_acts))%>%
    #total_rdts 
    mutate(total_rdts_dfill = na.locf0(total_rdts, fromLast = TRUE)) %>%
    mutate(total_rdts_ufill = na.locf0(total_rdts, fromLast = FALSE)) %>%
    mutate(total_rdts_lifill = na.approx(total_rdts, na.rm = FALSE)) %>%
    mutate(final_interp_total_rdts = ifelse(is.na(total_rdts_lifill), total_rdts_ufill, total_rdts_lifill))%>%
    mutate(final_interp_total_rdts = ifelse(is.na(final_interp_total_rdts), total_rdts_dfill, final_interp_total_rdts)) %>% 
    #mutate(unit_cost_rdt = rdt_cost/total_rdts) %>% mutate(unit_cost_act = act_cost/total_acts)%>%
    #filter(year_survey2 > 2010) %>% mutate(nastotal_rdts = sum(is.na(total_rdts)))
    #act unit cost
    mutate(unit_cost_act_dfill = na.locf0(unit_cost_act, fromLast = TRUE)) %>%
    mutate(unit_cost_act_ufill = na.locf0(unit_cost_act, fromLast = FALSE)) %>%
    mutate(unit_cost_act_lifill = na.approx(unit_cost_act, na.rm = FALSE)) %>%
    mutate(final_interp_unit_cost_act = ifelse(is.na(unit_cost_act_lifill), unit_cost_act_ufill, unit_cost_act_lifill))%>%
    mutate(final_interp_unit_cost_act = ifelse(is.na(final_interp_unit_cost_act), unit_cost_act_dfill, final_interp_unit_cost_act)) %>%
    #RDT unit cost
    mutate(unit_cost_rdt_dfill = na.locf0(unit_cost_rdt, fromLast = TRUE)) %>%
    mutate(unit_cost_rdt_ufill = na.locf0(unit_cost_rdt, fromLast = FALSE)) %>%
    mutate(unit_cost_rdt_lifill = na.approx(unit_cost_rdt, na.rm = FALSE)) %>%
    mutate(final_interp_unit_cost_rdt = ifelse(is.na(unit_cost_rdt_lifill), unit_cost_rdt_ufill, unit_cost_rdt_lifill))%>%
    mutate(final_interp_unit_cost_rdt = ifelse(is.na(final_interp_unit_cost_rdt), unit_cost_rdt_dfill, final_interp_unit_cost_rdt))
  
}

cntry_interp_list <- lapply(df_list, fill.act.rdt.fun) 
cntry_interp_list <- cntry_interp_list %>% 
  map(~mutate(., 
              final_interp_total_rdts = as.numeric(final_interp_total_rdts, final_interp_total_acts = as.numeric(final_interp_total_acts))))%>%
  map(~select(., country, costing_year, SubregionName, total_acts, final_interp_total_acts, total_rdts, final_interp_total_rdts,
              unit_cost_rdt, final_interp_unit_cost_rdt, final_interp_unit_cost_act, unit_cost_act))

all_var_interp_df <- bind_rows(cntry_interp_list)# %>% filter(nas < 10)   

#exploring regional mean
rgn_interp_mean <- all_var_interp_df %>% group_by(costing_year, SubregionName) %>% 
  summarise_at(vars(unit_cost_rdt, unit_cost_act), funs(mean(., na.rm=TRUE))) %>% 
  select(SubregionName, unit_cost_rdt, unit_cost_act) %>% 
  rename_at(3, ~ "rmean_rdt") %>% rename_at(4, ~ "rmean_act") %>%arrange(SubregionName,costing_year)%>% 
  mutate(rmean_rdt = na.approx(rmean_rdt, na.rm = FALSE))%>%
  mutate(rmean_rdt = na.locf0(rmean_rdt))%>% 
  mutate(rmean_act = na.approx(rmean_act, na.rm = FALSE))%>%
  mutate(rmean_act = na.locf0(rmean_act)) 

final_df <- all_var_interp_df %>% inner_join(rgn_interp_mean, by = c("costing_year","SubregionName"))%>% mutate(rdt_dif = rmean_rdt - unit_cost_rdt) %>% 
  mutate(act_dif = rmean_act - unit_cost_act)

#check regional mean
range(final_df$unit_cost_act, na.rm = T)
act_dif_p <- mcheck_plot.fun(final_df, "act_dif", "ACT - center around ± $0.25",
                             "ACT unit cost differences between countries and regional means", "Frequency")
range(final_df$unit_cost_rdt, na.rm = T)
rdt_dif_p <- mcheck_plot.fun(final_df, "rdt_dif", "RDT - center around ± $0.20",
                             "RDT unit cost differences between countries and regional means", "")
act_dif_p + rdt_dif_p
#
final_df1 <- final_df %>% 
  mutate(final_interp_unit_cost_act = ifelse(is.na(final_interp_unit_cost_act), rmean_act, final_interp_unit_cost_act))%>% 
  mutate(final_interp_unit_cost_rdt = ifelse(is.na(final_interp_unit_cost_rdt), rmean_rdt, final_interp_unit_cost_rdt))


#plots
region_list <- split(final_df1, with(final_df1, interaction(SubregionName)), drop = TRUE) %>% 
  map(~bind_rows(.,))

acr_rdt_trend.fun(region_list[[1]], "Country level interpolated trend of ACT and RDT unit cost in the eastern region")
acr_rdt_trend.fun(region_list[[2]], "Country level interpolated trend of ACT and RDT unit cost in the middle region")
acr_rdt_trend.fun(region_list[[3]], "Country level interpolated trend of ACT and RDT unit cost in the western region")

#overall
overall_average <- final_df1 %>% group_by(costing_year) %>% 
  summarise_at(vars(final_interp_unit_cost_act, final_interp_unit_cost_rdt), funs(mean(., na.rm=TRUE))) %>%
  right_join(final_df1, by = "costing_year")

act_av_p <- average_trens.fun(overall_average, "final_interp_unit_cost_act.y", "final_interp_unit_cost_act.x",
                              "ACT average unit cost", "Average procurement cost (USD)")
rdt_av_p <- average_trens.fun(overall_average, "final_interp_unit_cost_rdt.y", "final_interp_unit_cost_rdt.x",
                              "RDT average unit cost", "")

act_av_p + rdt_av_p


#Agregate plots

df_act_c <- aggregate(final_df1$final_interp_total_act, by=list(country=final_df1$country), FUN=sum)%>%
  filter(x > 0)
df_act_r <- aggregate(final_df1$final_interp_total_rdt, by=list(country=final_df1$country), FUN=sum)%>%
  filter(x > 0)

#act total
act_rdt_total.fun(df_act_c, "seagreen", "Countries with data","ACT supplies",
                  "Total ACTs purchased per country from 2011 to 2021")

act_rdt_total.fun(df_act_r, "firebrick4", "Countries with data","RDT supplies",
                  "Total RDTs purchased per country from 2011 to 2021")

final_df1$costing_year<-as.factor(final_df1$costing_year)

wmr22 <- read.csv(file.path(data_dir,"wmr22_rdt_act.csv"), header = T, sep = ',') %>% 
  rename_at(1, ~'country') %>% 
  mutate(year=as.factor(year)) %>%
  left_join(final_df1, by =c("country","year"="costing_year")) %>%
  mutate(rdt_funds = final_interp_unit_cost_rdt * rdt_dist) %>% mutate(act_funds = final_interp_unit_cost_act * act_courses_del) %>%
  mutate(rdt_prop = rdt_funds/(rdt_funds+act_funds)) %>% mutate(act_prop = act_funds/(rdt_funds+act_funds)) %>%
  select(country,rdt_funds,act_funds,rdt_prop,act_prop, final_interp_unit_cost_rdt,final_interp_unit_cost_act) %>%
  rename_at(6,~'rdt_wmr') %>%
  rename_at(7,~'act_wmr')

ggplot(wmr22)+geom_point(aes(x=rdt_wmr,y=rdt_prop,color='RDT'),size=2)+
 geom_text(aes(label=country,x=rdt_wmr,y=rdt_prop,color='RDT'),vjust=0.5,nudge_y = 0.005,hjust=0,show.legend = F)+
 geom_point(aes(x=act_wmr,y=act_prop,color='ACT'),size=2)+
 geom_text(aes(label=country,x=act_wmr,y=act_prop,color='ACT'),vjust=0.5,nudge_y = 0.005,hjust=0,show.legend = F)+

 labs(x='Unit Cost (USD)',y='Proportion of Funds Allotted')+
 scale_color_manual(name="Intervention",values=c('RDT'='darkblue','ACT'= 'pink3'))

unhabitat <- read.csv(file.path(data_dir,"prop_urb_slums.csv"),check.names = F) %>% #Urban_Population_Living_in_Slums.csv
  rename_at(1, ~'Region') #%>%
unhabitat_m<-reshape2::melt(unhabitat,value.name = 'u_slum_prop', id=c('Region','country'),variable.name='costing_year')

reall<- unhabitat_m %>% arrange(country,costing_year) %>%
  right_join(final_df1, by =c("country","costing_year")) %>%
  group_by(country) %>%
  left_join(wmr22,by=c('country'))

urb_pop<-read.csv(file.path(data_dir,"urb_pop.csv"), header = T, sep = ',',check.names = F) %>%
  rename_at(1, ~'country')
rur_pop<-read.csv(file.path(data_dir,"rur_pop.csv"), header = T, sep = ',',check.names = F) %>%
  rename_at(1, ~'country')
urb_pop_m<-reshape2::melt(urb_pop,value.name = 'u_pop', id=c('country','country_code'),variable.name='costing_year')
rur_pop_m<-reshape2::melt(rur_pop,value.name = 'r_pop', id=c('country','Country Code'),variable.name='costing_year') %>%
  rename_at(2,~'country_code')
test <- urb_pop_m %>% left_join(rur_pop_m,by =c("country","costing_year","country_code"))%>%
  arrange(country,costing_year) %>%
  right_join(reall, by =c("country","costing_year")) %>%
  mutate(u_slum_n = u_slum_prop * u_pop/100) %>%
  mutate(u_nets_need = ceiling(u_slum_n/2)) %>%
  mutate(r_nets_need = ceiling(r_pop/2))

itn_costs<-read.csv(file.path(data_dir,"intcost_inerp_df.csv"))%>%
  select(c('country','costing_year','final_interp_itn','rgnl_mean_itn')) %>%
  mutate(year_survey2=as.factor(costing_year))

nets <- read.csv(file.path(data_dir,"final_interp_df.csv"), header = T, sep = ',',check.names = F) %>%
  select(-c(1)) %>%
  mutate(year_survey2 = as.factor(year_survey2)) %>%
  right_join(test, by=c("country","year_survey2" = "costing_year","SubregionName")) %>%
  mutate(leftovers = floor(final_ur_nets) - u_nets_need) %>%
  filter(!is.na(leftovers))
  

nets$redist<-NA
nets$redist<-ifelse(is.na(nets$leftovers) | nets$leftovers<=0,yes = FALSE, no = TRUE)

nettime<-ggplot(nets)+geom_line(aes(x=year_survey2,y=leftovers/1000000,color=SubregionName,group=country))+
  #ylim(-5000000,5100000)+
  geom_hline(yintercept = 0)+
  ylab('Redistributable Nets (millions)')+
  xlab('Year')+
  labs(color='Region')
  #facet_wrap(~country)

pdf(paste(ResultsDir,"/netsovertime.pdf",sep = ""), height = 7, width = 10)
nettime
dev.off()
png(paste(ResultsDir,"/netsovertime.png",sep = ""), height = 7, width = 10, units = "in", res = 300)
nettime
dev.off()

ur_rur_pfpr<-read.csv(file.path(data_dir,"ur_rur_pfpr.csv"))%>%select(country,year_survey2,pfpr_urb)%>%
  mutate(year_survey2=as.factor(year_survey2))

redist_df<- nets %>%
  filter(redist==TRUE) %>% 
  left_join(itn_costs) %>%
  mutate(net_funds=leftovers*final_interp_itn) %>%
  mutate(redist_rdts = floor(net_funds * rdt_prop / final_interp_unit_cost_rdt)) %>%
  mutate(redist_acts = floor(net_funds * act_prop / final_interp_unit_cost_act)) %>%
  mutate(r_add_need = r_nets_need - floor(final_ru_nets)) %>%
  #mutate(r_redist_itns = ifelse(r_add_need<=0,0,leftovers) )
  mutate(r_leftovers = ifelse(r_add_need<=0,leftovers,(leftovers - r_add_need))) %>%
  mutate(r_redist_itns = ifelse(r_leftovers>0,r_add_need,leftovers))%>%
  mutate(r_redist_itns = ifelse(r_add_need<=0,0,r_redist_itns))%>%
  mutate(r_leftovers = ifelse(r_leftovers<=0,0,r_leftovers)) %>%
  mutate(r_net_funds=ifelse(is.na(r_leftovers) | r_leftovers<=0, 0, r_leftovers*final_interp_itn)) %>%
  mutate(r_redist_rdts = floor(r_net_funds * rdt_prop / final_interp_unit_cost_rdt)) %>%
  mutate(r_redist_acts = floor(r_net_funds * act_prop / final_interp_unit_cost_act)) %>%
  left_join(ur_rur_pfpr,by = c('country','year_survey2'))

redist_m <- redist_df %>% select(country,year_survey2,SubregionName,pfpr_urb,total.net.distributed,final_ur_nets,u_nets_need,r_nets_need,leftovers,r_leftovers,rdt_funds,act_funds,rdt_prop,act_prop,net_funds,redist_rdts,redist_acts,r_net_funds,r_redist_itns,r_redist_rdts,r_redist_acts) %>%
  reshape2::melt(value.name = 'redist_num', id=c('country','year_survey2','SubregionName','pfpr_urb','total.net.distributed','final_ur_nets','u_nets_need','r_nets_need','r_leftovers','rdt_funds','act_funds','rdt_prop','act_prop','net_funds','r_net_funds'),variable.name='cm_type') %>%
  filter(!is.na(redist_num))

redistplot<-
  ggplot(redist_m,group=year_survey2)+geom_col(aes(x=(as.character(year_survey2)),y=redist_num/1000000,fill=cm_type),position = position_dodge2(preserve = "single"))+
  scale_fill_viridis(option = "A", discrete=TRUE,direction = -1,begin = 0.25, end= 0.8,name='Commodities',label=c('Redistributable Nets','RDTs','ACTs','ITNs to Rural Areas','RDTs following Rural ITN Distribution','ACTs following Rural ITN Distribution'))+
  xlab('Year')+
  ylab('Redistributed commodities (millions)')+
  facet_wrap(~country)

pdf(paste(ResultsDir,"/redist_commod.pdf",sep = ""), height = 10, width = 18)
redistplot
dev.off()
png(paste(ResultsDir,"/redist_commod.png",sep = ""), height = 10, width = 18, units = "in", res = 300)
redistplot
dev.off()
  
netsavail<-ggplot(redist_df)+geom_col(aes(leftovers/1000000,reorder(name_text,leftovers)))+scale_x_continuous(expand=c(0,0))+
  ylab('Country, Year')+
  xlab('Nets Available for Redistribution (millions)')

pdf(paste(ResultsDir,"/netsavail.pdf",sep = ""), height = 7, width = 10)
netsavail
dev.off()
png(paste(ResultsDir,"/netsavail.png",sep = ""), height = 7, width = 10, units = "in", res = 300)
netsavail
dev.off()

redist_p <- redist_df %>% select(country,year_survey2,SubregionName,total.net.distributed,final_ur_nets,u_nets_need,leftovers,rdt_funds,act_funds,rdt_prop,act_prop,net_funds,redist_rdts,redist_acts) %>%
  group_by(country) %>%
  arrange(year_survey2) %>%
  filter(row_number()==1) %>%
  reshape2::melt(value.name = 'redist_prop', id=c('country','year_survey2','SubregionName','total.net.distributed','final_ur_nets','u_nets_need','leftovers','rdt_funds','act_funds','redist_rdts','redist_acts','net_funds'),variable.name='cm_type') %>%
  filter(!is.na(redist_prop))

commoddist<-ggplot(redist_p, aes(x="", y=redist_prop, fill=cm_type)) +
  geom_bar(stat="identity", width=1, color="white") +
  scale_fill_viridis(option = "A", discrete=TRUE,direction = -1,begin = 0.25, end= 0.55,name='Commodities',label=c('RDTs','ACTs'))+
  coord_polar("y", start=0) +
  theme_void() +
  facet_wrap(~country)

pdf(paste(ResultsDir,"/commoddist.pdf",sep = ""), height = 7, width = 10)
commoddist
dev.off()
png(paste(ResultsDir,"/commoddist.png",sep = ""), height = 7, width = 10, units = "in", res = 300)
commoddist
dev.off()

#table creation
redist_table<-redist_df %>%select(country,year_survey2,SubregionName,leftovers,rdt_prop,act_prop,r_redist_itns,r_net_funds,r_redist_rdts,r_redist_acts,net_funds,redist_rdts,redist_acts,r_leftovers)%>%
  group_by(country)#%>%
  #dplyr::rename('Country'=country,'Year'=year_survey2,'SubRegion'=SubregionName,'Leftover Urban ITNs'=leftovers,'Proportion of Funds for RDTs'=rdt_prop,'Proportion of Funds for ACTs'=act_prop,
  #              'ITNs Redistributed to Rural Areas'=r_redist_itns,'Option 1 Funds'=r_net_funds,'Option 1 Purchased RDTs'=r_redist_rdts,'Option 1 Purchased ACTs'=r_redist_acts,'Option 2 Funds'=net_funds,
  #              'Option 2 Purchased RDTs'=redist_rdts,'Option 2 Purchased ACTs'=redist_acts)
#,total.net.distributed,final_ur_nets,u_nets_need,r_nets_need,r_leftovers,rdt_funds,act_funds,
#write.csv(redist_table,file.path(ResultsDir, "redist_table.csv"))
  
ggplot(redist_df)+geom_line(aes(x=year_survey2,y=leftovers,color=SubregionName,group=country))

redist_ex<-redist_table %>% filter(country=='Cameroon')%>%filter(year_survey2=='2016')
redist_exm<-redist_df %>% select(country,year_survey2,SubregionName,total.net.distributed,final_ur_nets,u_nets_need,r_nets_need,leftovers,r_leftovers,rdt_funds,act_funds,rdt_prop,act_prop,net_funds,redist_rdts,redist_acts,r_net_funds,r_redist_itns,r_redist_rdts,r_redist_acts) %>%
  reshape2::melt(value.name = 'redist_num', id=c('country','year_survey2','SubregionName','total.net.distributed','final_ur_nets','u_nets_need','r_nets_need','rdt_funds','act_funds','rdt_prop','act_prop','net_funds','r_net_funds'),variable.name='cm_type') %>%
  filter(!is.na(redist_num))%>% 
  filter(country=='Cameroon')%>%filter(year_survey2=='2016')

redist_exm$waf<-NA
redist_exm$unit<-NA

for(j in 1:length(redist_exm$cm_type)){
  if(redist_exm$cm_type[j] %in% c('leftovers','r_leftovers','r_redist_itns')){
    redist_exm$waf[j]<-redist_exm$redist_num[j]/50000
    redist_exm$unit[j]<-'50k'
  } else{
    redist_exm$waf[j]<-redist_exm$redist_num[j]/200000
    redist_exm$unit[j]<-'200k'
  }
}

