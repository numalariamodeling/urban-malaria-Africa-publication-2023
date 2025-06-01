# -----------------------------------------
### Required functions and settings
## -----------------------------------------

# # Reading in the necessary packages 
list.of.packages <- c("ggplot2", "reshape2", "ggpubr", "wesanderson", "haven", "tidyr", "dplyr",
                      "questionr", "data.table", "plyr", "purrr", "forcats", "survey", "stringr", 
                      "patchwork", "viridis", "zoo", "rdhs", "scales", "Kendall", "lemon", "sp",
                      "rgeos", "sf", "tmap", "blscrapeR")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


lapply(list.of.packages, library, character.only = TRUE) #applying the library function to packages


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


#itn proportion trend fun
itn_prop_trend.fun <- function(df){
  ggplot(df) +
    geom_line(aes(x = year_survey2, y= rur_itn_prop), size = 1, color = "hotpink4") +
    geom_point(aes(x = year_survey2, y= rur_itn_prop), size = 1) +
    geom_line(aes(x = year_survey2, y= urb_itn_prop), size = 1, color = "darkcyan") +
    geom_point(aes(x = year_survey2, y= urb_itn_prop), size = 1) +
    theme_bw()+
    labs (title = df[1,1:1], x = "", y = "") +
    ylab("") + xlab("") +
    ylim(0, 1)+
    xlim(2011, 2022)+
    theme_manuscript()  +
    #theme(legend.position="right") +
    scale_x_continuous(breaks= pretty_breaks())
}

#itn proportion trend fun
itn_prop_trend.fun1 <- function(df){
  net1<-df$total_nets
  df$nets<-df$total_nets/max(net1,na.rm = T)
  ggplot(df) +
    geom_area(aes(x=year_survey2,y=nets),alpha=0.4)+
    geom_line(aes(x = year_survey2, y= final_interp_prop),color = "hotpink4") +
    geom_point(aes(x = year_survey2, y= urb_itn_prop),color = "hotpink4",size=2) +
    geom_line(aes(x = year_survey2, y= 1-final_interp_prop), color = "darkcyan") +
    geom_point(aes(x = year_survey2, y= 1-urb_itn_prop), color = "darkcyan",size=2) +
    #geom_vline(aes(xintercept = md_year))+
    theme_bw()+
    labs (title = df[1,1:1], x = "", y = "") +
    scale_y_continuous(name="",sec.axis = sec_axis(~ .*max(net1,na.rm = T), name=""))+
    xlab("") +
    xlim(2010, 2021)+
    theme_manuscript()  #+
    #theme(legend.position="right") +
    #scale_x_continuous(breaks= pretty_breaks())
}

#data loading cleaning pre-extracted itn csv

cleaner.fun.itn <- function(csv_name){
  
  read.csv(file.path(data_dir, csv_name), header = T, sep = ',') %>%
    mutate(year_survey = ifelse(country == 'Zambia' & year_survey == 2014, 2013, year_survey)) %>%
    mutate(year_survey = ifelse(country == 'Malawi' & year_survey == 2016, 2015, year_survey)) %>%
    mutate(year_survey = ifelse(country == 'Rwanda' & year_survey == 2008, 2007, year_survey)) %>%
    mutate(year_survey = ifelse(country == 'Rwanda' & year_survey == 2011, 2010, year_survey)) %>%
    mutate(year_survey = ifelse(country == 'Rwanda' & year_survey == 2015, 2014, year_survey)) %>%
    mutate(year_survey = ifelse(country == 'Rwanda' & year_survey == 2020, 2019, year_survey)) %>%
    mutate(year_survey = ifelse(country == 'Angola' & year_survey == 2016, 2015, year_survey))%>%
    mutate(year_survey = ifelse(country == 'Congo Democratic Republic' & year_survey == 2014, 2013, year_survey))%>%
    mutate(year_survey = ifelse(country == 'Benin' & year_survey == 2018, 2017, year_survey))%>%
    mutate(year_survey = ifelse(country == 'Gambia' & year_survey == 2020, 2019, year_survey))%>%
    mutate(year_survey = ifelse(country == 'Tanzania' & year_survey == 2008, 2007, year_survey))%>%
    mutate(year_survey = ifelse(country == 'Tanzania' & year_survey == 2012, 2011, year_survey))%>%
    mutate(year_survey = ifelse(country == 'Senegal' & year_survey == 2011, 2010, year_survey))%>%
    mutate(year_survey = ifelse(country == 'Senegal' & year_survey == 2013, 2012, year_survey))%>%
    mutate(year_survey = ifelse(country == 'Uganda' & year_survey == 2015, 2014, year_survey))%>%
    mutate(year_survey = ifelse(country == 'Zimbabwe' & year_survey == 2011, 2010, year_survey))%>%
    mutate(year_survey2 = year_survey) %>% mutate(name_text = paste(country, year_survey2, sep=" ")) %>%
    mutate(country = ifelse(country== "C<f4>te dlvoire","Cote DIvoire", country)) %>%
    mutate(country = ifelse(country== "Sierra-Leone","Sierra Leone", country)) 
  
}

##plot pfpr vs net ownership plot fun

p_pfpr_itn.fun <- function(df, xval, yval, data_source, point_label, p_title, legend_show){
  
  df %>% ggplot(aes_string(x=xval, y=yval)) +
    geom_point(alpha=0.6, aes_string(shape = data_source, size = 2, color = "SubregionName")) +
    geom_text(aes_string(label=point_label),vjust=-.8,nudge_y = 0.005,hjust=-0.1, size = 3)+
    scale_fill_viridis(discrete=TRUE, guide=FALSE, option="D") +
    theme_manuscript() +
    theme(legend.position=legend_show) +
    labs(x = "ITN ownership", y = "PfPR", title = p_title) +
    geom_abline(intercept =0 , slope = 1, size = 1.1)+
    xlim(c(0, 1)) + ylim(c(0, 1))+
    guides(fill = guide_legend(), size = guide_legend())
}


#dhs ids

ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode", "SubregionName", "RegionName")) %>% 
  filter(RegionName == "Sub-Saharan Africa")


#####pfpr trend plots
colorsp <- c("All urban areas"= "royalblue1", "Largest Urban center"= "limegreen", "All other urban areas"= "orange")

pfpr_trend.fun <- function(df){
  ggplot(df) +
    #geom_point(aes(x = year_survey2, y= rdt_og_urb), color = "orange", size = 1) +
    #geom_point(aes(x = year_survey2, y= rdt_og_rur), color = "orange", size = 1) +
    geom_line(aes(x = year_survey2, y= final_pfpr_city), color = "limegreen") +
    geom_point(aes(x = year_survey2, y= city_pfpr), color = "limegreen", size = 1) +
    geom_line(aes(x = year_survey2, y= final_pfpr_oth), color = "orange") +
    geom_point(aes(x = year_survey2, y= other_urb_pfpr), color = "orange", size = 1) +
    
    geom_line(aes(x = year_survey2, y= final_pfpr_urb), color = "royalblue1") +
    geom_point(aes(x = year_survey2, y= urban_pfpr), color = "royalblue1", size = 1) +
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

# plots by wealth category
pfpr_trend_w.fun <- function(df){
  ggplot(df) +
    geom_line(aes(x = year_survey2, y= final_pfpr_ur_nweal), color = "#ef8a62") +
    geom_point(aes(x = year_survey2, y= pfpr_notwealthy), color = "#ef8a62", size = 1) +
    
    geom_line(aes(x = year_survey2, y= final_pfpr_ur_weal), color = "royalblue1") +
    geom_point(aes(x = year_survey2, y= pfpr_wealthy), color = "royalblue1", size = 1) +
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

#####urb itn - pfpr trend plots
urb_trend.fun <- function(df){
  ggplot(df) +
    geom_line(aes(x = year_survey2, y= pfpr_city), size = 1, color = "limegreen") +
    geom_point(aes(x = year_survey2, y= test_result_urban), size = 1.5) +
    geom_line(aes(x = year_survey2, y= final_interp_prop), size = 1, color = "royalblue1") +
    geom_point(aes(x = year_survey2, y= urb_itn_prop), size = 1.5) +
    theme_bw()+
    labs (title = paste("Urban", df[1,1:1]), x = "", y = "") +
   # ylab("") + xlab("year") +
    ylim(0, 1)+
    xlim(2011, 2022)+
    theme_manuscript()  +
    #theme(legend.position="right") +
    scale_x_continuous(breaks= pretty_breaks())
}


#####city itn - pfpr trend plots
city_trend.fun <- function(df){
  ggplot(df) +
    geom_line(aes(x = year_survey2, y= final_interp_pfpr), size = 1, color = "limegreen") +
    geom_point(aes(x = year_survey2, y= test_result), size = 1.5) +
    geom_line(aes(x = year_survey2, y= (1-final_interp_prop)), size = 1, color = "royalblue1") +
    geom_point(aes(x = year_survey2, y= (1-urb_itn_prop)), size = 1.5) +
    theme_bw()+
    labs (title = paste("city", df[1,1:1]), x = "", y = "") +
    #ylab("") + xlab("year") +
    ylim(0, 1)+
    xlim(2011, 2022)+
    theme_manuscript()  +
    #theme(legend.position="right") +
    scale_x_continuous(breaks= pretty_breaks())
}

#####overall
overall_trend.fun <- function(df, title_lab){
  ggplot(df) +
    geom_line(aes(x = year_survey2, y= pfpr_city), size = 1, color = "green3") +
    geom_line(aes(x = year_survey2, y= final_interp_pfpr), size = 1, color = "royalblue1") +
    theme_bw()+
    labs (title = title_lab, x = "Year", y = "PfPR") +
    #ylab("") + xlab("year") +
    #ylim(0, 1)+
    xlim(2011, 2022)+
    theme_manuscript()  +
    theme(legend.position="right") +
    scale_x_continuous(breaks= pretty_breaks())
}


#####pfpr trend plots
itn_cost_trend.fun <- function(df){
  ggplot(df) +
    geom_line(aes(x = year, y= final_interp_itn, color = "Interpolated")) +
    geom_point(aes(x = year, y= dollar_per_itn, color = "Infered from MoPs"), size = 1.5) +
    theme_bw()+
    labs (x = "Year", y = "ITN procurement and distribution unit cost ($)") +
    xlim(2011, 2022)+
    ylim(0, 20)+
    facet_wrap(~country) +
    theme_manuscript()  +
    #theme(legend.position="right") +
    scale_x_continuous(breaks= pretty_breaks())+
    scale_color_manual(name = "", 
                       values = c("Interpolated" = "darkcyan", "Infered from MoPs" = "firebrick4" ))
}


#itn_prop annotate function
itn_prop_annot.fun <- function(plots_unannoted) {
  to_annotate <- ggarrange(plotlist=plots_unannoted)
  annotate_figure(to_annotate, bottom = text_grob("Year", color = "Black", size = 14),
                  left = text_grob("Proportion of ITN in urban and rural areas", color = "Black", size = 14,  rot = 90),
                  right = text_grob("Total ITN distributed nationwide (millions)", color = "Black", size = 14,  rot = -90))
}


#annotate function

annotate.fun <- function(plots_unannoted) {
  to_annotate <- ggarrange(plotlist=plots_unannoted)
  annotate_figure(to_annotate, bottom = text_grob("Year", color = "Black", size = 14),
                  left = text_grob("ITN procurement and distribution cost ($)", color = "Black", size = 14,  rot = 90))
}


#annotate function

t1 <- text_grob("All urban areas", color= "royalblue1", hjust= 1,vjust= 0.2, size=11)
t2 <-  text_grob("Largest Urban center", color= "limegreen", hjust= 1,vjust= 0.1, size=11)
t3 <- text_grob("All other urban areas", color= "orange", hjust= 1,vjust= 0, size=11)

#Legend not appearing
pfpr_annotate.fun <- function(plots_unannoted) {
  to_annotate <- ggarrange(plotlist=plots_unannoted)
  annotate_figure(to_annotate, bottom= text_grob("Year", color = "black", size = 14),
                  right = c(text_grob("Wealthy households", color= "royalblue1", hjust= 1,vjust= 0.2, size=11),
                            text_grob("Not wealthy households", color= "#ef8a62", hjust= 1,vjust= 0.1, size=11)),
                  left = text_grob("Prevalence in children under 5", color = "black", size = 14,  rot = 90)
                  )
}

#####pfpr trend plots
pfpr_itn_cost_trend.fun <- function(df){
  ggplot(df) +
    geom_line(aes(x = year_survey2, y= prop_rur_cost), size = 1, color = "limegreen") +
    geom_line(aes(x = year_survey2, y= prop_urb_cost), size = 1, color = "cyan4") +
    geom_line(aes(x = year_survey2, y= pfpr_rur), size = 1, color = "darkorange") +
    geom_line(aes(x = year_survey2, y= final_interp_pfpr), size = 1, color = "darkorange4") +
    theme_bw()+
    labs (title = df[1,1:1], x = "", y = "") +
    ylab("") + xlab("") +
    xlim(2011, 2022)+
    ylim(0, 1)+
    theme_manuscript()  +
    #theme(legend.position="right") +
    scale_x_continuous(breaks= pretty_breaks())
}


#annotate function

pfpr_itn_annotate.fun <- function(plots_unannoted) {
  to_annotate <- ggarrange(plotlist=plots_unannoted)
  annotate_figure(to_annotate, bottom = text_grob("Year", color = "Black", size = 14),
                  left = text_grob("PfPR/proportion of expense on ITNs", color = "Black", size = 14,  rot = 90))
}

#trend analyis using regression 
lm_plot.fun <- function(df){
  ggplot(df) + 
    geom_smooth(aes(x = df$year_survey2, y =df$final_interp_pfpr),method="lm",se=TRUE,level=0.95,alpha=0.5,fill="blue",span = 0.5) + 
    geom_smooth(aes(x = df$year_survey2, y =df$final_interp_pfpr), method="glm",col="blue",se=FALSE,alpha=0.5)+
    #geom_line(data=df,aes(x = df$year_survey2, y =df$final_interp_pfpr),col="black")+
    annotate("text",x=-0.2,y=0.2,label=(paste0("slope==",coef(lm(df$final_interp_pfpr~df$year_survey2))[2])),parse=TRUE)+
    
    
    geom_smooth(aes(x = df$year_survey2, y =df$pfpr_rur),method="lm",se=TRUE,level=0.95,col="purple",fill="green3",span = 0.5) + 
    geom_smooth(aes(x = df$year_survey2, y =df$pfpr_rur), method="glm",col="green3",se=FALSE)+
    annotate("text",x=-0.3,y=0.1,label=(paste0("slope==",coef(lm(df$pfpr_rur~df$year_survey2))[2])),parse=TRUE)+
    
    #geom_line(data=df,aes(x = df$year_survey2, y =df$pfpr_rur),col="black")+
    #theme(panel.background = element_rect(fill='white', colour='black'))+
    theme_manuscript() +
    #labs (title = df[1,1:1], x = "", y = "") +
    ylab("") + xlab("") +
    #facet_wrap(~country) +
    ylim(0, 0.3)+
    xlim(2011, 2020)+
    scale_x_continuous(breaks= pretty_breaks())
}


#pfpr reg annotate fun

pfpr_annotate.fun2 <- function(plots_unannoted, title) {
  to_annotate <- ggarrange(plotlist=plots_unannoted)
  annotate_figure(to_annotate, top = text_grob(title,  color = "Black", size = 18), 
                  bottom = text_grob("Year", color = "Black", size = 14),
                  left = text_grob("PfPR", color = "Black", size = 14,  rot = 90))
}


#regional mean check function
mcheck_plot.fun <- function(df, var, title_lab, x_lab, y_lab){
  ggplot(df) +
    geom_histogram(aes_string(x = var), color = "blue3", fill = "darkcyan")+
    ylim(0, 45)+
    theme_manuscript()  +
    labs(title = title_lab, x = x_lab, y = y_lab)
}

#itn proportion trend fun
acr_rdt_trend.fun <- function(df, title_lab){
  ggplot(df) +
    geom_line(aes(x = costing_year, y= final_interp_unit_cost_act, color = "ACT")) +
    geom_point(aes(x = costing_year, y= unit_cost_act), size = 0.7) +
    geom_line(aes(x = costing_year, y= final_interp_unit_cost_rdt, color = "RDT")) +
    geom_point(aes(x = costing_year, y= unit_cost_rdt, color = "Uninterpolated"), size = 0.7) +
    theme_bw()+
    facet_wrap(~country) +
    labs (title = title_lab, x = "", y = "") +
    ylab("Unit cost") + xlab("Year") +
    #ylim(0, 1)+
    xlim(2011, 2020)+
    theme_manuscript()+
    #theme(legend.position="bottom") +
    scale_x_continuous(breaks= pretty_breaks())+
    scale_color_manual(name = "Unit cost", 
                       values = c("ACT" = "blue", "RDT" = "red", "Uninterpolated" = "black" ))
}


#overall trend in unit cost
average_trens.fun <- function(df, var1, var2, title_lab, x_lab){
  ggplot(df)+
    geom_point(aes_string(x = "costing_year", y= var1), color = "peachpuff3", alpha = 0.5) +
    geom_line(aes_string(x = "costing_year", y= var2), color = "firebrick4", size = 0.66) +
    theme_bw()+
    labs (title = title_lab, x = "Costing year", y = x_lab) +
    #ylab("") + 
    xlab("Year") +
    ylim(0, 6.5)+
    xlim(2011, 2020)+
    theme_manuscript() +
    scale_x_continuous(breaks= pretty_breaks())
  
}

#urban nets distributed
act_rdt_total.fun <- function(df, fill_col, x_lab, y_lab, title_lab){
  ggplot(df , aes(x=reorder(country,x),y=x))+
    geom_col(fill = fill_col)+
    #geom_text(aes(label=country),vjust=0.5,nudge_y = 0.005,hjust=0)+
    coord_flip()+
    labs(x=x_lab, y= y_lab, title = title_lab)+
    scale_y_continuous(expand=c(0,0))+
    theme(axis.text.y=element_blank(),axis.line.y = element_blank(),
          axis.ticks.y = element_blank())+theme_classic2()+theme_manuscript()
  
}

