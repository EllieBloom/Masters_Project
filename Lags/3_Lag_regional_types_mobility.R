# CCF and DTW for different mobility types and regions


# Date started; 21st July 2022



# Setup -------------------------------------------------------------------


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(stringr)
library(dtw)
library(BBmisc)
library(RColorBrewer)

min_max_normalise <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}



# Useful dates ------------------------------------------------------------

lockdown_1_start <- as.Date("2020-03-26","%Y-%m-%d")
lockdown_1_end <- as.Date("2020-06-15","%Y-%m-%d")

lockdown_2_start <- as.Date("2020-11-05","%Y-%m-%d")
lockdown_2_end <- as.Date("2020-12-02","%Y-%m-%d")

lockdown_3_start <- as.Date("2021-01-06","%Y-%m-%d")
lockdown_3_end <- as.Date("2021-04-21","%Y-%m-%d")

REACT_start <-as.Date("2020-05-01","%Y-%m-%d")

# Loading mobility tibble ---------------------------------------------------------

mobility_google <- readRDS("/Users/elliebloom/Desktop/Masters/Project/Analysis/Mapping/Outputs/google_england_long.rds")

# Only want the rolling averages
mobility_av <- mobility_google %>% filter(str_detect(type_mobility, 'av'))

# Defining a list of types of mobility
types_mobility <- c("retail_recreation_av", "grocery_pharmacy_av", "parks_av","transit_stations_av", "workplaces_av","residential_av")

mobility_av <- mobility_av %>% 
  mutate(type_mobility = 
           case_when(type_mobility=="retail_recreation_av"~"Retail & Recreation" ,
                     type_mobility=="grocery_pharmacy_av"~ "Grocery & Pharmacy",
                     type_mobility=="parks_av"~ "Parks",
                     type_mobility=="transit_stations_av"~ "Transit Stations",
                     type_mobility=="workplaces_av"~ "Workplaces",
                     type_mobility=="residential_av"~ "Residential"))


types_mobility <- dput(unique(mobility_av$type_mobility))    
types_mobility

mobility_av <- mobility_av %>% 
  mutate(region = 
           case_when(region=="EAST"~"East of England" ,
                     region=="EAST MIDLANDS"~ "East Midlands",
                     region=="ENGLAND"~ "England",
                     region=="LONDON"~ "London",
                     region=="NORTH EAST"~ "North East",
                     region=="NORTH WEST"~ "North West",
                     region=="SOUTH EAST"~ "South East",
                     region=="SOUTH WEST"~ "South West",
                     region=="WEST MIDLANDS"~ "West Midlands",
                     region=="YORKSHIRE AND THE HUMBER"~ "Yorkshire and The Humber"))


# Load REACT reproduction number data -------------------------------------


# England national level
react_reprod_national <- readRDS("~/Desktop/Masters/Project/Data/REACT_reproduction/National_reproduction_R.rds")
react_reprod_national$reg_char <- "England"


REACT_reprod_start <- as.Date("2020-05-15","%Y-%m-%d")

react_reprod_regional <-readRDS("~/Desktop/Masters/Project/Data/REACT_reproduction/regional_reproduction_R.RDS")

react_reprod_regional <- rbind(react_reprod_regional, react_reprod_national)

region_list <- dput(unique(react_reprod_regional$reg_char))

# CCF ---------------------------------------------------------------------

# Lockdown 3 only

lag_max = 50

ccf_summary <-NA

for (i in 1:length(types_mobility)){
  for (j in 1:length(region_list)){
    start_date = lockdown_3_start
    end_date = lockdown_3_end
    mobility_place = types_mobility[i]
    region_interest = region_list[j]
    mob <- mobility_av %>% filter(region==region_interest, type_mobility==mobility_place, date>=lockdown_3_start, date<=lockdown_3_end)
    reprod <- react_reprod_regional %>% filter(reg_char == region_interest, d_comb>=lockdown_3_start, d_comb<=lockdown_3_end )
    ccf <- ccf(reprod$r , mob$mobility, lag.max=lag_max)
    ccf_data <- as.data.frame(cbind(ccf$acf,ccf$lag))
    colnames(ccf_data)[1:2]<-c("ccf","lag")
    ccf_data$region <- region_interest
    ccf_data$type_mobility <- mobility_place
    ccf_summary <- rbind(ccf_summary, ccf_data)
  }
}

ccf_summary
ccf_summary <- ccf_summary[-1,]

setwd("~/Desktop/Masters/Project/Analysis/Lags/Outputs/regional")
write.csv(ccf_summary,"ccf_summary_regional.csv")

# Summarising min and maxes
ccf_max_min <- ccf_summary %>% group_by(region,type_mobility) %>% summarise(max_ccf = max(ccf),max_lag=(lag[which.max(ccf)]),
                                                                            min_ccf=min(ccf),min_lag=(lag[which.min(ccf)])) 

setwd("~/Desktop/Masters/Project/Analysis/Lags/Outputs/regional")
write.csv(ccf_max_min,"ccf_summary_min_max_regional.csv")

# CCF plot ----------------------------------------------------------------

difftime(lockdown_3_end, lockdown_3_start)+1

n <- 106

confint <- as.data.frame(seq(-50,50,1))

colnames(confint) <- "lag"
confint<- confint %>% 
  mutate(upper_ci = qnorm(0.975)*sqrt(1/(n-abs(lag))),
         lower_ci = -qnorm(0.975)*sqrt(1/(n-abs(lag))))




library(RColorBrewer)

# Matching the map:
# North West - #FB9A99"
# North East - "#33A02C"
# East Midlands - "#A6CEE3"
# South West - "#FDBF6F"
# South East - "#E31A1C"
# Yorkshire and Humber - "#CAB2D6"
# West Midlands - "#FF7F00"
# East of England - "#1F78B4"
# London - "#B2DF8A"
# England - "black"

display.brewer.pal(9, "Paired")
brewer.pal(9, "Paired")

colours <- c("#FB9A99","#CAB2D6", "#33A02C","#FF7F00","#A6CEE3","#1F78B4","#FDBF6F", "#B2DF8A","#E31A1C","black")
thickness <- c(rep(0.5,9),1)


## Faceted by type of mobility

ccf_plot <- ccf_summary %>% filter(!str_detect(type_mobility,"Parks")) %>% #,region!="England"
  ggplot(aes(x=lag))+
    geom_area(data=confint,aes(x=lag,y=upper_ci), fill="light gray", alpha=0.5)+
    geom_area(data=confint,aes(x=lag,y=lower_ci), fill="light gray", alpha=0.5)+
    geom_line(aes(y=ccf, color=region, size=region)) +
    geom_hline(yintercept = 0, col="gray")+
    geom_vline(xintercept=0, col="gray") +
    ylim(-1,1)+
    #scale_colour_brewer(palette="Paired")+
    scale_color_manual(values=colours)+
    scale_size_manual(values=thickness)+
    facet_wrap(~type_mobility)+
    theme_bw()+
    labs(y="CCF", x="Lag (days)")+
    theme(legend.title=element_blank(),
          legend.position = "none",
          strip.background=element_rect(color="white", fill="white"),
          strip.text=element_text(color="black", face="bold"),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour="black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

ccf_plot
setwd("~/Desktop/Masters/Project/Analysis/Lags/Outputs/regional")
ggsave("ccf_plot_regional_mobility_types.png",ccf_plot)


## Faceted by region


library(scales)
show_col(hue_pal()(6))

mobility_colours <- c("#B79F00", "#F564E3", "#F8766D", "#00BFC4", "#619CFF")

ccf_plot_region <- ccf_summary %>% filter(!str_detect(type_mobility,"Parks"), region!="England") %>% 
  ggplot(aes(x=lag))+
  geom_area(data=confint,aes(x=lag,y=upper_ci), fill="light gray", alpha=0.5)+
  geom_area(data=confint,aes(x=lag,y=lower_ci), fill="light gray", alpha=0.5)+
  geom_line(aes(y=ccf, color=type_mobility)) +
  geom_hline(yintercept = 0, col="gray")+
  geom_vline(xintercept=0, col="gray") +
  ylim(-1,1)+
  #scale_colour_brewer(palette="Paired")+
  scale_color_manual(values=mobility_colours)+
  #scale_size_manual(values=thickness)+
  facet_wrap(~region)+
  theme_bw()+
  labs(y="CCF", x="Lag (days)")+
  theme(legend.title=element_blank(),
        legend.position = "none",
        strip.background=element_rect(color="white", fill="white"),
        strip.text=element_text(color="black", face="bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ccf_plot_region





# For England only - lines = mobility types

ccf_plot_england <- ccf_summary %>% filter(!str_detect(type_mobility,"Parks"),region=="England") %>%
  ggplot(aes(x=lag))+
  geom_area(data=confint,aes(x=lag,y=upper_ci), fill="light gray", alpha=0.5)+
  geom_area(data=confint,aes(x=lag,y=lower_ci), fill="light gray", alpha=0.5)+
  geom_line(aes(y=ccf, color=type_mobility)) +
  geom_hline(yintercept = 0, col="gray")+
  geom_vline(xintercept=0, col="gray") +
  ylim(-1,1)+
  scale_color_manual(values=mobility_colours)+
  facet_wrap(~region)+
  theme_bw()+
  labs(y="CCF", x="Lag (days)")+
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        strip.background=element_rect(color="white", fill="white"),
        strip.text=element_text(color="black", face="bold", size=10),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ccf_plot_england
setwd("~/Desktop/Masters/Project/Analysis/Lags/Outputs/regional")
ggsave("ccf_plot_england_mobility_types.png",ccf_plot_england)


library(cowplot)
plot_grid(ccf_plot_england,
             ccf_plot_region, nrow=2,
             rel_heights=c(1,3))


setwd("~/Desktop/Masters/Project/Analysis/Lags/Outputs/regional")
ggsave(file="multiplot_ccf_regional.png", plot_grid(ccf_plot_england,
                                               ccf_plot, nrow=2,
                                               rel_heights=c(1,3)), width=220, height=300, units="mm") 



# Putting R number at the top instead of England

r_plot_with_legend <- react_reprod_regional %>% filter(d_comb>=lockdown_3_start, d_comb<=lockdown_3_end) %>%
  ggplot()+
  geom_line(aes(x=d_comb,y=r, col=reg_char, size=reg_char))+
  scale_color_manual(values=colours)+
  scale_size_manual(values=thickness)+
  geom_hline(yintercept = 1, col="gray")+
  xlim(lockdown_3_start,lockdown_3_end)+
  scale_x_date(date_labels="%b %Y")+
  labs(x="",y="Reproduction number")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "right",
      panel.border=element_blank(),
      axis.line = element_line(colour = "black"),
      #axis.text.x=element_blank(),
      legend.title=element_blank(),
      plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

r_plot_with_legend

r_plot_no_legend <- r_plot_with_legend + theme(legend.position = "none")

r_plot_no_legend

# Alternative multiplot

plot_grid(r_plot_no_legend,
          ccf_plot, nrow=2,
          rel_heights=c(1,1.8), align="v",axis="lr")


# Manually add legend later
library(ggpubr) 
legend <- get_legend(r_plot_with_legend)
as_ggplot(legend)

setwd("~/Desktop/Masters/Project/Analysis/Lags/Outputs/regional")
ggsave(file="multiplot_ccf_regional_reproduction.png", plot_grid(r_plot_no_legend,
                                                                 ccf_plot, nrow=2,
                                                                 rel_heights=c(1,1.8), align="v",axis="lr"),
                                                                  width=220, height=250, units="mm") 
ggsave("regional_legend.png",as_ggplot(legend))



# By region but with R number

library(cowplot)
plot_grid(r_plot_with_legend,
          ccf_plot_region, 
          ccf_plot_england,
          nrow=3,
          rel_heights=c(2,3,1.5))


setwd("~/Desktop/Masters/Project/Analysis/Lags/Outputs/regional")
ggsave(file="multiplot_ccf_3parts.png", plot_grid(r_plot_with_legend,
                                                  ccf_plot_region, 
                                                  ccf_plot_england,
                                                  nrow=3,
                                                  rel_heights=c(2,3,1.5)),
       width=300, height=400, units="mm") 

# DTW ---------------------------------------------------------------------

# Lockdown 3 only

dtw_index_summary <-NA
dtw_lag_summary <-NA

for (i in 1:length(types_mobility)){
  for (j in 1:length(region_list)){
    start_date = lockdown_3_start
    end_date = lockdown_3_end
    mobility_place = types_mobility[i]
    region_interest = region_list[j]
    mob <- mobility_av %>% filter(region==region_interest, type_mobility==mobility_place, date>=lockdown_3_start, date<=lockdown_3_end)
    reprod <- react_reprod_regional %>% filter(reg_char == region_interest, d_comb>=lockdown_3_start, d_comb<=lockdown_3_end )
    dtw_lag <- dtw(normalize(reprod$r),normalize(mob$mobility),)
    dtw_data <- as.data.frame(cbind(dtw_lag$index1, dtw_lag$index2))
    colnames(dtw_data)<- c("mob_index","r_index")
    dtw_data$region <- region_interest
    dtw_data$type_mobility <- mobility_place
    dtw_index_summary <- rbind(dtw_index_summary,dtw_data)
    
    dtw_lag <- mean(dtw_data$mob_index-dtw_data$r_index)
    dtw_lag <- as.data.frame(dtw_lag)
    colnames(dtw_lag) <- "lag"
    dtw_lag$region <- region_interest
    dtw_lag$type_mobility <-mobility_place
    dtw_lag_summary <- rbind(dtw_lag_summary, dtw_lag)
    
  }
}

dtw_index_summary

dtw_index_summary <- dtw_index_summary[-1,]
dtw_lag_summary <- dtw_lag_summary[-1,]


ggplot(data=dtw_lag_summary)+
  geom_bar(aes(x=as.factor(type_mobility),y=lag, fill=type_mobility), stat="identity")+
  facet_wrap(~region)



# Map

eng <- rgdal::readOGR(paste0("https://opendata.arcgis.com/datasets/",
                             "8d3a9e6e7bd445e2bdcc26cdf007eac7_4.geojson"))

countries <- rgdal::readOGR(paste0("https://opendata.arcgis.com/datasets/",
                                   "92ebeaf3caa8458ea467ec164baeefa4_0.geojson"))

eng <- sf::st_as_sf(eng)
countries <- sf::st_as_sf(countries)
# UK <- countries[-1,] 
names(eng)[3] <- "Region"
# names(UK)[3] <- "Region"
# UK$objectid <- 10:12
eng <- eng[-2]
# UK <- UK[c(1, 3, 9:11)]
# UK <- rbind(eng, UK)

eng$rgn15nm

# Template plot to use for maps of regions of England

colour_breaks = seq(-40,40,by=5)
colour_limits = c(-40,45)

# Workplaces


dtw_lag_workplaces <- dtw_lag_summary %>% filter(type_mobility=="Workplaces", region!="England")
eng_workplaces <- merge(eng,dtw_lag_workplaces, by.x="Region",by.y="region")

map_dtw_workplaces <- ggplot(eng_workplaces, aes(fill = lag, label=lag)) + 
                      geom_sf() +
                      theme_bw() +
                      scale_fill_gradient2(low = "#2166AC", mid="#F7F7F7", high = "#B2182B", midpoint=0,
                                           breaks=colour_breaks, limits=colour_limits) +
                      guides(fill=guide_legend(title="Lag"))+
                      ggtitle("Workplaces")+
                      theme(
                        plot.title = element_text(hjust=0.5, size=14, face="bold"),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        axis.ticks.x = element_blank(),
                        axis.text.x = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.text.y = element_blank(),
                        legend.position = "none",
                        #legend.position = c(0.17,0.5),
                        legend.key = element_rect(fill = "transparent"),
                        plot.margin = unit(c(0, 0, 0, 0), "cm"),
                        panel.border = element_blank()
                      ) 


# Separate plot for the legend ONLY ---------------------------------------

map_dtw_workplaces_legend <- ggplot(eng_workplaces, aes(fill = lag, label=lag)) + 
  geom_sf() +
  theme_bw() +
  scale_fill_gradient2(low = "#2166AC", mid="#F7F7F7", high = "#B2182B", midpoint=0,
                       breaks=colour_breaks, limits=colour_limits) +
  guides(fill=guide_legend(title="Lag (days)"))+
  ggtitle("Workplaces")+
  theme(
    plot.title = element_text(hjust=0.5, size=14, face="bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    #legend.position = "none",
    legend.position = c(0.17,0.5),
    legend.key = element_rect(fill = "transparent"),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    panel.border = element_blank()
  ) 


library(ggpubr)
workplaces_legend <- get_legend(map_dtw_workplaces_legend)
as_ggplot(workplaces_legend)

# Residential

dtw_lag_retail <- dtw_lag_summary %>% filter(type_mobility=="Retail & recreation", region!="England")
eng_retail <- merge(eng,dtw_lag_retail, by.x="Region",by.y="region")

map_dtw_retail <- ggplot(eng_retail, aes(fill = lag, label=lag)) + 
  geom_sf() +
  theme_bw() +
  scale_fill_gradient2(low = "#2166AC", mid="#F7F7F7", high = "#B2182B", midpoint=0,
                       breaks=colour_breaks, limits=colour_limits) +
  guides(fill=guide_legend(title="Lag"))+
  ggtitle("Retail")+
  theme(
    plot.title = element_text(hjust=0.5, size=14, face="bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none",
    #legend.position = c(0.17,0.5),
    legend.key = element_rect(fill = "transparent"),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    panel.border = element_blank()
  ) 

map_dtw_retail

# Grocery

dtw_lag_grocery <- dtw_lag_summary %>% filter(type_mobility=="Grocery & pharmacy", region!="England")
eng_grocery <- merge(eng,dtw_lag_grocery, by.x="Region",by.y="region")

map_dtw_grocery <- ggplot(eng_grocery, aes(fill = lag, label=lag)) + 
  geom_sf() +
  theme_bw() +
  scale_fill_gradient2(low = "#2166AC", mid="#F7F7F7", high = "#B2182B", midpoint=0,
                       breaks=colour_breaks, limits=colour_limits) +
  guides(fill=guide_legend(title="Lag"))+
  ggtitle("Grocery and pharmacy")+
  theme(
    plot.title = element_text(hjust=0.5, size=14, face="bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none",
    #legend.position = c(0.17,0.5),
    legend.key = element_rect(fill = "transparent"),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    panel.border = element_blank()
  ) 

map_dtw_grocery

# Transit

dtw_lag_transit <- dtw_lag_summary %>% filter(type_mobility=="Transit stations" , region!="England")
eng_transit <- merge(eng,dtw_lag_transit, by.x="Region",by.y="region")

map_dtw_transit <- ggplot(eng_transit, aes(fill = lag, label=lag)) + 
  geom_sf() +
  theme_bw() +
  scale_fill_gradient2(low = "#2166AC", mid="#F7F7F7", high = "#B2182B", midpoint=0,
                       breaks=colour_breaks, limits=colour_limits) +
  guides(fill=guide_legend(title="Lag"))+
  ggtitle("Transit stations")+
  theme(
    plot.title = element_text(hjust=0.5, size=14, face="bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none",
    #legend.position = c(0.17,0.5),
    legend.key = element_rect(fill = "transparent"),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    panel.border = element_blank()
  ) 


map_dtw_transit

# Residential

dtw_lag_residential <- dtw_lag_summary %>% filter(type_mobility=="Residential" , region!="England")
eng_residential <- merge(eng,dtw_lag_residential, by.x="Region",by.y="region")

map_dtw_residential <- ggplot(eng_residential, aes(fill = lag, label=lag)) + 
  geom_sf() +
  theme_bw() +
  scale_fill_gradient2(low = "#2166AC", mid="#F7F7F7", high = "#B2182B", midpoint=0,
                       breaks=colour_breaks, limits=colour_limits) +
  guides(fill=guide_legend(title="Lag"))+
  ggtitle("Residential")+
  theme(
    plot.title = element_text(hjust=0.5, size=14, face="bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none",
    #legend.position = c(0.17,0.5),
    legend.key = element_rect(fill = "transparent"),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    panel.border = element_blank()
  ) 

map_dtw_residential


plot_grid(map_dtw_workplaces,
          map_dtw_grocery,
          map_dtw_transit,
          map_dtw_retail,
          map_dtw_residential,
          as_ggplot(workplaces_legend))

setwd("~/Desktop/Masters/Project/Analysis/Lags/Outputs/regional")
ggsave(file="multiplot_dtw_regional.png", plot_grid(map_dtw_workplaces,
                                                    map_dtw_grocery,
                                                    map_dtw_transit,
                                                    map_dtw_retail,
                                                    map_dtw_residential,
                                                    as_ggplot(workplaces_legend),
                                                    align="hv"), width=37, height=37, units="cm") 

