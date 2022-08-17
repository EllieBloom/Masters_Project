# Maps of counties with boxplot and map

# Date started: 17 August

# Setup -------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)


# Playing around with maps
# https://stackoverflow.com/questions/63768949/plotting-uk-regions-using-gadm-data-in-r


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

library(RColorBrewer)

map_plot <- ggplot(eng, aes(fill = Region)) + 
  geom_sf() +
  scale_fill_brewer(palette="Paired") +
  theme_bw() +
  guides(fill=guide_legend(title=""))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = c(0.05,0.65),
    legend.key = element_rect(fill = "transparent"),
    panel.border = element_blank(),
    legend.text=element_text(size=15)
  ) 

map_plot

setwd("~/Desktop/Masters/Project/Analysis/Descriptive/Ouputs")
ggsave("map_plot.png",map_plot)


# Without legend

ggplot(eng, aes(fill = Region)) + 
  geom_sf() +
  scale_fill_brewer(palette="Paired") +
  theme_bw() +
  guides(fill=guide_legend(title="Region"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none",
    legend.key = element_rect(fill = "transparent")
  ) 


map_template <- ggplot(eng) + 
  geom_sf(fill="white") +
  scale_fill_brewer(palette="Paired") +
  theme_bw() +
  guides(fill=guide_legend(title="Region"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none",
    legend.key = element_rect(fill = "transparent")
  ) 

map_template

# Trying to add counties as points ----------------------------------------

county_locations <- read_csv("~/Desktop/Masters/Project/Analysis/Mapping/county_locations.csv")

map_counties <- map_template +
  geom_point(data=county_locations, aes(x=Latitude, y=Longitude), size=5, shape="diamond")

map_counties

merge(county_locations, beta, by="region"
      
# Adding betas in  --------------------------------------------------------
      
# Data
beta_lockdown1_workplaces <- read_csv("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/England_regional_Lockdown_1_Workplaces.csv")
colnames(beta_lockdown1_workplaces)[1]<-"County"
lockdown1 <- merge(county_locations, beta_lockdown1_workplaces, by="County")

lockdown1 <- lockdown1[,c("County","Longitude","Latitude","days_since_lockdown")]
lockdown1$lockdown <- "Lockdown 1"
      
beta_lockdown2_workplaces <- read_csv("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/England_regional_Lockdown_2_Workplaces.csv")
colnames(beta_lockdown2_workplaces)[1]<-"County"
lockdown2 <- merge(county_locations, beta_lockdown2_workplaces, by="County")
      
lockdown2 <- lockdown2[,c("County","Longitude","Latitude","days_since_lockdown")]
lockdown2$lockdown <- "Lockdown 2"
      
beta_lockdown3_workplaces <- read_csv("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/England_regional_Lockdown_3_Workplaces.csv")
colnames(beta_lockdown3_workplaces)[1]<-"County"
lockdown3 <- merge(county_locations, beta_lockdown3_workplaces, by="County")
      
lockdown3 <- lockdown3[,c("County","Longitude","Latitude","days_since_lockdown")]
lockdown3$lockdown <- "Lockdown 3"
      
lockdowns_combined <- rbind(lockdown1,lockdown2)
lockdowns_combined <- rbind(lockdowns_combined,lockdown3)
      
      
# Exploring min and maxes for scaling

min(lockdowns_combined$days_since_lockdown) # -0.04081633
max(lockdowns_combined$days_since_lockdown) # 0.3534263
      
      
 mean(lockdowns_combined$days_since_lockdown)
      

## Map plot ----------------------------------------------------------------

      
      
colour_limits <- c(-0.05,0.36)
breaks <- seq(-0.05,0.36,by=0.05)
      
map_counties <- map_template +
        geom_point(data=lockdowns_combined, aes(x=Latitude, y=Longitude, color=days_since_lockdown), size=3, shape="diamond") +
        facet_wrap(.~lockdown) +
        guides(color=guide_legend(title=bquote(beta[days]), reverse=TRUE))+
        scale_color_viridis(breaks=breaks)+
        # scale_colour_gradient2(low = "#ffffd9", mid="#41b6c4", high = "#081d58", midpoint=0.17,
        #                        limits=colour_limits, breaks=breaks) +
        theme(
          legend.position = "left",
          plot.title = element_text(hjust=0.5, size=14, face="bold"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.key = element_rect(fill = "transparent"),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          panel.border = element_blank(),
          strip.background = element_blank(),
          strip.text.x = element_blank()) 
      
      
map_counties

map_counties_without_legend <- map_counties + theme(legend.position="none")
map_counties_without_legend




# Boxplot -----------------------------------------------------------------


# Boxplots for beta values ------------------------------------------------

workplace_regional_lockdown1 <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/England_regional_Lockdown_1_Workplaces.csv")
workplace_regional_lockdown2 <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/England_regional_Lockdown_2_Workplaces.csv")
workplace_regional_lockdown3 <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/England_regional_Lockdown_3_Workplaces.csv")



# Excluding Rutland as too much missingness

workplace_regional_lockdown1 <- workplace_regional_lockdown1 %>% filter(X!="Rutland")
workplace_regional_lockdown2 <- workplace_regional_lockdown2 %>% filter(X!="Rutland")
workplace_regional_lockdown3 <- workplace_regional_lockdown3 %>% filter(X!="Rutland")


# Now combining with Rutland removed

workplace_boxplot_data_regional <- cbind("beta_lockdown1"=workplace_regional_lockdown1$days_since_lockdown,
                                         "beta_lockdown2"=workplace_regional_lockdown2$days_since_lockdown,
                                         "beta_lockdown3"=workplace_regional_lockdown3$days_since_lockdown)

workplace_boxplot_data_regional <- as.data.frame(workplace_boxplot_data_regional)



workplace_boxplot_data_regional_long <- as.data.frame(melt(workplace_boxplot_data_regional[,c("beta_lockdown1", 
                                                                                              "beta_lockdown2",  
                                                                                              "beta_lockdown3")]))

workplace_boxplot_data_regional_long


# Barplot

boxplot_beta_workplace <- ggplot(data = workplace_boxplot_data_regional_long , aes(x=as.factor(variable),y=value))+
  geom_boxplot(outlier.shape=4,lwd=0.5) +
  ggtitle("")+
  xlab("") +
  scale_y_continuous( limits=c(-0.12,0.42)) +
  ylab(bquote(beta[days]~(percentage~points/day))) +
  scale_x_discrete(labels=c("beta_lockdown1" = "Lockdown 1",
                            "beta_lockdown2" = "Lockdown 2",
                            "beta_lockdown3" = "Lockdown 3"),
                   limits = c("beta_lockdown1", "beta_lockdown2", "beta_lockdown3")) +
  theme_bw() +
  theme(plot.title = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        axis.text.x=element_text(size=14, face="bold"),
        axis.text.y=element_text(size=12, face="bold"),
        plot.margin= margin(0, 0, 0, 0, "cm"))

boxplot_beta_workplace 


England_total_Lockdown_1 <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/England_total_Lockdown_1.csv")
England_total_Lockdown_2 <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/England_total_Lockdown_2.csv")
England_total_Lockdown_3 <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/England_total_Lockdown_3.csv")



workplace_boxplot_data_regional_long


england_total <- matrix(nrow=3,ncol=2)

england_total[1,1] <-"beta_lockdown1"
england_total[2,1] <-"beta_lockdown2"
england_total[3,1] <-"beta_lockdown3"
england_total[1,2] <- England_total_Lockdown_1$WorkplacesEstimate[9]
england_total[2,2] <- England_total_Lockdown_2$WorkplacesEstimate[8]
england_total[3,2] <- England_total_Lockdown_3$WorkplacesEstimate[9]

england_total <- as.data.frame(england_total)
colnames(england_total) <-c("variable","value")
england_total$value <- as.numeric(england_total$value)

boxplot_beta_workplace_final <- boxplot_beta_workplace + 
  geom_point(data=england_total, aes(x=as.factor(variable),y=value), shape=19,size=2)+
  geom_point(data=england_total, aes(x=as.factor(variable),y=value), shape=19,size=2)+
  ggtitle("")+
  theme(plot.title = element_text(hjust = 0, size=22),
        
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        axis.text.y=element_text(size=12, face="bold"))

boxplot_beta_workplace_final

# Change to virdis

# Grid multiplot ----------------------------------------------------------

plot_grid(boxplot_beta_workplace_final,
          map_counties_without_legend,
          nrow=2)
