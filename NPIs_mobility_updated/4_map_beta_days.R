# Mapping the beta coefficients...



# Setup -------------------------------------------------------------------

library(tidyverse)




# Data --------------------------------------------------------------------

# Produced in NPIs_updates/2
workplace_lockdown1_nine_regions <- read_csv("workplace_lockdown1_nine_regions.csv")
workplace_lockdown2_nine_regions <- read_csv("workplace_lockdown2_nine_regions.csv")
workplace_lockdown3_nine_regions <- read_csv("workplace_lockdown3_nine_regions.csv")


colnames(workplace_lockdown1_nine_regions)[1]<-"coefficient" 
colnames(workplace_lockdown2_nine_regions)[1]<-"coefficient" 
colnames(workplace_lockdown3_nine_regions)[1]<-"coefficient" 

# Data set of beta days coefficients
beta_days_lockdown1<-workplace_lockdown1_nine_regions %>% filter(coefficient=="days_since_lockdown")
beta_days_lockdown2<-workplace_lockdown2_nine_regions %>% filter(coefficient=="days_since_lockdown")
beta_days_lockdown3<-workplace_lockdown3_nine_regions %>% filter(coefficient=="days_since_lockdown")


beta_days_lockdown1 <- beta_days_lockdown1 %>%
  mutate(region = 
           case_when(region=="ENGLAND"~"England",
                     region=="EAST"~"East" ,
                     region=="EAST MIDLANDS"~"East Midlands",
                     region=="LONDON"~"London" ,
                     region=="NORTH EAST"~"North East",
                     region=="NORTH WEST"~"North West",
                     region=="SOUTH EAST"~"South East" ,
                     region=="SOUTH WEST"~"South West" ,
                     region=="WEST MIDLANDS"~"West Midlands",
                     region=="YORKSHIRE AND THE HUMBER"~"Yorkshire and The Humber" ))

beta_days_lockdown2 <- beta_days_lockdown2 %>%
  mutate(region = 
           case_when(region=="ENGLAND"~"England",
                     region=="EAST"~"East" ,
                     region=="EAST MIDLANDS"~"East Midlands",
                     region=="LONDON"~"London" ,
                     region=="NORTH EAST"~"North East",
                     region=="NORTH WEST"~"North West",
                     region=="SOUTH EAST"~"South East" ,
                     region=="SOUTH WEST"~"South West" ,
                     region=="WEST MIDLANDS"~"West Midlands",
                     region=="YORKSHIRE AND THE HUMBER"~"Yorkshire and The Humber" ))



beta_days_lockdown3 <- beta_days_lockdown3 %>%
  mutate(region = 
           case_when(region=="ENGLAND"~"England",
                     region=="EAST"~"East" ,
                     region=="EAST MIDLANDS"~"East Midlands",
                     region=="LONDON"~"London" ,
                     region=="NORTH EAST"~"North East",
                     region=="NORTH WEST"~"North West",
                     region=="SOUTH EAST"~"South East" ,
                     region=="SOUTH WEST"~"South West" ,
                     region=="WEST MIDLANDS"~"West Midlands",
                     region=="YORKSHIRE AND THE HUMBER"~"Yorkshire and The Humber" ))













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

# colour_breaks = seq(-40,40,by=5)
# colour_limits = c(-40,45)

# Merging the shape file 

beta_days_lockdown1 <- merge(eng,beta_days_lockdown1, by.x="Region",by.y="region")
beta_days_lockdown2 <- merge(eng,beta_days_lockdown2, by.x="Region",by.y="region")
beta_days_lockdown3 <- merge(eng,beta_days_lockdown3, by.x="Region",by.y="region")





# Map ---------------------------------------------------------------------

# Looking at max and min to have consistent lines
summary(beta_days_lockdown1$Estimate) # min 0.2987, max 0.3299
summary(beta_days_lockdown2$Estimate) # min 0.1253, max 0.1924
summary(beta_days_lockdown3$Estimate) # 0.1300, max 0.1543



beta_days_lockdown1$coefficient

library(viridis)


map_beta_days_lockdown1 <- ggplot(beta_days_lockdown1, aes(fill=Estimate, label=Region))+
  geom_sf()+
  theme_bw()+
  guides(fill=guide_legend(title=bquote(beta[days]~(percentage~points/day))))+
  theme(
    plot.title = element_text(hjust=0.5, size=14, face="bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.key = element_rect(fill = "transparent"),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    panel.border = element_blank()
  ) 

map_beta_days_lockdown1

map_beta_days_lockdown2 <- ggplot(beta_days_lockdown2, aes(fill=Estimate, label=Region))+
  geom_sf()+
  theme_bw()+
  guides(fill=guide_legend(title=bquote(beta[days]~(percentage~points/day))))+
  theme(
    plot.title = element_text(hjust=0.5, size=14, face="bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.key = element_rect(fill = "transparent"),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    panel.border = element_blank()
  ) 

map_beta_days_lockdown2 

map_beta_days_lockdown3 <- ggplot(beta_days_lockdown3, aes(fill=Estimate, label=Region))+
  geom_sf()+
  theme_bw()+
  guides(fill=guide_legend(title=bquote(beta[days]~(percentage~points/day))))+
  theme(
    plot.title = element_text(hjust=0.5, size=14, face="bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.key = element_rect(fill = "transparent"),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    panel.border = element_blank()
  ) 

map_beta_days_lockdown3

# Try facetting
beta_days_lockdown1$lockdown <- "Lockdown 1"
beta_days_lockdown2$lockdown <- "Lockdown 2"
beta_days_lockdown3$lockdown <- "Lockdown 3"

beta_days <- rbind(beta_days_lockdown1,beta_days_lockdown2,beta_days_lockdown3)

colour_limits <- c(0.1,0.35)
breaks <- seq(0.1,0.35,by=0.05)

map_beta_days <- ggplot(beta_days, aes(fill=Estimate, label=Region))+
  geom_sf(colour="black")+
  facet_wrap(~lockdown)+
  scale_fill_gradient2(low = "#ffffcc", mid="#41b6c4", high = "#0c2c84", midpoint=sum(colour_limits)/2,
                       limits=colour_limits, breaks=breaks) +
  theme_bw()+
  guides(fill=guide_legend(title=bquote(beta[days])))+
  theme(
    plot.title = element_text(hjust=0.5, size=14, face="bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.key = element_rect(fill = "transparent"),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    panel.border = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) 

map_beta_days_without_legend <- map_beta_days + theme(legend.position = "none")

map_legend <-


# Boxplot ----------------------------------------------------------------


boxplot_beta <- ggplot(data = beta_days , aes(x=as.factor(lockdown),y=Estimate))+
  geom_boxplot(outlier.shape=4,lwd=0.5) +
  xlab("") +
  scale_fill_gradient2(low = "#ffffd9", mid="#7fcdbb", high = "#081d58", midpoint=sum(colour_limits)/2,
                       limits=colour_limits, breaks=breaks) +
  scale_y_continuous( limits=c(0.11,0.35)) +
  ylab(bquote(beta[days]~(percentage~points/day))) +
  # scale_x_discrete(labels=c("beta_lockdown1" = "Lockdown 1",
  #                           "beta_lockdown2" = "Lockdown 2",
  #                           "beta_lockdown3" = "Lockdown 3"),
  #                  limits = c("beta_lockdown3", "beta_lockdown2", "beta_lockdown1")) +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "none")

boxplot_beta


# Adding the England point over the top

England_total_Lockdown_1 <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/England_total_Lockdown_1.csv")
England_total_Lockdown_2 <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/England_total_Lockdown_2.csv")
England_total_Lockdown_3 <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/England_total_Lockdown_3.csv")



england_total <- matrix(nrow=3,ncol=2)

england_total[1,1] <-"Lockdown 1"
england_total[2,1] <-"Lockdown 2"
england_total[3,1] <-"Lockdown 3"
england_total[1,2] <- England_total_Lockdown_1$WorkplacesEstimate[9]
england_total[2,2] <- England_total_Lockdown_2$WorkplacesEstimate[8]
england_total[3,2] <- England_total_Lockdown_3$WorkplacesEstimate[9]

england_total <- as.data.frame(england_total)
colnames(england_total) <-c("variable","value")
england_total$value <- as.numeric(england_total$value)

boxplot_beta_final <- boxplot_beta + geom_point(data=england_total, aes(x=as.factor(variable),y=value), shape=19,size=2.5)+
  geom_point(data=england_total, aes(x=as.factor(variable),y=value, color=variable), shape=19,size=1.5)+
  scale_color_manual(values=c("Lockdown 1"="#225ea8","Lockdown 2"="#c7e9b4","Lockdown 3"="#c7e9b4"))+
  ggtitle("")+
  theme(plot.title = element_text(hjust = 0, size=22),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        axis.text.x=element_text(size=14, face="bold"),
        axis.text.y=element_text(size=12, face="bold"),
        plot.margin= margin(0, 0, 0, 0, "cm"))

boxplot_beta_final





# Grid --------------------------------------------------------------------



setwd("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/Plots")
plot_grid(boxplot_beta_final,
          map_beta_days_without_legend,
          nrow=2)

setwd("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/Plots")
ggsave(filename = "map_boxplot_betas_regional.png")


