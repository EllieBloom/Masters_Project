### Visualising the mobility NPI results

# Results from script 1_Mobility_NPIs.R

# Date started: 25th May 2022

# Issues:
# Baseline measures are so different
# Reduction in movement precedes lockdown often
# For lockdown 2 - end of lockdown there are still tier3/4 restrictions

# Setup

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(reshape)



# Boxplots for beta values ------------------------------------------------

regression_lockdown1 <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility/Outputs/Regional_workplace_lockdown1_regression_results.csv")
regression_lockdown2 <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility/Outputs/Regional_workplace_lockdown2_regression_results.csv")
regression_lockdown3 <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility/Outputs/Regional_workplace_lockdown3_regression_results.csv")


boxplot_data <- cbind("beta_lockdown1"=regression_lockdown1$days_since_lockdown,
                      "beta_lockdown2"=regression_lockdown2$days_since_lockdown,
                      "beta_lockdown3"=regression_lockdown3$days_since_lockdown)

boxplot_data <- as.data.frame(boxplot_data)

boxplot_data_long <- as.data.frame(melt(boxplot_data[,c("beta_lockdown1", 
                                                    "beta_lockdown2",  
                                                    "beta_lockdown3")]))


# Plot

boxplot_beta <- ggplot(data = boxplot_data_long , aes(x=as.factor(variable),y=value*7))+
                              geom_boxplot(aes(color=variable), outlier.shape=4) +
                              xlab("") +
                              ylab("Percentage point change in mobility per week") +
                              scale_x_discrete(labels=c("beta_lockdown1" = "Lockdown 1",
                                                        "beta_lockdown2" = "Lockdown 2",
                                                        "beta_lockdown3" = "Lockdown 3")) +
                              theme_light() +
                              theme(axis.text.x=element_text(face="bold", size=12),
                                    axis.text.y=element_text(size=10),
                                    legend.position = "none")

boxplot_beta

setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility/Outputs/Plots")
ggsave("boxplot_beta_lockdowns.pdf", plot=boxplot_beta, device="pdf")



# Barplot for beta values -------------------------------------------------


## Lockdown 1 --------------------------------------------------------------

regression_lockdown1 <- regression_lockdown1[order(-regression_lockdown1$days_since_lockdown),]
regression_lockdown1$X <-as.factor(regression_lockdown1$X)


barplot_beta_lockdown1 <- ggplot(data=regression_lockdown1, aes(x=X,y=days_since_lockdown*7))+
                          geom_bar(stat="identity", color="#F8766D", fill="#F8766D", width=0.25) +
                          scale_x_discrete(limits=regression_lockdown1$X) +
                          xlab("")+
                          scale_y_continuous(expand=c(0,0)) +
                          ylab("Percentage point change in mobility per week") +
                          ggtitle("Lockdown 1") +
                          coord_flip() +
                          theme_light() +
                          theme(plot.title = element_text(hjust = 0.5))

barplot_beta_lockdown1
ggsave("barplot_beta_lockdown1.pdf",barplot_beta_lockdown1)

## Lockdown 2 --------------------------------------------------------------

regression_lockdown2 <- regression_lockdown2[order(-regression_lockdown2$days_since_lockdown),]
regression_lockdown2$X <-as.factor(regression_lockdown2$X)


barplot_beta_lockdown2 <- ggplot(data=regression_lockdown2, aes(x=X,y=days_since_lockdown*7))+
                          geom_bar(stat="identity", color="#00BA38", fill="#00BA38", width=0.25) +
                          scale_x_discrete(limits=regression_lockdown2$X) +
                          xlab("")+
                          scale_y_continuous(expand=c(0,0)) +
                          ylab("Percentage point change in mobility per week") +
                          ggtitle("Lockdown 2") +
                          coord_flip() +
                          theme_light() +
                          theme(plot.title = element_text(hjust = 0.5))

barplot_beta_lockdown2
ggsave("barplot_beta_lockdown2.pdf",barplot_beta_lockdown2)

## Lockdown 3 --------------------------------------------------------------

regression_lockdown3 <- regression_lockdown3[order(-regression_lockdown3$days_since_lockdown),]
regression_lockdown3$X <-as.factor(regression_lockdown3$X)


barplot_beta_lockdown3 <- ggplot(data=regression_lockdown3, aes(x=X,y=days_since_lockdown*7))+
                          geom_bar(stat="identity", color="#619CFF", fill="#619CFF", width=0.25) +
                          scale_x_discrete(limits=regression_lockdown3$X) +
                          xlab("")+
                          scale_y_continuous(expand=c(0,0)) +
                          ylab("Percentage point change in mobility per week") +
                          ggtitle("Lockdown 3") +
                          coord_flip() +
                          theme_light() +
                          theme(plot.title = element_text(hjust = 0.5))

barplot_beta_lockdown3
ggsave("barplot_beta_lockdown3.pdf",barplot_beta_lockdown3)


# Boxplots for drop values -----------------------------------------------

drop_lockdown1<-read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility/Outputs/Regional_lockdown1_drop.csv")
drop_lockdown2<-read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility/Outputs/Regional_lockdown2_drop.csv")
drop_lockdown3<-read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility/Outputs/Regional_lockdown3_drop.csv")


drop_boxplot_data <- cbind("drop_lockdown1"=drop_lockdown1$Drop,
                          "drop_lockdown2"=drop_lockdown2$Drop,
                          "drop_lockdown3"=drop_lockdown3$Drop)

drop_boxplot_data <- as.data.frame(drop_boxplot_data)

drop_boxplot_data_long <- as.data.frame(melt(drop_boxplot_data[,c("drop_lockdown1", 
                                                            "drop_lockdown2",  
                                                            "drop_lockdown3")]))


boxplot_drop_lockdowns <- ggplot(data = drop_boxplot_data_long , aes(x=as.factor(variable),y=value))+
                          geom_boxplot(aes(color=variable), outlier.shape=4) +
                          xlab("") +
                          ylab("Drop in mobility (percentage points)") +
                          scale_x_discrete(labels=c("drop_lockdown1" = "Lockdown 1",
                                                    "drop_lockdown2" = "Lockdown 2",
                                                    "drop_lockdown3" = "Lockdown 3")) +
                          theme_light() +
                          theme(axis.text.x=element_text(face="bold", size=12),
                                axis.text.y=element_text(size=10),
                                legend.position = "none")

boxplot_drop_lockdowns
ggsave("boxplot_drop_lockdowns.pdf",boxplot_drop_lockdowns)

# Barplot for drop values -------------------------------------------------


## Lockdown 1 --------------------------------------------------------------

drop_lockdown1 <-drop_lockdown1[order(-drop_lockdown1$Drop),]
drop_lockdown1$sub_region_1 <-as.factor(drop_lockdown1$sub_region_1)


barplot_drop_lockdown1 <- ggplot(data=drop_lockdown1, aes(x=sub_region_1,y=Drop))+
                          geom_bar(stat="identity", color="#F8766D", fill="#F8766D", width=0.25) +
                          scale_x_discrete(limits=drop_lockdown1$sub_region_1) +
                          xlab("")+
                          scale_y_continuous(expand=c(0,0)) +
                          ylab("Drop in mobility (percentage points)") +
                          ggtitle("Lockdown 1") +
                          coord_flip() +
                          theme_light() +
                          theme(plot.title = element_text(hjust = 0.5))


barplot_drop_lockdown1
ggsave("barplot_drop_lockdown1.pdf",barplot_drop_lockdown1)


## Lockdown 2 --------------------------------------------------------------

drop_lockdown2 <-drop_lockdown2[order(-drop_lockdown2$Drop),]
drop_lockdown2$sub_region_1 <-as.factor(drop_lockdown2$sub_region_1)


barplot_drop_lockdown2 <- ggplot(data=drop_lockdown2, aes(x=sub_region_1,y=Drop))+
                          geom_bar(stat="identity", color="#00BA38", fill="#00BA38", width=0.25) +
                          scale_x_discrete(limits=drop_lockdown2$sub_region_1) +
                          xlab("")+
                          scale_y_continuous(expand=c(0,0)) +
                          ylab("Drop in mobility (percentage points)") +
                          ggtitle("Lockdown 2") +
                          coord_flip() +
                          theme_light() +
                          theme(plot.title = element_text(hjust = 0.5))

barplot_drop_lockdown2
ggsave("barplot_drop_lockdown2.pdf",barplot_drop_lockdown2)

## Lockdown 3 --------------------------------------------------------------

drop_lockdown3 <-drop_lockdown3[order(-drop_lockdown3$Drop),]
drop_lockdown3$sub_region_1 <-as.factor(drop_lockdown3$sub_region_1)


barplot_drop_lockdown3 <- ggplot(data=drop_lockdown3, aes(x=sub_region_1,y=Drop))+
                          geom_bar(stat="identity", color="#619CFF", fill="#619CFF", width=0.25) +
                          scale_x_discrete(limits=drop_lockdown3$sub_region_1) +
                          xlab("")+
                          scale_y_continuous(expand=c(0,0)) +
                          ylab("Drop in mobility (percentage points)") +
                          ggtitle("Lockdown 3") +
                          coord_flip() +
                          theme_light() +
                          theme(plot.title = element_text(hjust = 0.5))


barplot_drop_lockdown3
ggsave("barplot_drop_lockdown3.pdf",barplot_drop_lockdown3)




# Regional mobility boxplots ----------------------------------------------


england_sub_region_1 <- c("Bath and North East Somerset","Bedford", "Blackburn with Darwen",
                          "Blackpool", "Borough of Halton","Bracknell Forest","Brighton and Hove", 
                          "Bristol City","Buckinghamshire","Cambridgeshire","Central Bedfordshire",
                          "Cheshire East","Cheshire West and Chester","Cornwall","County Durham",
                          "Cumbria", "Darlington","Derby","Derbyshire","Devon", "Dorset",
                          "East Riding of Yorkshire", "East Sussex", "Essex","Gloucestershire",
                          "Greater London","Greater Manchester","Hampshire" , "Hartlepool",
                          "Herefordshire", "Isle of Wight", "Kent", "Kingston upon Hull",
                          "Lancashire", "Leicester","Leicestershire", "Lincolnshire","Luton",
                          "Medway", "Merseyside", "Middlesbrough","Milton Keynes", "Norfolk",
                          "North East Lincolnshire", "North Lincolnshire", "North Somerset", 
                          "North Yorkshire", "Northamptonshire", "Northumberland", "Nottingham",
                          "Nottinghamshire", "Oxfordshire", "Peterborough", "Plymouth","Portsmouth",
                          "Reading", "Redcar and Cleveland", "Rutland", "Shropshire", "Slough",
                          "Somerset", "South Gloucestershire", "South Yorkshire", "Southampton",
                          "Southend-on-Sea", "Staffordshire", "Stockton-on-Tees", "Stoke-on-Trent",
                          "Suffolk", "Surrey", "Swindon", "Thurrock", "Torbay","Tyne and Wear",
                          "Warrington", "Warwickshire", "West Berkshire", "West Midlands",
                          "West Sussex", "West Yorkshire", "Wiltshire", "Windsor and Maidenhead",
                          "Wokingham", "Worcestershire","York")

google_overall <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Data/Google/google_overall.csv")

# Converting dates from string format to date

str(google_overall$date) # Dates are currently stored as characters
google_overall$date <- as.Date(google_overall$date,format="%Y-%m-%d")
str(google_overall$date)


google_england <- google_overall %>% filter(sub_region_1 %in% england_sub_region_1) %>% filter(sub_region_2 == "")






# Mobility drop vs rebound --------------------------------------------------


## Lockdown 1 ----------------------------------------------------------------


regional_drop_summary <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility/Outputs/Regional_lockdown1_drop.csv")

regional_drop_summary
regional_drop_summary$Drop <-as.numeric(regional_drop_summary$Drop)
regional_drop_summary$Rebound_30days<- as.numeric(regional_drop_summary$Rebound_30days)

# Adding rebound after 4 weeks
library(ggrepel)

median_drop <- median(regional_drop_summary$Drop )
median_rebound <- median(regional_drop_summary$Rebound_30days)

# Linear model, ready for labelling on the graph
lm_eqn <- function(df){
  m <- lm(Rebound_30days ~ Drop, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

# Summary of lm
summary(lm(Rebound_30days ~ Drop, regional_drop_summary))
# Note p<0.0001

# Graph
plot_drop_rebound_lockdown1<- ggplot(data=regional_drop_summary, aes(x=Drop,y=Rebound_30days)) +
  geom_point(shape=20)+
  geom_text_repel(aes(label=sub_region_1),size=2.5) +
  geom_hline(yintercept=median_rebound, linetype="dashed", color="red")+
  geom_vline(xintercept=median_drop, linetype="dashed", color="red")+
  geom_smooth(method='lm',se=FALSE, color="royalblue")+
  xlab("Drop from average 2 weeks before lockdown to minimum (percentage points)")+
  ylab("Rebound in mobility after 30 days (percentage points)") +
  ggtitle("Lockdown 1") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(x = 60, y = 18, label = lm_eqn(regional_drop_summary), parse = TRUE, color="royalblue", hjust=0)

plot_drop_rebound_lockdown1

setwd("Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility/Outputs/Plots")
ggsave("Regional_lockdown1_drop_rebound_plot.pdf",plot_drop_rebound_lockdown1)





## Lockdown 2 ----------------------------------------------------------------

# Analysis doesn't work for lockdown 2 as it is so short

# regional_drop_summary_2 <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility/Outputs/Regional_lockdown2_drop.csv")
# 
# regional_drop_summary_2
# regional_drop_summary_2$Drop <-as.numeric(regional_drop_summary_2$Drop)
# regional_drop_summary_2$Rebound_30days<- as.numeric(regional_drop_summary_2$Rebound_30days)
# 
# # Adding rebound after 4 weeks
# library(ggrepel)
# 
# median_drop_2 <- median(regional_drop_summary_2$Drop )
# median_rebound_2 <- median(regional_drop_summary_2$Rebound_30days)
# 
# # Linear model, ready for labelling on the graph
# 
# # Summary of lm
# summary(lm(Rebound_30days ~ Drop, regional_drop_summary_2))
# # Note p<0.0001
# 
# # Graph
# plot_drop_rebound_lockdown2<- ggplot(data=regional_drop_summary_2, aes(x=Drop,y=Rebound_30days)) +
#                               geom_point(shape=20)+
#                               geom_text_repel(aes(label=sub_region_1),size=2.5) +
#                               geom_hline(yintercept=median_rebound_2, linetype="dashed", color="red")+
#                               geom_vline(xintercept=median_drop_2, linetype="dashed", color="red")+
#                               geom_smooth(method='lm',se=FALSE, color="royalblue")+
#                               xlab("Drop from average 2 weeks before lockdown to minimum (percentage points)")+
#                               ylab("Rebound in mobility after 30 days (percentage points)") +
#                               ggtitle("Lockdown 2") +
#                               theme_light() +
#                               theme(plot.title = element_text(hjust = 0.5))+
#                               geom_text(x = 68.5, y = 28, label = lm_eqn(regional_drop_summary_2), parse = TRUE, color="royalblue")
# 
# plot_drop_rebound_lockdown2
# 
# setwd("Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility/Outputs/Plots")
# ggsave("Regional_lockdown2_drop_rebound_plot.pdf",plot_drop_rebound_lockdown2)





## Lockdown 3 ----------------------------------------------------------------


regional_drop_summary_3 <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility/Outputs/Regional_lockdown3_drop.csv")

regional_drop_summary_3
regional_drop_summary_3$Drop <-as.numeric(regional_drop_summary_3$Drop)
regional_drop_summary_3$Rebound_30days<- as.numeric(regional_drop_summary_3$Rebound_30days)

# Adding rebound after 4 weeks
library(ggrepel)

median_drop_3 <- median(regional_drop_summary_3$Drop )
median_rebound_3 <- median(regional_drop_summary_3$Rebound_30days)

# Linear model, ready for labelling on the graph

# Summary of lm
summary(lm(Rebound_30days ~ Drop, regional_drop_summary_3))
# Note p<0.0001

# Graph
plot_drop_rebound_lockdown3<- ggplot(data=regional_drop_summary_3, aes(x=Drop,y=Rebound_30days)) +
  geom_point(shape=20)+
  geom_text_repel(aes(label=sub_region_1),size=2.5) +
  geom_hline(yintercept=median_rebound_3, linetype="dashed", color="red")+
  geom_vline(xintercept=median_drop_3, linetype="dashed", color="red")+
  geom_smooth(method='lm',se=FALSE, color="royalblue")+
  xlab("Drop from average 2 weeks before lockdown to minimum (percentage points)")+
  ylab("Rebound in mobility after 30 days (percentage points)") +
  ggtitle("Lockdown 3") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(x = 28, y = 15, label = lm_eqn(regional_drop_summary_3), parse = TRUE, color="royalblue")

plot_drop_rebound_lockdown3

setwd("Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility/Outputs/Plots")
ggsave("Regional_lockdown3_drop_rebound_plot.pdf",plot_drop_rebound_lockdown3)


