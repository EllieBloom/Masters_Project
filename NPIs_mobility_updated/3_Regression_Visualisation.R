# Visualisation of Linear regression results

# Results generateed form 2_Linear_Regression.R

# Date started: 4th July 2022


# Focusing on workplace mobility for plots


# Setup

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(reshape)
library(extrafont) 

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


# Plot

boxplot_beta_workplace <- ggplot(data = workplace_boxplot_data_regional_long , aes(x=as.factor(variable),y=value))+
                            geom_boxplot(aes(color=variable), outlier.shape=4,lwd=1) +
                            ggtitle("A")+
                            xlab("") +
                            scale_y_continuous( limits=c(-0.12,0.39)) +
                            ylab("\u03b2 (percentage point / day)") +
                            scale_x_discrete(labels=c("beta_lockdown1" = "Lockdown 1",
                                                      "beta_lockdown2" = "Lockdown 2",
                                                      "beta_lockdown3" = "Lockdown 3"),
                                             limits = c("beta_lockdown3", "beta_lockdown2", "beta_lockdown1")) +
                            theme_bw() +
                            theme(panel.border = element_blank(),
                                  panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(), 
                                  axis.line = element_line(colour = "black"),
                                  legend.position = "none")

boxplot_beta_workplace 

# Add the england point over the top

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
  
boxplot_beta_workplace_final <- boxplot_beta_workplace + geom_point(data=england_total, aes(x=as.factor(variable),y=value), shape=19,size=3)+
                                geom_point(data=england_total, aes(x=as.factor(variable),y=value, color=variable), shape=19,size=2)+
                                coord_flip() +
                                ggtitle("A")+
                                theme(plot.title = element_text(hjust = 0, size=22),
                                      panel.border = element_blank(),
                                      panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), 
                                      axis.line = element_line(colour = "black"),
                                      legend.position = "none",
                                      axis.text.y=element_text(size=12, face="bold"))

boxplot_beta_workplace_final

setwd("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/Plots")
ggsave("boxplot_england_beta_workplace.png",boxplot_beta_workplace_final, device="png")




# Barplot of betas --------------------------------------------------------

min(workplace_regional_lockdown1$days_since_lockdown)
max(workplace_regional_lockdown1$days_since_lockdown) # overall maximum

min(workplace_regional_lockdown2$days_since_lockdown) # overall minimum
max(workplace_regional_lockdown2$days_since_lockdown)

min(workplace_regional_lockdown3$days_since_lockdown)
max(workplace_regional_lockdown3$days_since_lockdown)

barplot_min <-  min(workplace_regional_lockdown2$days_since_lockdown)
barplot_max <- max(workplace_regional_lockdown1$days_since_lockdown) 

## Lockdown 1 --------------------------------------------------------------

workplace_regional_lockdown1 <- workplace_regional_lockdown1[order(-workplace_regional_lockdown1$days_since_lockdown),]
workplace_regional_lockdown1$X <-as.factor(workplace_regional_lockdown1$X)


barplot_beta_lockdown1 <- ggplot(data=workplace_regional_lockdown1, aes(x=X,y=days_since_lockdown))+
  geom_bar(stat="identity", color="#F8766D", fill="#F8766D", width=0.25) +
  scale_x_discrete(limits=workplace_regional_lockdown1$X) +
  xlab("")+
  scale_y_continuous( limits=c(-0.05,0.36)) +
  ylab("\u03b2 (percentage points/day)") +
  ggtitle("B") +
  coord_flip() +
  theme_light() +
  theme(plot.title = element_text(hjust = 0, size=22),
        text=element_text(),panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "none")

barplot_beta_lockdown1
setwd("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/Plots")
ggsave("barplot_england_beta_workplace_lockdown1.png",barplot_beta_lockdown1)



## Lockdown 2 --------------------------------------------------------------

workplace_regional_lockdown2 <- workplace_regional_lockdown2[order(-workplace_regional_lockdown2$days_since_lockdown),]
workplace_regional_lockdown2$X <-as.factor(workplace_regional_lockdown2$X)

barplot_beta_lockdown2 <- ggplot(data=workplace_regional_lockdown2, aes(x=X,y=days_since_lockdown))+
geom_bar(stat="identity", color="#00BA38", fill="#00BA38", width=0.25) +
  scale_x_discrete(limits=workplace_regional_lockdown2$X) +
  xlab("")+
  scale_y_continuous( limits=c(-0.05,0.36)) +
  ylab("\u03b2 (percentage points/day)") +
  coord_flip() +
  ggtitle("C")+
  theme_light() +
  theme(plot.title = element_text(hjust = 0, size=22),
        text=element_text(),panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "none")

barplot_beta_lockdown2
setwd("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/Plots")
ggsave("barplot_england_beta_workplace_lockdown2.png",barplot_beta_lockdown2)

## Lockdown 3 --------------------------------------------------------------

workplace_regional_lockdown3 <- workplace_regional_lockdown3[order(-workplace_regional_lockdown3$days_since_lockdown),]
workplace_regional_lockdown3$X <-as.factor(workplace_regional_lockdown3$X)


barplot_beta_lockdown3 <- ggplot(data=workplace_regional_lockdown3, aes(x=X,y=days_since_lockdown))+
  geom_bar(stat="identity", color="#619CFF", fill="#619CFF", width=0.25) +
  scale_x_discrete(limits=workplace_regional_lockdown3$X) +
  xlab("")+
  scale_y_continuous( limits=c(-0.12,0.36)) +
  ylab("\u03b2 (percentage points/day)") +
  ggtitle("") +
  coord_flip() +
  theme_light() +
  ggtitle("D")+
  theme(plot.title = element_text(hjust = 0, size=22),
        text=element_text(),panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "none")

barplot_beta_lockdown3
setwd("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/Plots")
ggsave("barplot_england_beta_workplace_lockdown3.png",barplot_beta_lockdown3)


# Change the x axis scale to be the same on all of them

summary(workplace_regional_lockdown1$days_since_lockdown) #min is 0.2248, max is 0.3534 -> use this max
summary(workplace_regional_lockdown2$days_since_lockdown) #min is -0.04082, max is 0.31429 -> use this min
summary(workplace_regional_lockdown3$days_since_lockdown) #min is -0.08791, max is 0.23582


# Multiplot

library(cowplot)
plot_grid(boxplot_beta_workplace_final,
          barplot_beta_lockdown1,
          barplot_beta_lockdown2,
          barplot_beta_lockdown3, nrow=2, align="v")
# Align= "v' allows all of the axis to line up despit the fact that the y axis labels force them not to normally using grid.arrange


# plot <- grid.arrange(boxplot_beta_workplace_final,
#              barplot_beta_lockdown1,
#              barplot_beta_lockdown2,
#              barplot_beta_lockdown3, nrow=2)

# Saving (need to use arrangeGrob here)
lockdown_multi <- arrangeGrob(boxplot_beta_workplace_final,
                 barplot_beta_lockdown1,
                 barplot_beta_lockdown2,
                 barplot_beta_lockdown3, nrow=2)
setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/Plots")
ggsave(file="boxplot_barplot_regression_multi.png", plot_grid(boxplot_beta_workplace_final,
                                                              barplot_beta_lockdown1,
                                                              barplot_beta_lockdown2,
                                                              barplot_beta_lockdown3, nrow=2, align="v"), width=425, height=475, units="mm") #saves g



# New barplot -------------------------------------------------------------


# Lockdown 1

regional_lockdown1 <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/Workplace_only/workplace_lockdown1_neat.csv")

regional_lockdown1 <- regional_lockdown1 %>% filter(region!="Rutland")


regional_lockdown1 <- regional_lockdown1[order(-regional_lockdown1$Estimate),]
regional_lockdown1$region <-as.factor(regional_lockdown1$region)

barplot_new_1 <- ggplot(data=regional_lockdown1, aes(x=region,y=Estimate))+
                  geom_bar(stat="identity", color="#F8766D", fill="#F8766D", width=0.25) +
                  geom_errorbar(aes(ymin=X2.50., ymax=X97.50.),col="lightslategray")+
                  scale_x_discrete(limits=regional_lockdown1$region) +
                  xlab("")+
                  scale_y_continuous( limits=c(-0.12,0.39)) +
                  ylab("\u03b2 (percentage points/day)") +
                  ggtitle("B") +
                  coord_flip() +
                  theme_light() +
                  theme(plot.title = element_text(hjust = 0, size=22),
                        text=element_text(),panel.border = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), 
                        axis.line = element_line(colour = "black"),
                        legend.position = "none")

barplot_new_1

# Lockdown 2

regional_lockdown2 <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/Workplace_only/workplace_lockdown2_neat.csv")
regional_lockdown2 <- regional_lockdown2 %>% filter(region!="Rutland")


regional_lockdown2 <- regional_lockdown2[order(-regional_lockdown2$Estimate),]
regional_lockdown2$region <-as.factor(regional_lockdown2$region)

barplot_new_2 <- ggplot(data=regional_lockdown2, aes(x=region,y=Estimate))+
                geom_bar(stat="identity", color="#00BA38", fill="#00BA38", width=0.25) +
                geom_errorbar(aes(ymin=X2.50., ymax=X0.975),col="lightslategray")+
                scale_x_discrete(limits=regional_lockdown2$region) +
                xlab("")+
                scale_y_continuous( limits=c(-0.12,0.39)) +
                ylab("\u03b2 (percentage points/day)") +
                ggtitle("C") +
                coord_flip() +
                theme_light() +
                theme(plot.title = element_text(hjust = 0, size=22),
                      text=element_text(),panel.border = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), 
                      axis.line = element_line(colour = "black"),
                      legend.position = "none")

barplot_new_2

# Lockdown 3

regional_lockdown3 <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/Workplace_only/workplace_lockdown3_neat.csv")
regional_lockdown3 <- regional_lockdown3 %>% filter(region!="Rutland")

regional_lockdown3 <- regional_lockdown3[order(-regional_lockdown3$Estimate),]
regional_lockdown3$region <-as.factor(regional_lockdown3$region)

barplot_new_3 <- ggplot(data=regional_lockdown3, aes(x=region,y=Estimate))+
                  geom_bar(stat="identity", color="#619CFF", fill="#619CFF", width=0.25) +
                  geom_errorbar(aes(ymin=X2.50., ymax=X97.50.),col="lightslategray")+
                  scale_x_discrete(limits=regional_lockdown3$region) +
                  xlab("")+
                  scale_y_continuous( limits=c(-0.12,0.39)) +
                  ylab("\u03b2 (percentage points/day)") +
                  ggtitle("D") +
                  coord_flip() +
                  theme_light() +
                  theme(plot.title = element_text(hjust = 0, size=22),
                        text=element_text(),panel.border = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), 
                        axis.line = element_line(colour = "black"),
                        legend.position = "none")


barplot_new_3

# Losing the ordering on 3rd plot and don't know why -> this is the only hting that needs chaning -> then save very tall

plot_grid(boxplot_beta_workplace_final,
          barplot_new_1,
          barplot_new_2,
          barplot_new_3, nrow=2, align="hv")

setwd("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/Workplace_only")
ggsave("bar_box_multiplot.png",plot_grid(boxplot_beta_workplace_final,
                                   barplot_new_1,
                                   barplot_new_2,
                                   barplot_new_3, nrow=2, align="hv"),width=415, height=510, units="mm")

