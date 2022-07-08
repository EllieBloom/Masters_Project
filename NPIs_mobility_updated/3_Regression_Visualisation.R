# Visualisation of Linear regression results

# Results generateed form 2_Linear_Regression.R

# Date started: 4th July 2022


# Focusing on workplace mobility for plots


# Setup

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(reshape)


# Boxplots for beta values ------------------------------------------------

workplace_regional_lockdown1 <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/England_regional_Lockdown_1_Workplaces.csv")
workplace_regional_lockdown2 <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/England_regional_Lockdown_2_Workplaces.csv")
workplace_regional_lockdown3 <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/England_regional_Lockdown_3_Workplaces.csv")


workplace_boxplot_data_regional <- cbind("beta_lockdown1"=workplace_regional_lockdown1$days_since_lockdown,
                                         "beta_lockdown2"=workplace_regional_lockdown2$days_since_lockdown,
                                         "beta_lockdown3"=workplace_regional_lockdown3$days_since_lockdown)

workplace_boxplot_data_regional <- as.data.frame(workplace_boxplot_data_regional)

workplace_boxplot_data_regional_long <- as.data.frame(melt(workplace_boxplot_data_regional[,c("beta_lockdown1", 
                                                        "beta_lockdown2",  
                                                        "beta_lockdown3")]))


# Plot

boxplot_beta_workplace <- ggplot(data = workplace_boxplot_data_regional_long , aes(x=as.factor(variable),y=value*7))+
                            geom_boxplot(aes(color=variable), outlier.shape=4) +
                            ggtitle("")+
                            xlab("") +
                            ylab("Percentage point change in mobility per week") +
                            scale_x_discrete(labels=c("beta_lockdown1" = "Lockdown 1",
                                                      "beta_lockdown2" = "Lockdown 2",
                                                      "beta_lockdown3" = "Lockdown 3")) +
                            theme_light() +
                            theme(legend.position = "none",
                                  axis.text.x=element_text(size=12,face="bold"),
                                  axis.text.y=element_text(size=10),
                                  axis.title.y=element_text(size=12))

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
england_total[1,2] <- England_total_Lockdown_1$Lockdown_1_Workplaces_England[9]
england_total[2,2] <- England_total_Lockdown_2$Lockdown_2_Workplaces_England[8]
england_total[3,2] <- England_total_Lockdown_3$Lockdown_3_Workplaces_England[9]

england_total <- as.data.frame(england_total)
colnames(england_total) <-c("variable","value")
england_total$value <- as.numeric(england_total$value)
  
boxplot_beta_workplace_final <- boxplot_beta_workplace + geom_point(data=england_total, aes(x=as.factor(variable),y=value*7), shape=19,size=3)+
                                geom_point(data=england_total, aes(x=as.factor(variable),y=value*7, color=variable), shape=19,size=2)


boxplot_beta_workplace_final

setwd("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/Plots")
ggsave("boxplot_england_beta_workplace.png",boxplot_beta_workplace_final)




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


barplot_beta_lockdown1 <- ggplot(data=workplace_regional_lockdown1, aes(x=X,y=days_since_lockdown*7))+
  geom_bar(stat="identity", color="#F8766D", fill="#F8766D", width=0.25) +
  scale_x_discrete(limits=workplace_regional_lockdown1$X) +
  xlab("")+
  scale_y_continuous(expand=c(0,0)) +
  ylab("Percentage point change in mobility per week") +
  ggtitle("") +
  coord_flip() +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

barplot_beta_lockdown1
setwd("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/Plots")
ggsave("barplot_england_beta_workplace_lockdown1.png",barplot_beta_lockdown1)



## Lockdown 2 --------------------------------------------------------------

workplace_regional_lockdown2 <- workplace_regional_lockdown2[order(-workplace_regional_lockdown2$days_since_lockdown),]
workplace_regional_lockdown2$X <-as.factor(workplace_regional_lockdown2$X)


barplot_beta_lockdown2 <- ggplot(data=workplace_regional_lockdown2, aes(x=X,y=days_since_lockdown*7))+
  geom_bar(stat="identity", color="#00BA38", fill="#00BA38", width=0.25) +
  scale_x_discrete(limits=workplace_regional_lockdown2$X) +
  xlab("")+
  scale_y_continuous(expand=c(0,0)) +
  ylab("Percentage point change in mobility per week") +
  ggtitle("") +
  coord_flip() +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

barplot_beta_lockdown2
setwd("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/Plots")
ggsave("barplot_england_beta_workplace_lockdown2.png",barplot_beta_lockdown2)

## Lockdown 3 --------------------------------------------------------------

workplace_regional_lockdown3 <- workplace_regional_lockdown3[order(-workplace_regional_lockdown3$days_since_lockdown),]
workplace_regional_lockdown3$X <-as.factor(workplace_regional_lockdown3$X)


barplot_beta_lockdown3 <- ggplot(data=workplace_regional_lockdown3, aes(x=X,y=days_since_lockdown*7))+
  geom_bar(stat="identity", color="#619CFF", fill="#619CFF", width=0.25) +
  scale_x_discrete(limits=workplace_regional_lockdown3$X) +
  xlab("")+
  scale_y_continuous(expand=c(0,0)) +
  ylab("Percentage point change in mobility per week") +
  ggtitle("") +
  coord_flip() +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

barplot_beta_lockdown3
setwd("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Regression/Plots")
ggsave("barplot_england_beta_workplace_lockdown3.png",barplot_beta_lockdown3)

