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

ggplot(data = boxplot_data_long , aes(x=as.factor(variable),y=value*7))+
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



# Barplot for beta values -------------------------------------------------


## Lockdown 1 --------------------------------------------------------------

regression_lockdown1 <- regression_lockdown1[order(-regression_lockdown1$days_since_lockdown),]
regression_lockdown1$X <-as.factor(regression_lockdown1$X)


ggplot(data=regression_lockdown1, aes(x=X,y=days_since_lockdown*7))+
  geom_bar(stat="identity", color="#F8766D", fill="#F8766D", width=0.25) +
  scale_x_discrete(limits=regression_lockdown1$X) +
  xlab("")+
  scale_y_continuous(expand=c(0,0)) +
  ylab("Percentage point change in mobility per week") +
  ggtitle("Lockdown 1") +
  coord_flip() +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

## Lockdown 2 --------------------------------------------------------------

regression_lockdown2 <- regression_lockdown2[order(-regression_lockdown2$days_since_lockdown),]
regression_lockdown2$X <-as.factor(regression_lockdown2$X)


ggplot(data=regression_lockdown2, aes(x=X,y=days_since_lockdown*7))+
  geom_bar(stat="identity", color="#00BA38", fill="#00BA38", width=0.25) +
  scale_x_discrete(limits=regression_lockdown2$X) +
  xlab("")+
  scale_y_continuous(expand=c(0,0)) +
  ylab("Percentage point change in mobility per week") +
  ggtitle("Lockdown 2") +
  coord_flip() +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

## Lockdown 3 --------------------------------------------------------------

regression_lockdown3 <- regression_lockdown3[order(-regression_lockdown3$days_since_lockdown),]
regression_lockdown3$X <-as.factor(regression_lockdown3$X)


ggplot(data=regression_lockdown3, aes(x=X,y=days_since_lockdown*7))+
  geom_bar(stat="identity", color="#619CFF", fill="#619CFF", width=0.25) +
  scale_x_discrete(limits=regression_lockdown3$X) +
  xlab("")+
  scale_y_continuous(expand=c(0,0)) +
  ylab("Percentage point change in mobility per week") +
  ggtitle("Lockdown 3") +
  coord_flip() +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))



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


ggplot(data = drop_boxplot_data_long , aes(x=as.factor(variable),y=value))+
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



# Barplot for drop values -------------------------------------------------


## Lockdown 1 --------------------------------------------------------------

drop_lockdown1 <-drop_lockdown1[order(-drop_lockdown1$Drop),]
drop_lockdown1$sub_region_1 <-as.factor(drop_lockdown1$sub_region_1)


ggplot(data=drop_lockdown1, aes(x=sub_region_1,y=Drop))+
  geom_bar(stat="identity", color="#F8766D", fill="#F8766D", width=0.25) +
  scale_x_discrete(limits=drop_lockdown1$sub_region_1) +
  xlab("")+
  scale_y_continuous(expand=c(0,0)) +
  ylab("Drop in mobility (percentage points)") +
  ggtitle("Lockdown 1") +
  coord_flip() +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))


## Lockdown 2 --------------------------------------------------------------

drop_lockdown2 <-drop_lockdown2[order(-drop_lockdown2$Drop),]
drop_lockdown2$sub_region_1 <-as.factor(drop_lockdown2$sub_region_1)


ggplot(data=drop_lockdown2, aes(x=sub_region_1,y=Drop))+
  geom_bar(stat="identity", color="#00BA38", fill="#00BA38", width=0.25) +
  scale_x_discrete(limits=drop_lockdown2$sub_region_1) +
  xlab("")+
  scale_y_continuous(expand=c(0,0)) +
  ylab("Drop in mobility (percentage points)") +
  ggtitle("Lockdown 2") +
  coord_flip() +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

## Lockdown 3 --------------------------------------------------------------

drop_lockdown3 <-drop_lockdown3[order(-drop_lockdown3$Drop),]
drop_lockdown3$sub_region_1 <-as.factor(drop_lockdown3$sub_region_1)


ggplot(data=drop_lockdown3, aes(x=sub_region_1,y=Drop))+
  geom_bar(stat="identity", color="#619CFF", fill="#619CFF", width=0.25) +
  scale_x_discrete(limits=drop_lockdown3$sub_region_1) +
  xlab("")+
  scale_y_continuous(expand=c(0,0)) +
  ylab("Drop in mobility (percentage points)") +
  ggtitle("Lockdown 3") +
  coord_flip() +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))
