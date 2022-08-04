# Visualisations of shifts and lags

# Library Load-In====
library(tidyverse) #For everything data#

#Let's Start with a Trig Refresher#========================#
theta <- seq(from = -pi,
             to = 3*pi, 
             length.out = 150)

sine <- tibble(x = theta-1,
               y = sin(theta/2),
               label = 1:length(theta))

sine_shift <- tibble(x = theta-3,
               y = sin(theta/2),
               label = 1:length(theta))


max_mob_x = sine_shift$x[which.max(sine_shift$y)]
max_prev_x = sine$x[which.max(sine$y)] 
colors <- c("Mobility illustration"="blue","Prevalence demonstration")


p1 <- sine %>%
  ggplot(aes(x=x,y=y))+
  geom_line(aes(color= "dark green"))+
  geom_line(data=sine_shift,aes(x=x,y=y,color="blue"))+
  xlim(0,8)+
  labs(x="Time", y="")+
  scale_color_identity(name="",
                       breaks=c("dark green","blue"),
                       labels=c("Prevalence","Mobiliy"),
                       guide="legend")+
  theme_bw()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position=c(0.9,0.5))+
  geom_segment(aes(x=max_mob_x, xend=max_prev_x,y=1, yend=1), 
               arrow=arrow(length=unit(0.4,"cm")))+
  annotate("text",x=(max_mob_x+max_prev_x)/2,y=1.05,label="Mobility precedes prevalence by 2 weeks") # Note annotate is not blurry but using geom_text is blurry
p1

# For some reason the legend disappears when annotations added

p2 <- sine %>%
  ggplot(aes(x=x,y=y))+
  geom_line(color= "dark green")+
  geom_line(data=sine_shift,aes(x=x,y=y),color="blue")+
  xlim(0,max(sine$x))+
  labs(x="Time", y="")+
  scale_color_identity(name="",
                       breaks=c("dark green","blue"),
                       labels=c("Prevalence","Mobiliy"),
                       guide="legend")+
  theme_bw()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position=c(0.9,0.5))+
  annotate("rect",xmin=0,xmax=3,ymin=-1,ymax=1.01, alpha=0.1, colour="black")+
  annotate("text",x=1.5,y=1.05,label="Current view of data using granger causality")+
  annotate("text",x=(max_mob_x+max_prev_x)/2,y=0.45,label="Shift mobility curve forwards") +
  geom_segment(aes(x=max_mob_x, xend=max_prev_x,y=0.5, yend=0.5), 
               arrow=arrow(length=unit(0.4,"cm")))
p2

p3 <- sine %>%
  ggplot(aes(x=x,y=y))+
  geom_line(color= "dark green")+
  geom_line(data=sine_shift,aes(x=x+1.3,y=y),color="blue")+
  xlim(0,max(sine$x))+
  labs(x="Date (weeks)", y="")+
  scale_color_identity(name="",
                       breaks=c("dark green","blue"),
                       labels=c("Prevalence","Mobiliy"),
                       guide="legend")+
  theme_bw()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="top")+
  annotate("rect",xmin=0,xmax=3,ymin=-1,ymax=1.01, alpha=0.1, colour="black")+
  annotate("text",x=1.5,y=1.05,label="New view of data using granger causality")

p3

library(gridExtra)
grid.arrange(p1, p2, p3,nrow = 3)
