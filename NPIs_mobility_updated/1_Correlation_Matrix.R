# Creating mobility correlation heatmaps using standard function

# Date started: 3rd July 2022





# Setup -------------------------------------------------------------------

library(tidyverse)
library(gtsummary)
library(zoo)
library(reshape2)



# Loading data ------------------------------------------------------------

# Data at Gb and county level
google_overall <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Data/Google/google_overall.csv")

# Data at England and regional level
google_england <- readRDS("/Users/elliebloom/Desktop/Masters/Project/Analysis/Mapping/Outputs/google_england_wide.rds")



# Inputting useful dates --------------------------------------------------


start_date_default <- min(google_england$date)
end_date_default <-  max(google_england$date)

# Useful dates
lockdown_1_start <- as.Date("2020-03-26","%Y-%m-%d")
lockdown_1_end <- as.Date("2020-06-15","%Y-%m-%d")

lockdown_2_start <- as.Date("2020-11-05","%Y-%m-%d")
lockdown_2_end <- as.Date("2020-12-02","%Y-%m-%d")

lockdown_3_start <- as.Date("2021-01-06","%Y-%m-%d")
lockdown_3_end <- as.Date("2021-04-21","%Y-%m-%d")




# Inputting regions in England --------------------------------------------



region_list <- dput(unique(google_england$region))
region_list_neat <- c("England", "East of England", "East Midlands", "London", "North East", 
                      "North West", "South East", "South West", "West Midlands", "Yorkshire and the Humber")



# Functions ---------------------------------------------------------------


## Function to create correlation matrix

get_cormat <- function(data, region_interest,start_date=start_date_default, end_date=end_date_default){
  data_filter <- data %>% filter(region==region_interest,date>=start_date, date<=end_date)
  print("Correlation  matrix is for:")
  print(unique(data_filter$region))
  data_movingav <- data_filter[,(10:15)]
  colnames(data_movingav) <- c("Retail","Grocery and Pharmacy","Parks",
                               "Transit Stations", "Workplaces", "Residential")
  data_movingav <- data_movingav[-c(1:3),]
  n <- nrow(data_movingav)
  data_movingav <- data_movingav[-c((n-2):n),]  
  cormat <- round(cor(data_movingav),2)
  return(cormat)
}

## Function to return heatmap
convert_cormat_heatmap <- function(cormat, region_interest_neat,time_period_name){
  library(reshape2)
  melted_cormat <- melt(cormat)
  head(melted_cormat)
  
  # Preparing visualisation
  
  # Get lower triangle of the correlation matrix
  get_lower_tri<-function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  # Get upper triangle of the correlation matrix
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }
  
  upper_tri <- get_upper_tri(cormat)
  upper_tri
  
  
  # Melt the correlation matrix
  
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  
  # Heatmap
  
  ggplot(data = melted_cormat, aes(X2, X1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1),
          axis.text.y = element_text(size=12))+
    coord_fixed()
  
  # Reorder the correlation matrix
  reorder_cormat <- function(cormat){
    # Use correlation between variables as distance
    dd <- as.dist((1-cormat)/2)
    hc <- hclust(dd)
    cormat <-cormat[hc$order, hc$order]
  }
  
  # Reorder the correlation matrix
  # cormat <- reorder_cormat(cormat) # Commented this out
  # upper_tri <- get_upper_tri(cormat) # Commented this out
  # Melt the correlation matrix
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  # Create a ggheatmap
  ggheatmap <- ggplot(melted_cormat, aes(X2, X1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation",
                         na.value="white") +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1),
          axis.text.y = element_text(size=12),
          plot.title = element_text(hjust = 0))+
    coord_fixed()
  
  
  # Add correlation coefficients
  
  ggheatmap_final<- ggheatmap + 
    geom_text(aes(X2, X1, label = value), color = "black", size = 8) +
    ggtitle(region_interest_neat) +
    labs(subtitle=time_period_name)+
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.4, 0.7),
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))+
    scale_y_discrete(position = "right")
  
  
  
  return(ggheatmap_final)
  print(ggheatmap_final)
  
}





# Whole period ------------------------------------------------------------

# Loop for England and each region in England

cor_summary_whole_period <- NA

for (i in 1:length(region_list)){
  region=region_list[i]
  region_neat=region_list_neat[i]
  period="Whole period"
  cormat <- get_cormat(data=google_england, region_interest=region)
  cor_summary_whole_period <- rbind(cor_summary_whole_period,paste(region_neat,period),cormat)
  heatmap <- convert_cormat_heatmap(cormat,region_neat,period)
  plot_name <- paste0(region,"_",period,"_corr_heatmap.png")
  setwd("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Corr_heatmaps")
  ggsave(plot_name,heatmap,device="png")
}

cor_summary_whole_period <- cor_summary_whole_period[-1,]
cor_summary_whole_period

setwd("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Corr_matrices")
write.csv(cor_summary_whole_period,"Corr_England_whole_period.csv")

# Lockdown 1 ------------------------------------------------------------

# Loop for England and each region in England


cor_summary_lockdown_1 <- NA

for (i in 1:length(region_list)){
  region=region_list[i]
  region_neat=region_list_neat[i]
  period="Lockdown 1"
  cormat <- get_cormat(data=google_england, region_interest=region,start_date=lockdown_1_start, end_date=lockdown_1_end)
  cor_summary_lockdown_1 <- rbind(cor_summary_lockdown_1,paste(region_neat,period),cormat)
  heatmap <- convert_cormat_heatmap(cormat,region_neat,period)
  plot_name <- paste0(region,"_",period,"_corr_heatmap.png")
  setwd("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Corr_heatmaps")
  ggsave(plot_name,heatmap,device="png")
}

cor_summary_lockdown_1 <- cor_summary_lockdown_1[-1,]
cor_summary_lockdown_1

setwd("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Corr_matrices")
write.csv(cor_summary_lockdown_1,"Corr_England_lockdown_1.csv")

# Lockdown 2 ------------------------------------------------------------

# Loop for England and each region in England

cor_summary_lockdown_2 <- NA

for (i in 1:length(region_list)){
  region=region_list[i]
  region_neat=region_list_neat[i]
  period="Lockdown 2"
  cormat <- get_cormat(data=google_england, region_interest=region,start_date=lockdown_2_start, end_date=lockdown_2_end)
  cor_summary_lockdown_2 <- rbind(cor_summary_lockdown_2,paste(region_neat,period),cormat)
  heatmap <- convert_cormat_heatmap(cormat,region_neat,period)
  plot_name <- paste0(region,"_",period,"_corr_heatmap.png")
  setwd("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Corr_heatmaps")
  ggsave(plot_name,heatmap,device="png")
}

cor_summary_lockdown_2 <- cor_summary_lockdown_2[-1,]
cor_summary_lockdown_2

setwd("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Corr_matrices")
write.csv(cor_summary_lockdown_2,"Corr_England_lockdown_2.csv")

# Lockdown 3 ------------------------------------------------------------

# Loop for England and each region in England

cor_summary_lockdown_3 <- NA

for (i in 1:length(region_list)){
  region=region_list[i]
  region_neat=region_list_neat[i]
  period="Lockdown 3"
  cormat <- get_cormat(data=google_england, region_interest=region,start_date=lockdown_3_start, end_date=lockdown_3_end)
  cor_summary_lockdown_3 <- rbind(cor_summary_lockdown_3,paste(region_neat,period),cormat)
  heatmap <- convert_cormat_heatmap(cormat,region_neat,period)
  plot_name <- paste0(region,"_",period,"_corr_heatmap.png")
  setwd("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Corr_heatmaps")
  ggsave(plot_name,heatmap,device="png")
}

cor_summary_lockdown_3 <- cor_summary_lockdown_3[-1,]
cor_summary_lockdown_3

setwd("~/Desktop/Masters/Project/Analysis/NPIs_mobility_updated/Outputs/Corr_matrices")
write.csv(cor_summary_lockdown_3,"Corr_England_lockdown_3.csv")