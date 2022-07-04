# Correlation between mobility measures

library(tidyverse)

# Date started: 23rd May 2022

google_overall <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Data/Google/google_overall.csv")


# Filterting for GB dataset
google_gb <- google_overall %>% filter(sub_region_1=="") # Where there is no sub-region, the data is for GB overall

google_england <- readRDS("/Users/elliebloom/Desktop/Masters/Project/Analysis/Mapping/Outputs/google_england_wide.rds")

# Looking at correlations between metrics

#  Correlations between metrics at England level (using moving averages)---------


google_england_movingav <- google_england %>% filter(region=="ENGLAND")
# Select moving averages only

google_england_movingav <- google_england_movingav[,(10:15)]



# Correlation matrix
# Rename the columns to make visualisation easier


colnames(google_england_movingav)<-c("Retail","Grocery and Pharmacy","Parks",
                                      "Transit Stations", "Workplaces", "Residential")

# Remove the NAs due to moving av (1st 3 rows and last 3 rows)

head(google_england_movingav)
google_england_movingav <- google_england_movingav[-c(1:3),]
tail(google_england_movingav)
n = nrow(google_england_movingav)
google_england_movingav <- google_england_movingav[-c((n-2):n),]
tail(google_england_movingav)

# Correlation matrix
cormat <- round(cor(google_england_movingav[,c("Retail","Grocery and Pharmacy","Parks",
                                 "Transit Stations", "Workplaces", "Residential")]),2)
# Make long

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
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
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
        plot.title = element_text(hjust = 0.5))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)

# Add correlation coefficients

ggheatmap_final<- ggheatmap + 
                  geom_text(aes(X2, X1, label = value), color = "black", size = 4) +
                  ggtitle("Correlation heatmap of Google Mobility at England level") +
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



ggheatmap_final
setwd("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility/Outputs")
ggsave("GB_mobility_heatmap.pdf",ggheatmap_final)

# Now make the heatmap process into a function that can be applied to any region and any set of dates

