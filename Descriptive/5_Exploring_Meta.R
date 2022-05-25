# Exploring the meta Cornovirus Disease Prevention Map for GB

# Setup

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)


# Reading one csv (8 hour period on one date)


meta_a <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Data/Meta/Britain_Coronavirus_Disease_Prevention_Map/2020-03-10_2020-03-23_csv/704288455093254_2020-03-10_0000.csv", header = TRUE)
meta_b <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Data/Meta/Britain_Coronavirus_Disease_Prevention_Map/2020-03-10_2020-03-23_csv/704288455093254_2020-03-10_0800.csv", header = TRUE)


head(meta)
colnames(meta)

# Exploring the first date time combination - meta_a

meta_a_4 <- meta_a %>% filter(level=="LEVEL4")

length(unique(meta_a_4$start_polygon_name))
length(unique(meta_a_4$end_polygon_name))

meta_a_5 <- meta_a %>% filter(level=="LEVEL5")

length(unique(meta_a_5$start_polygon_name))
length(unique(meta_a_5$end_polygon_name))

nrow(meta_a)

# Exploring the second date time combination - meta_b

nrow(meta_b)

meta_b_4 <- meta_b %>% filter(level=="LEVEL4")

length(unique(meta_b_4$start_polygon_name))
length(unique(meta_b_4$end_polygon_name))

meta_b_5 <- meta_b %>% filter(level=="LEVEL5")

length(unique(meta_b_5$start_polygon_name))
length(unique(meta_b_5$end_polygon_name))



# Trying to plot on a map -------------------------------------------------

library(sf)
library(tidyverse)
library(maps)


## First attempt -----------------------------------------------------------
my_df <- meta_a

my_sf <- st_as_sf(my_df, coords = c('start_lon', 'start_lat'))
                  
my_sf <- st_set_crs(my_sf, crs = 4326)
                  
#Plot it:
ggplot(my_sf) + geom_sf()



# More complex attempt ---------------------------------------------------------
# https://bookdown.org/yann_ryan/r-for-newspaper-data/mapping-with-r-geocode-and-map-the-british-librarys-newspaper-collection.html

worldmap = map_data('world')

# Map of GB
ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, 
                   group = group), 
               fill = 'gray90', 
               color = 'black') + 
  coord_fixed(ratio = 1.3, 
              xlim = c(-10,3), 
              ylim = c(50, 59)) + 
  theme_void() +
  geom_point(data=meta_a, aes(x=start_lon,y=start_lat, shape=factor(level)),color="navy blue", size=1)+
  scale_size_area(max_size = 8) + 
  scale_color_viridis_c() + 
  theme() + 
  theme(title = element_text(size = 12))

ggplot(data=meta_a, aes(x=start_lon,y=start_lat))+
  geom_point()

