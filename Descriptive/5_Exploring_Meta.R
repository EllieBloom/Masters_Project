


# With connections between every point
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
  geom_path(data=meta_a, aes(x=start_lon,y=start_lat, shape=factor(level)))+
  scale_size_area(max_size = 8) + 
  scale_color_viridis_c() + 
  theme() + 
  theme(title = element_text(size = 12))





# Data - bing tile level --------------------------------------------------

meta_tile <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Data/Meta/Britain_Coronavirus_Prevention_Map_Tile/704288455093254_2020-03-10_2020-03-23_csv/704288455093254_2020-03-10_0000.csv", header=TRUE)

colnames(meta_tile)

meta_tile_4 <- meta_tile %>% filter(level=="LEVEL4")

# Plot


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
  geom_point(data=meta_tile_4, aes(x=start_lon,y=start_lat, shape=factor(level)),color="navy blue", size=1)+
  scale_size_area(max_size = 8) + 
  scale_color_viridis_c() + 
  theme() + 
  theme(title = element_text(size = 12))

# Looks the same as with the administrative levels??
