# Maps of regions of England

# Date started: 20th  June 2022

# Setup -------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)


# Playing around with maps
# https://stackoverflow.com/questions/63768949/plotting-uk-regions-using-gadm-data-in-r


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

library(RColorBrewer)

ggplot(eng, aes(fill = Region)) + 
  geom_sf() +
  scale_fill_brewer(palette="Paired") +
  theme_bw() +
  guides(fill=guide_legend(title="Region"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = c(0.17,0.8),
    legend.key = element_rect(fill = "transparent")
  ) 


# Without legend

ggplot(eng, aes(fill = Region)) + 
  geom_sf() +
  scale_fill_brewer(palette="Paired") +
  theme_bw() +
  guides(fill=guide_legend(title="Region"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none",
    legend.key = element_rect(fill = "transparent")
  ) 
