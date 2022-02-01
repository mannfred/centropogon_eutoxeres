library(raster)
library(sf) #simple features
library(spData)
library(tidyverse)
library(tmap)

data(World, land)

sa_region <- c(xmin = -85.957031, xmax = -23.725012,
               ymin = -55.722656, ymax = 6.509364)

sitedata <- data.frame(site ='San Pedro', lon = -71.548, lat = -13.055)
loc_geo <- st_as_sf(x=sitedata, coords=c('lon', 'lat'), crs = 4326)

# polygons and raster
tm_shape(land, bbox=sa_region) +
  tm_raster('elevation', palette = terrain.colors(4), n = 4, style='cont') +
  tm_legend(show=TRUE, position = c("left", "bottom"), legend.text.size=1.18, title.size=1.18) + 
  
tm_shape(World, bbox = sa_region) +
  tm_borders(lwd=3, col='white') +

tm_shape(loc_geo) +
  tm_dots(size=1, col='purple', shape=3, border.lwd=5) +
  tm_graticules(n.x=3, n.y=3, lines=FALSE, labels.size=1.2)


