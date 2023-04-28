###***********************###
### Plotting spatial data ###
###***********************###
# Project aim:
#
# Author: Alys 
#
# Collaborators:
#
# Date:
#
# Script aim:



## data
setwd("/Users/alys/Documents/2. RA Hotspots/HOTspots")

## hotspot data
#hotspot_yearly_data  <- read.csv("www/data copy/hotspot_yearly_data.csv")
HOTspots_yearly <- read.csv("~/Dropbox/HOTspots _data/HOTspots_yearly.csv")

## Shapefile
SA3 <- rgdal::readOGR("www/data/Australian_regions/Aus_regions.shp")
# SA3_data <- SA3[SA3$SA3_NAME16 %in% HOTspots_yearly$region,] # select the SA3 for which we have data for currently
#rm(SA3)

library(sf)
SA3_sf <- st_read("www/data/Australian_regions/Aus_regions.shp")
data <- HOTspots_yearly %>%
  filter(onset == "Community") %>%
  group_by(sample_type)

data_messy <- data %>%
  filter(organism ==  "E. coli", antimicrobial == "Ceftriaxone") %>%
  filter(sample_type == "All") %>%
  group_by(region) %>%
  select(-year) %>%
  summarise(res = (sum(resistant_yearly)/ sum(num_of_tests_yearly))*100)

# shapefile as sf or sp  object
SA3_sf_merged <- merge(SA3_sf, data_messy, by.x = "SA3_NAME16", by.y = "region", all.x = TRUE)





# Base R -----------------------------------------------------------------------------------------------------------------


## base R - plot
plot(SA3)


plot(st_geometry(SA3_sf))









# tmap -------------------------------------------------------------------------------------------------------------------

# Resources
https://bookdown.org/nicohahn/making_maps_with_r5/docs/tmap.html
https://bookdown.org/lexcomber/brunsdoncomber2e/Ch3.html#mapping-an-introduction-to-tmap
https://stackoverflow.com/questions/60657044/one-legend-with-same-scale-tmap/60677505#60677505
https://stackoverflow.com/questions/32890762/how-to-manipulate-tmap-legend
https://campus.datacamp.com/courses/visualizing-geospatial-data-in-r/raster-data-and-color?ex=9

## tmap
library(tmap)
tmap_mode("view") # in the view window
tmap_mode("plot") # in the plot window

#in plot mode
# + tm_layout(bg.color = "skyblue", inner.margins = c(0, .02, .02, .02))

# options
tmap_options()

map_name <- tm_shape() + # for the sf object
  tm_polygons() # set attribute, title
#  + tm_legend(show = FALSE)

tm_shape(SA3_sf_merged) + # for the sf object
  tm_polygons("res") # set attribute, title

data("World")
qtm(World, fill="HPI", fill.n = 9, fill.palette = "div",
    fill.title = "Happy Planet Index", fill.id = "name", 
    style = "gray", format = "World")

# continuous scale
tm_shape(World) + 
  tm_polygons(col = "gdp_cap_est",
              style = "cont")
# save
tmap_save()

# save from plot mode
tmap_save(map_name, filename = "world_map.png")

# save from view mode
tmap_save(map_name, filename = "world_map.html")



## use own palette



### Define palette ------------------------------------------------------------------------------------------------------------------
h_palette <- list(
  
  ## For heat map
  `heat`  = c(
    `green`         = "#629c25",
    `yellow orange` = "#FFF100", 
    `orangy yellow` = "#FFD100",
    `orange`        = "#fc8105", 
    `dark orange`   = "#e34f00",
    `red`           = "#ff0000",
    `red2`          = "#db0000",
    `deep red`      = "#C00004",
    `dark red`      = "#810000",
    `almost black`  = "#410000")
)

# for a continuous variable, Change distinct colours into a gradient
pal_num <- leaflet::colorNumeric(h_palette$heat, domain = 0:100)
pal_num10 <- pal_num(1:10)
pal_num100 <- pal_num(1:100)

# In qtm function, use for the fill.palette argument
qtm(World, fill="HPI", fill.n = 9, fill.palette = pal_num10,
    fill.title = "Happy Planet Index", fill.id = "name", 
    style = "gray", format = "World")

# in other function, set with the fill variable using the palette argument
tm_ceftriax_ecoli_all <- tm_shape(SA3_sf_merged) + # for the sf object
  tm_polygons("res",  title = "% Resistance",  palette  = pal_num100, lwd = 0.4, textNA = "Not included in study",  style = "cont", breaks = seq(0, 100, 10)) + 
  tm_layout("Ceftriaxone resistant E. coli (all)",
            legend.title.size = 1.5,
            legend.text.size = 1,
            legend.position = c("left","bottom"),
            legend.bg.color = "white",
            #legend.digits = 5,
            legend.bg.alpha = 1)


tmap_save(tm_ceftriax_ecoli_all,  filename = "Outputs_plot/ceftriax_ecoli_all.png")

# save from plot mode
tmap_save(map_name, filename = "world_map.png")









tm_shape(SA3_sf_merged) + 
  tm_polygons("res", breaks = seq(0, 100, 10), palette = pal_num100, style = "cont",
              lwd = 0.4, textNA = "Not included in study",  title = "% Resistance") +
  tm_layout("Ceftriaxone resistant E. coli (all)",
            legend.title.size = 1.7,
            legend.text.size = 1.3,
            legend.position = c("left","bottom"))




# Spplot -----------------------------------------------------------------------------------------------------------------
library(sp)
spplot(SA3_data, "fid", main = "Title", col = "transparent")





# ggplot -----------------------------------------------------------------------------------------------------------------

# being really slow
# takes a dataframe rather than spatial object
library(ggplot2)
library(broom)

data_messy_tidy <- broom::tidy(data_messy, region = "SA3_NAME16")

ggplot() +
  geom_polygon(data = data_messy_tidy, aes(x = long, y = lat), colour = "black", fill = NA)

# tidy data
ggplot() +
  geom_polygon(data = data_messy_tidy, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  theme_void() 

# sf object
ggplot() + 
  geom_sf(data = SA3_sf, size = 3, color = "black", fill = "cyan1") + 
  coord_sf()

ggplot(SA3_sf) +
  geom_sf(aes(fill=fid))




# leaflet ---------------------------------------------------------------------------------------------------------------------------------------------------------------

library(leaflet)
merged_data_2_1 <- merge(SA3_data, data_2_1, by.x="SA3_NAME16", by.y="region")


leaflet(data = merged_data_2_1) %>% # create a leaflet map
  fitBounds(lng1 = summary(SA3_data)$bbox[1], lat1 = summary(SA3_data)$bbox[2], lng2 = summary(SA3_data)$bbox[3], lat2 = summary(SA3_data)$bbox[4]) %>% # the starting position of the map
  addTiles(options = providerTileOptions(minZoom = 2))  %>%
  addPolygons(  fillColor = ~col_palette(merged_data$res),
                fillOpacity = ifelse(is.na(merged_data$res), 0, 0.7),
                weight = 2,
                opacity = 1,
                # color = "white",
                # dashArray = "3",
                highlight = highlightOptions( weight = 5, 
                                              color = "black", 
                                              bringToFront = TRUE))  %>%
  addLegend("bottomright", pal = col_palette, values = ~c(0:100), # also add values as ~percent_resistant_yearly
            title = "% resistance",
            opacity = 1)



