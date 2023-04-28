## Plotting and mapping for spatial analysis in R

## Resources:
# https://jakubnowosad.com/SIGR2021/workshop1/workshop1_jn.html#1
# https://jakubnowosad.com/whyr_19/workshop/#1


## To look into:
# Mapview
# mapdeck




# terra and gridded data and plotting.
# sf and vector layers and plotting. And bringing them together

## Session 1: Gridded data
# Package focus: terra
# other packages: raster,

# class of data - most often .tif files.
# reclassify
# crop
# stack - if they have different extents, resolutions (reproject)

# tmap
# rasterVis


## Session 2: Vector data
# Package focus: sf
# other packages: sp, rgdal, stars

# use fire scars?

# plot
# ggplot
# leaflet - interactive
# tmap
# mapview https://github.com/r-spatial/mapview - use the argument zcol


install.packages("spDataLarge", repos = "https://geocompr.r-universe.dev")


library(tmap)
library(spData)
library(spDataLarge)

data()
data(package = .packages(all.available = TRUE))

data("nz", package = "spData")

# Data from sp
# islands, precip, rivers, state, stackloss, uspop, volcano

# Data from sp large:
# alaska, columbus, elev, hawaii, nz, nz_height, us_states, census_de, comm (look at this for the plant PA), elevation, ndvi, nlcd, random_points,

# Data from the tmap package
# land, metro, rivers

# From boot: city
# From camtrapR: camtraps

install.packages("spData")
library(spData)

library(tmap)
data(land)

# hopkins: Hopkins burnt savanna herb remains
data(hopkins)
# A 20m square is divided into 40 by 40 0.5m quadrats. Observations are in tens of grams of herb
# remains, 0 being from 0g to less than 10g, and so on. Analysis was mostly conducted using the
# interior 32 by 32 grid.
data(hopkins)
image(1:32, 1:32, hopkins[5:36,36:5], breaks=c(-0.5, 3.5, 20),
      col=c("white", "black"))








# getisord: Getis-Ord remote sensing example data
data(getisord)
image(go_x, go_y, t(matrix(go_xyz$val, nrow = 16, ncol=16, byrow = TRUE)), asp = 1)
text(go_xyz$x, go_xyz$y, go_xyz$val, cex = 0.7)
polygon(c(195, 225, 225, 195), c(195, 195, 225, 225), lwd = 2)
title(main = "Getis-Ord 1996 remote sensing data")








# SF ----------------------------------------------------------------------

## Buffer 
# 1. You believe that the animal that you found at these 3 points is affected by the variation in topography.
# But you dont know how far around the animal affects them the most
# You want to test 100m (the average distance moved during breeding season), 800m (the average home range size), and 3km (the maximum movement distance recorded for the species)
# create 3 new object which are circles around your sampling sites for each of these distnace






# to look into ------------------------------------------------------------

# shapefile of Aus

# ALA for a threatened species around Darwin
  # e.g. gouldian finches
# count the number

# priority environmental areas?


# Australian capital cities
  # use these to buffer around


# Aus census data ----------------------------------------------------------
# sf data --------------------------------------------------------------------
install.packages("Census2016")
install.packages("eechidna")
install.packages("ggthemes")

# library(knitr)
library(dplyr)
library(ggplot2)
library(eechidna)
data(tpp16) ## from eechidna - two party preferred
data(abs2016)## from eechidna
data(fp16) ## from eechidna - first preference
data(tcp16) ## from eechidna - two candidate preference


# Join 2016 election and Census
data16 <- left_join(tpp16 %>% select(LNP_Percent, UniqueID), abs2016, by = "UniqueID")


# points to polygons?







library(ggthemes)
nat_map16 <- nat_map_download(2016)
nat_data16 <- nat_data_download(2016)

fp16 %>% merge(nat_map16, by.x="DivisionNm", by.y="elect_div")

fp16 %>% merge(nat_map16, by.x="DivisionNm", by.y="elect_div") %>% 
ggplot() + 
  geom_polygon(aes(x=long, y=lat, group=group, fill=PartyNm)) +
  theme_map() + coord_equal() + theme(legend.position="bottom")


ggplot(aes(map_id=id), data=nat_data16) +
  geom_map(aes(fill=state), map=nat_map16, col = "grey50") +
  expand_limits(x=nat_map16$long, y=nat_map16$lat) + 
  theme_map() + coord_equal()


# Get the electorate winners
map.winners <- fp16 %>% filter(Elected == "Y") %>% 
  select(DivisionNm, PartyNm) %>% 
  merge(nat_map16, by.x="DivisionNm", by.y="elect_div")

# Grouping
map.winners$PartyNm <- as.character(map.winners$PartyNm)
map.winners <- map.winners %>% arrange(group, order)

# Combine Liberal and National parties
map.winners <- map.winners %>% 
  mutate(PartyNm = ifelse(PartyNm %in% c("NATIONAL PARTY", "LIBERAL PARTY"), "LIBERAL NATIONAL COALITION", PartyNm))

# Colour cells to match that parties colours
# Order = Australian Labor Party, Independent, Katters, Lib/Nats Coalition, Palmer, The Greens
partycolours = c("#FF0033", "#000000", "#CC3300", "#0066CC", "#FFFF00", "#009900")


ggplot(data=map.winners) + 
  geom_polygon(aes(x=long, y=lat, group=group, fill=PartyNm)) +
  scale_fill_manual(name="Political Party", values=partycolours) +
  theme_map() + coord_equal() + theme(legend.position="bottom")


## GGPLOT
  # geom_sf
  # geom_polygon
  # themes_bw themes_void
  # labs
ggplot() + 
  geom_sf()

## TMAP
  # tm_shape
  # tm_polygons
# classed colour bins
  # tm_layout
# basemap, north arrow, and scale bar
# rosm package for base map
# to say but not ues. mapboxapi to get mapbox basemaps
  #   tm_facets()

tm_shape(hennepin_black) + 
  tm_polygons()

tm_shape(hennepin_black) + 
  tm_polygons() + 
  tm_layout(title = "Percent Black\nby Census tract",
            frame = FALSE,
            legend.outside = TRUE,
            bg.color = "grey70",
            legend.hist.width = 5,
            fontfamily = "Verdana")

tm_shape(hennepin_tiles) + 
  tm_rgb() + 
  tm_shape(hennepin_black) + 
  tm_polygons(col = "percent",
              style = "jenks",
              n = 5,
              palette = "Purples",
              title = "2020 US Census",
              alpha = 0.7) +
  tm_layout(title = "Percent Black\nby Census tract",
            legend.outside = TRUE,
            fontfamily = "Verdana") + 
  tm_scale_bar(position = c("left", "bottom")) + 
  tm_compass(position = c("right", "top")) + 
  tm_credits("(c) Mapbox, OSM    ", 
             bg.color = "white",
             position = c("RIGHT", "BOTTOM"))
# position arguments in tm_scale_bar(), tm_compass(), and tm_credits()

tm_facets(by = "variable", scale.factor = 4)

tmap_mode("view")
tmap_mode("plot")

  
# more census dat  --------------------------------------------------------

library(magrittr) 
library(data.table) 
library(Census2016) # for SA2. If theres a way to access SA 2 in r that would be IDEAL






# terra data --------------------------------------------------------------
install.packages("elevatr")

data(lake)
elevation <- get_elev_raster(lake, z = 9)
plot(elevation)
plot(lake, add = TRUE)

library(elevatr)

