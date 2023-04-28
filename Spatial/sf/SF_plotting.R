## Ploting maps of sf objects

install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

library("ggplot2")
theme_set(theme_bw())
library("sf")


## Country data 
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)


# Basic 
ggplot(data = world) +
  geom_sf()

# Colour
ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# Zoom in to one place
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)

# Scale bar and north arrow
library("ggspatial")
ggplot(data = world) +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97))


# Final
ggplot(data = world) + 
  geom_sf(fill= “antiquewhite”) + 
  geom_text(data= world_points, aes(x=X, y=Y, label=name), 
            color = “darkblue”, fontface = “bold”, check_overlap = FALSE) + 
  annotate(geom = “text”, x = -90, y = 26, label = “Gulf of Mexico”, 
           fontface = “italic”, color = “grey22”, size = 6) + 
  annotation_scale(location = “bl”, width_hint = 0.5) + 
  annotation_north_arrow(location = “bl”, which_north = “true”, 
                         pad_x = unit(0.75, “in”), pad_y = unit(0.5, “in”),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE) +
  xlab(“Longitude”) +
  ylab(“Latitude”) + 
  ggtitle(“Map of the Gulf of Mexico and the Caribbean Sea”) + 
  theme(panel.grid.major = element_line(color = gray(.5), linetype = “dashed”, size = 0.5), 
        panel.background = element_rect(fill = “aliceblue”))