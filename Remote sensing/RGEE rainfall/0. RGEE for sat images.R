# RGEE
# calls the python API
# https://r-earthengine.github.io/ # examples
# https://www.dropbox.com/sh/oaxj3fa8qdo25r2/AAD5T67zbjZjjZweJD-R76mJa?dl=0&preview=Google_Earth_Engine.mp4
# https://github.com/r-spatial/rgee

# https://r-earthengine.com/image_02/
  

# 0. set up ---------------------------------------------------------------

# Packages

library(raster)
library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)

library(rgee)
library(reticulate)
library(googleCloudStorageR)
library(googledrive)

library(here)
library(lubridate)



dir <- list()
dir$proj <- getwd()
dir$TiwiShp <- "~/Documents/1. PhD/1_Modeling_and_Data/1_Modeling_and_Data/Administration/Tiwi_Islands"
dir$rainfall <- "Data_raw/Rainfall_TerraClimate"





# set up rgee
  # only for registered users
# rgee::ee_install() # sets up a python environment. Run and then restart R. 
# ee_install(py_env = "rgee")
# ee_check() 



# py_install("geemap")
# gm <- import("geemap")


# ee_Initialize()
# ee_Initialize(user = 'alysy.research@gmail.com', drive = T)
# ee_install()
# ee_install(py_env = "rgee")
# ee_clean_pyenv()
# ee_install_upgrade()


# ee_Initialize()
# ee_Initialize(user = 'alysy.research@gmail.com', drive = T)
# 
# # gcloud not working
# # install here
# # run the installation following this https://erikmartinjordan.com/gcloud-command-not-found
# # ee_clean_credentials('alysy.research@gmail.com')
# ee_get_earthengine_path()
# 
# ee_Authenticate()

ee_check()
# ◉  Python version
# ✔ [Ok] /Users/aryo/.virtualenvs/rgee/bin/python v3.8
# ◉  Python packages:
#   ✔ [Ok] numpy
# ✔ [Ok] earthengine-api
ee_check_python()
ee_check_python_packages()
ee_check_credentials()
# ◉  Credentials neccesaries for rgee:
#   Error in ee_check_credentials() : 
#   Does not exist Earth Engine credentials in their system. Try rgee::ee_Initialize() to fixed.
ee_Initialize()
ee_Initialize(user = 'alysy.research@gmail.com', drive = T)
# for a long time this didn't work with gcloud and i don't know what I did to fix it!
ee_check_credentials()




isl_shp <- st_read(here(dir$TiwiShp, "islands.shp")) %>% st_geometry %>% st_combine
isl_ee <- sf_as_ee(isl_shp)



# functions ---------------------------------------------------------------

# This function gets NDVI from Landsat 8 imagery.
L8_addNDVI <- function(image) {
  return(image$addBands(image$normalizedDifference(c("B5", "B4"))))
}

# This function gets NDWI from Landsat 8 imagery.
L8_getNDWI <- function(image) {
  return(image$normalizedDifference(c("B3", "B5")))
}

# This function gets NDVI from Landsat 5 imagery.
L5_getNDVI <- function(image) {
  return(image$normalizedDifference(c("B4", "B3")))
}
# This function masks cloudy pixels.
L8_cloudMask <- function(image) {
  clouds <- ee$Algorithms$Landsat$simpleCloudScore(image)$select("cloud")
  return(image$updateMask(clouds$lt(10)))
}



# visualisation parameters ------------------------------------------------

# For SRTM - srtm <- ee$Image("USGS/SRTMGL1_003")
vizParams_SRTM <- list(
  max = 500,
  min = 0,
  palette = c("#000000","#5AAD5A","#A9AD84","#FFFFFF")
)


# Sentinel-2
vizParams_Sen2 <- list(
  bands = c("B4", "B3", "B2"),
  min = 0,
  max = 2000,
  gamma = c(0.95, 1.1, 1)
)


# Landsat 8 - true
vizParams_L8_true

# Landsat 8 - false colour
vizParams_L8_false <- list(
  bands = c("B5", "B4", "B3"),
  min = 5000,
  max = 15000,
  gamma = 1.3
)

# NDVI 
vizParams_NDVI <- list(
  min = -0.5,
  max = 0.5,
  palette = c("FF0000", "FFFFFF", "0000FF")
)

# EVI
vizParams_EVI <- list(
  min = 0.0,
  max = 1.0,
  palette = c(
    'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
    '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
    '012E01', '011D01', '011301'
  )
)

# define visualization parameters and display image.
vizParams_NDWI <- list(
  min = 0.5,
  max = 1,
  palette = c('00FFFF', '0000FF')
)

# add map to view pane ----------------------------------------------------
Map$addLayer(
  eeObject = srtm,
  visParams =  vizParams_SRTM,
  name = 'SRTM',
  legend = TRUE
)




# Errors ------------------------------------------------------------------

# Map is used with a ee$List
# #> Error in py_call_impl(callable, dots$args, dots$keywords): RuntimeError: Evaluation error: argument "x" is missing, with no default.
# solution: wrap your function that is being mapped in ee_utils_pyfunc

# rgee is strict on integers. In python, the default is for whole numbers to be saved as integers. In R, you must specify
# Solution: use as.integer or add an L after the number

# Dates - I dont understand this error
# Era5 dataset
era_img <- ee$ImageCollection("ECMWF/ERA5/DAILY")$
  filterDate("2019-01-01", "2019-12-31")$
  first()
# Extracting init date
ee_date <- era_img$get('system:time_start')
ee_date$getInfo() # Silent error
#> [1] 112573440
eedate_to_rdate(ee_date = ee_date, timestamp = TRUE)
#> [1] 1.546301e+12


## The word repeat
# cannot use it as a variable name in R 
# when using in rgee, put backticks or quotes around the word 'repeat'











# Example 1. lights at night -------------------------------------------------
# Adds a band containing image date as years since 1991.

createTimeBand <- function(img) {
  year <- ee$Date(img$get('system:time_start'))$get('year')$subtract(1991L)
  ee$Image(year)$byte()$addBands(img)
}
# Map the time band creation helper over the night-time lights collection.

collection <- ee$
  ImageCollection('NOAA/DMSP-OLS/NIGHTTIME_LIGHTS')$
  select('stable_lights')$
  map(createTimeBand)
#Compute a linear fit over the series of values at each pixel, visualizing the y-intercept in green, and positive/negative slopes as red/blue.

col_reduce <- collection$reduce(ee$Reducer$linearFit())
col_reduce <- col_reduce$addBands(
  col_reduce$select('scale'))
ee_print(col_reduce)

#Create an interactive visualization!
  
Map$setCenter(9.08203, 47.39835, 3)
Map$addLayer(
  eeObject = col_reduce,
  visParams = list(
    bands = c("scale", "offset", "scale"),
    min = 0,
    max = c(0.18, 20, -0.18)
  ),
  name = "stable lights trend"
)













# Example 2.  extracting rainfall ---------------------------------------------
# Install and load tidyverse and sf R packages, and initialize the Earth Engine R API.

library(tidyverse)
library(sf)


#Read the nc shapefile.

nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

#Map each image from 2001 to extract the monthly precipitation (Pr) from the Terraclimate dataset
# TerraClimate: Monthly Climate and Climatic Water Balance for Global Terrestrial Surfaces, by the University of Idaho
# resolution: 4638.3 meters
terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate("2001-01-01", "2002-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("pr")) %>% # Select only precipitation bands
  ee$ImageCollection$toBands() %>% # from imagecollection to image
  ee$Image$rename(sprintf("PP_%02d",1:12)) # rename the bands of an image


# Extract monthly precipitation values from the Terraclimate ImageCollection through ee_extract. ee_extract works similar to raster::extract, you just need to define: the ImageCollection object (x), the geometry (y), and a function to summarize the values (fun).

ee_nc_rain <- ee_extract(x = terraclimate, y = nc["NAME"], sf = FALSE)


#Use ggplot2 to generate a beautiful static plot!
  
ee_nc_rain %>%
  pivot_longer(-NAME, names_to = "month", values_to = "pr") %>%
  mutate(month, month=gsub("PP_", "", month)) %>%
  ggplot(aes(x = month, y = pr, group = NAME, color = pr)) +
  geom_line(alpha = 0.4) +
  xlab("Month") +
  ylab("Precipitation (mm)") +
  theme_minimal()






# Example 2.1  extracting rainfall for the Tiwi Islands  ---------------------------------------------


#Read the shapefile.
# nc <- st_read(here(dir$TiwiShp, "islands.shp")) %>% st_geometry %>% st_combine
isl_shp <- st_read(here(dir$TiwiShp, "islands.shp")) %>% st_geometry %>% st_combine
date_start <- ymd("1997-01-01")
date_end <- ymd("2021-12-01")


date_seq <- seq(date_start, date_end, by = "month")
seq_date <- date_seq[-length(date_seq)]
seq_month <- month(seq_date)
seq_monthname <- month(seq_date, label = TRUE)
seq_year <- year(seq_date)


#Map each image from 2001 to extract the monthly precipitation (Pr) from the Terraclimate dataset
# TerraClimate: Monthly Climate and Climatic Water Balance for Global Terrestrial Surfaces, by the University of Idaho
# resolution: 4638.3 meters
terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate(as.character(date_start), as.character(date_end)) %>%
  ee$ImageCollection$map(function(x) x$select("pr")) %>% # Select only precipitation bands
  ee$ImageCollection$toBands() #%>% # from imagecollection to image
  #ee$Image$rename(paste(as.character(seq_month), as.character(seq_year), sep = "-" )) # rename the bands of an image to the months

# Extract monthly precipitation values from the Terraclimate ImageCollection through ee_extract. ee_extract works similar to raster::extract, you just need to define: the ImageCollection object (x), the geometry (y), and a function to summarize the values (fun).
rain_extracted <- ee_extract(x = terraclimate, y = isl_shp , sf = FALSE)
# write.csv(ee_nc_rain, here(dir$rainfall, "/Rainfall_97-21_raw.csv"))

colnames(rain_extracted) <- c("id", paste(as.character(seq_month), as.character(seq_year), sep = "-" ))



#Use ggplot2 to generate a beautiful static plot!

rain_df <- rain_extracted %>%
  pivot_longer(-id, names_to = "date", values_to = "rainfall") %>%
  mutate(month = as.integer(seq_month),
    month_name = seq_monthname,
         year = seq_year,
    rowReveal = row_number())

# write.csv(rain_df, here(dir$rainfall, "/Rainfall_97-21_clean.csv")) # write the name using the last two digits of the years 

rain_df %>% 
  ggplot(aes(x = month, y = rainfall, group = year, color = rainfall)) +
  geom_line(alpha = 0.4) +
  xlab("Month") +
  ylab("Precipitation (mm)") +
  theme_minimal()
#ggsave(here(dir$rainfall, "/Rainfall_97-21.jpeg"))


library(gganimate)

p <- rain_df %>% 
  ggplot(aes(x = month_name, y = rainfall, group = year, color = rainfall)) +
  geom_line(alpha = 0.4) +
  xlab("Month") +
  ylab("Precipitation (mm)") +
  theme_minimal()

p2 <- p + transition_reveal(rowReveal)
animate(p2, nframe = 299, fps = 10)




# Example 3. NDVI animation -----------------------------------------------
library(magick)
# library(rgee)
# library(sf)

# ee_Initialize()
# Define the regional bounds of animation frames and a mask to clip the NDVI data by.

mask <- system.file("shp/arequipa.shp", package = "rgee") %>%
  st_read(quiet = TRUE) %>%
  sf_as_ee()
region <- mask$geometry()$bounds()
# Retrieve the MODIS Terra Vegetation Indices 16-Day Global 1km dataset as an ee.ImageCollection and select the NDVI band.

col <- ee$ImageCollection('MODIS/006/MOD13A2')$select('NDVI')
# Group images by composite date

col <- col$map(function(img) {
  doy <- ee$Date(img$get('system:time_start'))$getRelative('day', 'year')
  img$set('doy', doy)
})
distinctDOY <- col$filterDate('2013-01-01', '2014-01-01')
# Define a filter that identifies which images from the complete collection match the DOY from the distinct DOY collection.

filter <- ee$Filter$equals(leftField = 'doy', rightField = 'doy')
# Define a join; convert the resulting FeatureCollection to an ImageCollection.

join <- ee$Join$saveAll('doy_matches')
joinCol <- ee$ImageCollection(join$apply(distinctDOY, col, filter))
# Apply median reduction among matching DOY collections.

comp <- joinCol$map(function(img) {
  doyCol = ee$ImageCollection$fromImages(
    img$get('doy_matches')
  )
  doyCol$reduce(ee$Reducer$median())
})
# Define RGB visualization parameters.

visParams = list(
  min = 0.0,
  max = 9000.0,
  bands = "NDVI_median",
  palette = c(
    'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
    '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
    '012E01', '011D01', '011301'
  )
)
# Create RGB visualization images for use as animation frames.

rgbVis <- comp$map(function(img) {
  do.call(img$visualize, visParams) %>%
    ee$Image$clip(mask)
})
# Define GIF visualization parameters.

gifParams <- list(
  region = region,
  dimensions = 600,
  crs = 'EPSG:3857',
  framesPerSecond = 10
)
# Get month names

dates_modis_mabbr <- distinctDOY %>%
  ee_get_date_ic %>% # Get Image Collection dates
  '[['("time_start") %>% # Select time_start column
  lubridate::month() %>% # Get the month component of the datetime
  '['(month.abb, .) # subset around month abbreviations
# Use ee_utils_gif_* functions to render the GIF animation and add some texts.

animation <- ee_utils_gif_creator(rgbVis, gifParams, mode = "wb")
animation %>%
  ee_utils_gif_annotate(
    text = "NDVI: MODIS/006/MOD13A2",
    size = 15, color = "white",
    location = "+10+10"
  ) %>%
  ee_utils_gif_annotate(
    text = dates_modis_mabbr,
    size = 30,
    location = "+290+350",
    color = "white",
    font = "arial",
    boxcolor = "#000000"
  ) # -> animation_wtxt

# ee_utils_gif_save(animation_wtxt, path = "raster_as_ee.gif")





# Example 4.  SRTM --------------------------------------------------------
srtm <- ee$Image("USGS/SRTMGL1_003")

Map$addLayer(
  eeObject = srtm,
  visParams =  vizParams_SRTM,
  name = 'SRTM'
)



# Example 5. Landsat ------------------------------------------------------
# Task: Specify an image collection, filter it for a date range, and select one product:
# filter a collection like this: ee$ImageCollection()$filterDate()
evi <- ee$ImageCollection('LANDSAT/LC08/C01/T1_8DAY_EVI')$
  filterDate('2017-01-01', '2017-12-31')$
  select('EVI')$
  toBands()  # Convert  ImageCollection to a multi-band Image and display the band names.

class(evi)
ee_print(evi)

# Task: Set a region of interest and centre the map display on it:
# ee$Geometry$Polygon

Map$centerObject(isl_ee, 9)
Map$addLayer(isl_ee)

# region.ithaca <- ee$Geometry$Rectangle(
#   coords = c(-76.7, 42.2, -76.2, 42.7),
#   proj = "EPSG:4326",
#   geodesic = FALSE
# )
# Map$centerObject(region.ithaca, 11)



#Task: Select one date and display its EVI in a map window with this visualization:
evi02jul <- evi$select("20170704_EVI")
Map$addLayer(evi02jul, vizParams_EVI, 'Landsat 8 EVI 02-July-2017')



# Converting Earth Engine objects into terra and raster ------------------

# For large rasters this function by default uses your Google Drive storage to export the GEE raster and then import to R, so you will be asked allow tidyverse packages to use your Google Drive.
# 
# For example, we will import the EVI stack and process it in R.
# 
# We need to bound it with the region argument, a ee$Geometry$Rectangle). We defined a region above, for the map display, and export a downscaled EVI (1 km nominal pixel):

class(evi)
evi.r <- ee_as_raster(evi, 
                      region=region.ithaca,
                      via = "drive",
                      scale = 1000)
class(evi.r) # raster stack
evi.r <- rast(evi.r)  # convert to terra
class(evi.r)
summary(evi.r)
plot(evi.r)
names(evi.r)


# Now we can do some typical raster functions from within R.
# 
# For example, k-means clustering to find more-or-less homogeneous zones within one date (10-February, in the winter):

# one image
k5 <- kmeans(as.vector(evi.r["20170210_EVI"]), centers = 5, nstart = 5)
print(k5)
print(k5$centers)

k5.rast <- rast(evi.r[[1]]) # create a new raser the same as the evi raster
values(k5.rast) <- k5$cluster # overwrite the values with the K means cluster
plot(k5.rast) # plot the clusters



# Use five images from 20-July through 29-August. This period usually has some drought, and is when field crops are ripening but forest continues well-vegetated.
# find the image names
names(evi.r)

evi.r.3 <- evi.r[[26:31]] # select the images
summary(evi.r.3)

k5 <- kmeans(as.matrix(evi.r.3), centers = 5, nstart = 5) # run the clustering on each image
print(k5$centers) # check the clusters for each image
k5$size
k5$betweenss
k5$tot.withinss
k5$betweenss/k5$totss # The proportion of variance explained

k5.rast <- rast(evi.r[[1]])
values(k5.rast) <- k5$cluster
plot(k5.rast)




# Example. sentinel-2 --------------------------------------------------------------

# Load an image collection
sen <- ee$ImageCollection("COPERNICUS/S2")$
  filterBounds(ee$Geometry$Point(-70.48, 43.3631))$
  filterDate('2019-01-01', '2019-12-31')$
  sort('CLOUDY_PIXEL_PERCENTAGE')$
  first()

# Define the visualization parameters.
vizParams <- list(
  bands = c("B4", "B3", "B2"),
  min = 0,
  max = 2000,
  gamma = c(0.95, 1.1, 1)
)

# Center the map and display the image.
Map$centerObject(sen, 7)
# You can see it
m1 <- Map$addLayer(sen, vizParams, 'first')
m1



# Example. sentinel-2 for the Tiwi Islands --------------------------------------------------------------

# Load an image collection
sen <- ee$ImageCollection("COPERNICUS/S2")$
  filterBounds(isl_ee)$ # this selects only one tile... the documentation m
  filterDate('2019-01-01', '2019-12-31')$
  sort('CLOUDY_PIXEL_PERCENTAGE')$ # apply sort after the filters to save computational time
  first()

# access the metadata
sen_metadata <- ee_print(sen)
sen_metadata$img_bands_names
sen$metadata
attr(sen, "metadata")

# Define the visualization parameters.
vizParams <- list(
  bands = c("B4", "B3", "B2"),
  min = 0,
  max = 2000,
  gamma = c(0.95, 1.1, 1)
)

# Center the map and display the image.
Map$centerObject(sen, 9)
# You can see it
m1 <- Map$addLayer(sen, vizParams, 'first')
m1
# to see this in QGIS copy m1$rgee$tokens into XYZ Tiles
m1$rgee$tokens








# Example. Landsat 8 and ndvi ------------------------------------------------------

# This function gets NDVI from Landsat 8 imagery.
addNDVI <- function(image) {
  return(image$addBands(image$normalizedDifference(c("B5", "B4"))))
}

# This function masks cloudy pixels. - using simple cloud score - pretty basic
cloudMask <- function(image) {
  clouds <- ee$Algorithms$Landsat$simpleCloudScore(image)$select("cloud")
  return(image$updateMask(clouds$lt(10)))
}

# Load a Landsat collection, map the NDVI and cloud masking functions over it.
collection <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filterBounds(ee$Geometry$Point(c(-122.262, 37.8719)))$
  filterDate("2014-03-01", "2014-05-31")$
  map(addNDVI)$
  map(cloudMask)

# Reduce the collection to the mean of each pixel and display.
meanImage <- collection$reduce(ee$Reducer$mean())
vizParams <- list(
  bands = c("B5_mean", "B4_mean", "B3_mean"),
  min = 0,
  max = 0.5
)
Map$centerObject(ee$Geometry$Point(c(-122.262, 37.8719)), 9)
Map$addLayer(
  eeObject = meanImage,
  visParams = vizParams_L8,
  name = "mean"
)



## With a different cloud masking 
# Cloud masking function. - using quality assessment pixel (QA_pixel) mask
maskL8sr <- function(image) { # in the example, image is "LANDSAT/LC08/C01/T1_SR"
  cloudShadowBitMask <- bitwShiftL(1, 3)
  cloudsBitMask <- bitwShiftL(1, 5)
  qa <- image$select('pixel_qa')
  mask <- qa$bitwiseAnd(cloudShadowBitMask)$eq(0)$
    And(qa$bitwiseAnd(cloudsBitMask)$eq(0))
  image$updateMask(mask)
}

# Load a Landsat collection, map the NDVI and cloud masking functions over it.
collection <- ee$ImageCollection("LANDSAT/LC08/C01/T1_SR")$
  filterBounds(ee$Geometry$Point(c(-122.262, 37.8719)))$
  filterDate("2014-03-01", "2014-05-31")$
  map(addNDVI)$
  map(cloudMask)




# Load a Landsat collection, map the NDVI and cloud masking functions over it.
# Trying for the Tiwi Islands
collection <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filterBounds(isl_ee)$
  filterDate("2021-03-01", "2021-07-01")$
  map(addNDVI)$
  map(cloudMask)
# Reduce the collection to the mean of each pixel and display.
meanImage <- collection$reduce(ee$Reducer$mean())
vizParams <- list(
  bands = c("B5_mean", "B4_mean", "B3_mean"),
  min = 0,
  max = 0.5
)
Map$centerObject(isl_ee, 9)
Map$addLayer(
  eeObject = meanImage,
  visParams = vizParams_L8,
  name = "mean"
)




# Load a region in which to compute the mean and display it.
counties <- ee$FeatureCollection("TIGER/2016/Counties")
santaClara <- ee$Feature(counties$filter(
  ee$Filter$eq("NAME", "Santa Clara")
)$first())

Map$addLayer(
  eeObject = santaClara,
  visParams = list(palette = "yellow"),
  name = "Santa Clara"
)

# Get the mean of NDVI in the region.
mean <- meanImage$select("nd_mean")$reduceRegion(
  reducer = ee$Reducer$mean(),
  geometry = santaClara$geometry(),
  scale = 30
)

# Print mean NDVI for the region.
cat("Santa Clara spring mean NDVI:", mean$get("nd_mean")$getInfo())

