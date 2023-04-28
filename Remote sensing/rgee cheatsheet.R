## RGEE how to



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
# library(rgeeExtra)
# remotes::install_github("r-earthengine/rgeeExtra")



# Load shapefile ----------------------------------------------------------
## Example 1 - from package rgee
ee_arequipa <- system.file("shp/arequipa.shp", package="rgee") %>% 
  st_read() %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  sf_as_ee() %>% 
  ee$FeatureCollection$geometry()

## Example 2 - from package sf
ee_x <- st_read(system.file("shape/nc.shp", package = "sf")) %>%
  sf_as_ee()
roi <- ee_x$first()$geometry()

## Tiwi islands
isl_shp <- st_read(file.path(dir$TiwiShp, "islands.shp")) %>% st_geometry %>% st_combine
isl_ee <- sf_as_ee(isl_shp)

## Tiwi islands with a buffer
islBuf_shp <- st_read(file.path(dir$TiwiShp, "islands.shp")) %>%
  st_geometry %>%
  st_combine %>%
  st_buffer(dist = 3000) # add a buffer
islBuf_ee <- sf_as_ee(islBuf_shp)

## Check the shapefiles by plotting them
# plot(islBuf_shp)
# plot(isl_shp, add = TRUE)

## Add a point with a buffer
EE_geom <- ee$Geometry$Point(c(-70.06240, -6.52077))$buffer(5000)

## Add a point with a buffer and visualise it
vector <- ee$Geometry$Point(-77.011,-11.98) %>%
  ee$Feature$buffer(50*1000)
Map$centerObject(vector)
Map$addLayer(vector) # eeObject is a ee$Geometry$Polygon.


## Load a place in the US using the census data of the counties
counties <- ee$FeatureCollection("TIGER/2016/Counties")
santaClara <- ee$Feature(counties$filter(
  ee$Filter$eq("NAME", "Santa Clara")
)$first())

Map$addLayer(
  eeObject = santaClara,
  visParams = list(palette = "yellow"),
  name = "Santa Clara"
)


# Functions to create vegetation indices ---------------------------------

# This function adds NDVI to Landsat 8 imagery.
L8_addNDVI <- function(image) {
  return(image$addBands(image$normalizedDifference(c("B5", "B4"))))
}

# This function gets NDVI from Landsat 8 imagery.
L8_getNDVI <- function(image) {
  return(image$normalizedDifference(c("B5", "B4")))
}

# This function adds NDVI to Landsat 8 imagery.
L8_addNDWI <- function(image) {
  return(image$addBands(image$normalizedDifference(c("B3", "B5"))))
}

# This function gets NDWI from Landsat 8 imagery.
L8_getNDWI <- function(image) {
  return(image$normalizedDifference(c("B3", "B5")))
}


# This function adds NDVI to Landsat 5 imagery.
L8_addNDWI <- function(image) {
  return(image$addBands(image$normalizedDifference(c("B3", "B5"))))
}
# This function gets NDVI from Landsat 5 imagery.
L5_getNDVI <- function(image) {
  return(image$normalizedDifference(c("B4", "B3")))
}

# This function adds NDVI to Landsat 8 imagery.
S2_addNDVI <- function(image) {
  return(image$addBands(image$normalizedDifference(c("B8", "B4"))))
}

# This function gets NDVI from Landsat 8 imagery.
S2_getNDVI <- function(image) {
  return(image$normalizedDifference(c("B8", "B4")))
}



# Compute the EVI using an expression.
S2_addEVI <- function(image){
  EVI <- image$expression(
    expression = '2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))',
    opt_map =  list(
      'NIR' = image$select('B8'),
      'RED' = image$select('B4'),
      'BLUE' = image$select('B2')
    ))$rename("EVI")
  
  image = image$addBands(EVI)
  
  return(image)
}

# Compute the EVI using an expression.
L8TOA_addEVI = image$expression(
  expression = '2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))',
  opt_map =  list(
    'NIR' = image$select('B5'),
    'RED' = image$select('B4'),
    'BLUE' = image$select('B2')
  )
)


# # alternative method Compute the SAVI using an expression.
# S2_addSAVI <- function(image){
#   return(image$addBands(image$expression(
#     expression = '1.5 * ((NIR - RED) / (NIR - RED + 0.5))',
#     opt_map =  list(
#       'NIR' = image$select('B8'),
#       'RED' = image$select('B4')
#       )
#     )
#     )
#   )
#   }

# Compute the SAVI using an expression.
S2_addSAVI <- function(image){
  SAVI <- image$expression(
    expression = '1.5 * ((NIR - RED) / (NIR - RED + 0.5))',
    opt_map =  list(
      'NIR' = image$select('B8'),
      'RED' = image$select('B4')))$rename("SAVI")
  image = image$addBands(SAVI)
  
  return(image)
}


# Clip to a shapefile -----------------------------------------------------

landsat8FiltClouds.median().clip(country);




# cloud-mask --------------------------------------------------------------

## Sentinel-2
# cloud mask function for Sentinel-2
#https://github.com/ricds/DL_RS_GEE/blob/main/rgee_basics.R
S2_QACloudMask = function(image) {
  qa = image$select('QA60');
  
  # Bits 10 and 11 are clouds and cirrus, respectively.
  cloudBitMask = bitwShiftL(1,10)
  cirrusBitMask = bitwShiftL(1, 11)
  
  # Both flags should be set to zero, indicating clear conditions.
  mask_data = qa$bitwiseAnd(cloudBitMask)$eq(0)$And(qa$bitwiseAnd(cirrusBitMask)$eq(0));
  
  return(image$updateMask(mask_data)$divide(10000))
}

## Landsat 8
L8_QACloudMask <- function(image) { # in the example, image is "LANDSAT/LC08/C01/T1_SR"
  cloudShadowBitMask <- bitwShiftL(1, 3)
  cloudsBitMask <- bitwShiftL(1, 5)
  qa <- image$select('pixel_qa')
  mask <- qa$bitwiseAnd(cloudShadowBitMask)$eq(0)$
    And(qa$bitwiseAnd(cloudsBitMask)$eq(0))
  image$updateMask(mask)
}

# Functions to create a simple cloud mask ---------------------------------
L8_simpleCloudMask <- function(image) {
  clouds <- ee$Algorithms$Landsat$simpleCloudScore(image)$select("cloud")
  return(image$updateMask(clouds$lt(10)))
}



# metadata ----------------------------------------------------------------

#print(ee_img$getInfo())

img$get("system:time_start")
img$get("MEAN_SOLAR_AZIMUTH_ANGLE")$getInfo()

ee_crs <- collection$first()$projection()$getInfo()$crs

# Merge two image collections ---------------------------------------------

# ee.ImageCollection(collection7$merge(collection8))




# Dates -------------------------------------------------------------------
# GEE handles dates in a way that R cannot read
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







# Visualisation parameters -----------------------------------------------------------------------------------------


# SRTM ----------------------------------------------------------------------------------
# srtm <- ee$Image("USGS/SRTMGL1_003")
vizParams_SRTM <- list(
  max = 500,
  min = 0,
  palette = c("#000000","#5AAD5A","#A9AD84","#FFFFFF")
)


# sentinal-2 ----------------------------------------------------------------------------------

# bands 4,3,2 for true colour
# try max at 6000
# Sentinel-2 - raw, TOA?
vizParams_Sen2 <- list(
  bands = c("B4", "B3", "B2"),
  min = 0,
  max = 2000,
  gamma = c(0.95, 1.1, 1)
)

# Sentinel-2 - reduced over a collection by the mean
vizParams_Sen2_mean <- list(
  bands = c("B4_mean", "B3_mean", "B2_mean"),
  min = 0,
  max = 2000,
  gamma = c(0.95, 1.1, 1)
)

# Sentinel-2 - reduced over a collection by the median
vizParams_Sen2_median <- list(
  bands = c("B4_median", "B3_median", "B2_median"),
  min = 0,
  max = 2000,
  gamma = c(0.95, 1.1, 1)
)

# ee$ImageCollection('COPERNICUS/S2_SR')$
vizParams_Sen2SR <- list(
  bands = c("B4", "B3", "B2"),
  min = 0,
  max = 0.3
)

# Landsat ---------------------------------------------------------------------------------------------------------------------------
# Landsat 8 -------------------------------------------------
# L8 - LANDSAT_LC08_C02_T1_L2 uses "SR_B4", "SR_B3", "SR_B2"
# Landsat 8 - true
vizParams_L8_true1 <- list(
  bands = c("B4", "B3", "B2"),
  min = 5000,
  max = 15000,
  gamma = 1.3
)

# Landsat 8 - true
# from here: https://github.com/r-spatial/rgee/blob/eaea0dbf503b25caaebf0a0b3c0aa163bc86fadd/R/ee_as_thumbnail.R
# img <- ee$Image("LANDSAT/LC08/C01/T1_SR/LC08_038029_20180810")$
#'   select(c("B4", "B3", "B2"))
#' Map$addLayer(img, list(min = 0, max = 5000, gamma = 1.5))
vizParams_L8_true2 <- list(
  bands = c("B4", "B3", "B2"),
  min = 0,
  max = 5000,
  gamma = 1.5
)

vizParams_L8_divtrue1 <- list(
  min = 0,
  max = 0.5,
  bands = c("B4", "B3", "B2"),
  gamma = c(0.95, 1.1, 1)
)

# From here: https://github.com/r-spatial/rgee/blob/30eac0d47e76d973ae09751b780e16237af7cc27/R/ee_image.R
# img <- ee$Image("LANDSAT/LC08/C01/T1_SR/LC08_038029_20180810")$
#'   select(c("B4", "B3", "B2"))$
#'   divide(10000)
#'
#' # OPTIONAL display it using Map
#' Map$centerObject(eeObject = img)
#' Map$addLayer(eeObject = img, visParams = list(max = 0.4,gamma=0.1))
vizParams_L8_divtrue2 <- list(
  max = 0.4,
  bands = c("B4", "B3", "B2"),
  gamma = 0.1
)



# Landsat 8 - true
vizParams_L8_true1_mean <- list(
  bands = c("B4_mean", "B3_mean", "B2_mean"),
  min = 5000,
  max = 15000,
  gamma = 1.3
)

vizParams_L8_true2_mean <- list(
  bands = c("B4_mean", "B3_mean", "B2_mean"),
  min = 0,
  max = 5000,
  gamma = 1.5
)

vizParams_L8_divtrue1_mean <- list(
  min = 0,
  max = 0.5,
  bands = c("B4_mean", "B3_mean", "B2_mean"),
  gamma = c(0.95, 1.1, 1)
)


vizParams_L8_divtrue2_mean <- list(
  max = 0.4,
  bands = c("B4_mean", "B3_mean", "B2_mean"),
  gamma = 0.1
)

vizParams_L8_true2_median <- list(
  max = 0.4,
  bands = c("B4_median", "B3_median", "B2_median"),
  gamma = 0.1
)

# landsat 8 TOA - "LANDSAT/LC08/C01/T1_TOA"
# make the max smaller to see it "brighter"
vizParams_L8TOA_true <- list(
  min = 0,
  max = 1,
  bands = c("B4", "B3", "B2")
)



# Landsat 8 - false colour
vizParams_L8_false <- list(
  bands = c("B5", "B4", "B3"),
  min = 5000,
  max = 15000,
  gamma = 1.3
)

vizParams_L8TOA_false <- list(
  min = 0,
  max = 0.5,
  bands = c("B5", "B4", "B3"),
  gamma = c(0.95, 1.1, 1)
)

# Landsat 5 -------------------------------------------------
# landsat 5
# 'LANDSAT/LT05/C02/T1'
vizParams_L5_true <- list(
  max = 128,
  bands = c("B4", "B3", "B2")
)



vizParams_L8TOA_false <- list(
  min = 0,
  max = 0.5,
  bands = c("B5", "B4", "B3"),
  gamma = c(0.95, 1.1, 1)
)# NDVI --------------------------------------------------------------------------------------------
# NDVI 
vizParams_NDVI <- list(
  min = -0.5,
  max = 0.5,
  palette = c("FF0000", "FFFFFF", "0000FF")
)



# EVI ---------------------------------------------------------------------------------------
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



# NDWI ---------------------------------------------------------------------------------------
# Normalised Difference Water Index
vizParams_NDWI <- list(
  min = 0.5,
  max = 1,
  palette = c('00FFFF', '0000FF')
)

# Visualisation parameters finished ---------






# Testing the visualisation parameter for Landsat 8 -----------------------------------
# For "LANDSAT/LC08/C01/T1_SR" and not divided, use true2
# 
# collection <- ee$ImageCollection("LANDSAT/LC08/C01/T1_SR")$
#   #divide(10000)$ # LANDSAT/LC08/C01/T1
#   filterDate("2020-03-01", "2023-04-01")$
#   filterBounds(isl_ee)
# 
# 
## testing the visualisation parameters for landsat ---- TRUE 2
# img_filt_cloudPerc <- collection$
#   filterMetadata("CLOUD_COVER", "less_than", 20)$
#   reduce(ee$Reducer$mean())
# 
# divtrue1 <- Map$addLayer(
#   eeObject = img_filt_cloudPerc,
#   visParams = vizParams_L8SR_scaled_true1_mean,
#   name = "divtrue1")
# 
# divtrue2 <- Map$addLayer(
#   eeObject = img_filt_cloudPerc,
#   visParams = vizParams_L8SR_scaled_true2_mean,
#   name = "divtrue2")
# 
# true1 <- Map$addLayer(
#   eeObject = img_filt_cloudPerc,
#   visParams = vizParams_L8SR_true1_mean,
#   name = "true1")
# 
# true2 <- Map$addLayer(
#   eeObject = img_filt_cloudPerc,
#   visParams = vizParams_L8SR_true2_mean,
#   name = "true2")
# 
# divtrue1 | divtrue2
# 
# true1 | true2




# Comparing reducer methods on Sentinel-2 ---------------------------------
# collection <- ee$ImageCollection("COPERNICUS/S2")$
#   filterDate("2021-03-01", "2022-05-01")$
#   filterBounds(isl_ee)
# 
# ## Filtering
# # note IMAGE_QUALITY is for Landsat
# img_filt_mean1 <- collection$
#   # filter(ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE", 20)) # try this, not sure if it will be faster
#   # filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE',20))$ 
#   filterMetadata("CLOUDY_PIXEL_PERCENTAGE", "less_than", 20)$
#   # mean() # try this
#   reduce(ee$Reducer$mean())
# 
# img_filt_median1 <- collection$
#   filterMetadata("CLOUDY_PIXEL_PERCENTAGE", "less_than", 20)$
#   # median() # try this
#   reduce(ee$Reducer$median())
# 
# img_filt_mean2 <- collection$
#   # filter(ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE", 20)) # try this, not sure if it will be faster
#   # filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE',20))$ 
#   filterMetadata("CLOUDY_PIXEL_PERCENTAGE", "less_than", 20)$
#   mean() # try this
# #reduce(ee$Reducer$mean())
# 
# img_filt_median2 <- collection$
#   filterMetadata("CLOUDY_PIXEL_PERCENTAGE", "less_than", 20)$
#   median() # try this
# # reduce(ee$Reducer$median())
# 
# 
# Map$centerObject(isl_ee)
# 
# map_filt_mean1 <- Map$addLayer(
#   eeObject = img_filt_mean1,
#   visParams = vizParams_Sen2_mean,
#   name = "Mean1")
# 
# map_filt_median1 <- Map$addLayer(
#   eeObject = img_filt_median1,
#   visParams = vizParams_Sen2_median,
#   name = "Median1")
# 
# 
# 
# map_filt_mean2 <- Map$addLayer(
#   eeObject = img_filt_mean2,
#   visParams = vizParams_Sen2,
#   name = "Mean 2")
# 
# map_filt_median2 <- Map$addLayer(
#   eeObject = img_filt_median2,
#   visParams = vizParams_Sen2,
#   name = "Median 2")
# 
# 
# 
# ## Compare using mean or median for the reducer method
# map_filt_mean1 | map_filt_median1 # median is so much better
# 
# map_filt_mean2 | map_filt_median2 #median again
# 
# 
# ## Compare using the direct functin or the reducer() for the reducer method
# map_filt_mean1 | map_filt_mean2 # no difference
# 
# map_filt_median1 | map_filt_median2 # no difference




###################
## Full examples ## ------------------------------------------------------------------------------------------------------------------------------------------------------------
###################

# Example 1. Lights at night -----------------------------------------------------------------------------------------------------------------
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




# Example 2.  Extracting rainfall ---------------------------------------------------------------------------------------------------------------------------
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






# Example 3. NDVI animation -----------------------------------------------------------------------------------------------------------------------------
library(magick)
# library(rgee)
# library(sf)

## Define the area
mask <- system.file("shp/arequipa.shp", package = "rgee") %>%
  st_read(quiet = TRUE) %>%
  sf_as_ee()
region <- mask$geometry()$bounds()

## Use MODIS NDVI
col <- ee$ImageCollection('MODIS/006/MOD13A2')$ # MODIS Terra Vegetation Indices 16-Day Global 1km dataset
  select('NDVI')$ # keep only NDVI
  map(function(img) { # group by the date
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


## Define RGB visualization parameters.
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

## Create RGB visualization images for use as animation frames.
rgbVis <- comp$map(function(img) {
  do.call(img$visualize, visParams) %>%
    ee$Image$clip(mask)
})

## Define GIF visualization parameters.
gifParams <- list(
  region = region,
  dimensions = 600,
  crs = 'EPSG:3857',
  framesPerSecond = 10
)

## Get month names
dates_modis_mabbr <- distinctDOY %>%
  ee_get_date_ic %>% # Get Image Collection dates
  '[['("time_start") %>% # Select time_start column
  lubridate::month() %>% # Get the month component of the datetime
  '['(month.abb, .) # subset around month abbreviations
# Use ee_utils_gif_* functions to render the GIF animation and add some texts.

## Create the animation
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



## Reduce over the whole area to a single number
regionalNDVI <- col$first()$
  reduceRegion(
  reducer = ee$Reducer$mean(),
  geometry = isl_ee,
  scale = 30
)

# Example 4.  SRTM ----------------------------------------------------------------------------------------------------------------
srtm <- ee$Image("USGS/SRTMGL1_003")

Map$addLayer(
  eeObject = srtm,
  visParams =  vizParams_SRTM,
  name = 'SRTM'
)





# Example 5. EVI from landsat for image classification ------------------------------------------------------
evi <- ee$ImageCollection('LANDSAT/LC08/C01/T1_8DAY_EVI')$
  filterDate('2017-01-01', '2017-12-31')$
  select('EVI')$
  toBands()  # Convert  ImageCollection to a multi-band Image and display the band names.

## Explore the object
class(evi)
ee_print(evi) # get metadata

## View the map
Map$centerObject(isl_ee, 9)
Map$addLayer(isl_ee)


# Select one date and display its EVI in a map window with this visualization:
evi02jul <- evi$select("20170704_EVI")
Map$addLayer(evi02jul, vizParams_EVI, 'Landsat 8 EVI 02-July-2017')


## Turn into a local raster in the environment
evi.r <- ee_as_raster(evi, 
                      region = isl_ee,
                      via    = "drive",
                      scale  = 1000)

## Explore
class(evi.r) # raster stack

## Turn into a terra object
evi.r <- rast(evi.r)

## Explore the terra object
class(evi.r)
summary(evi.r)
plot(evi.r)
names(evi.r)


## Find homogenous zones with a clustering method
# here we use k-means
k5 <- kmeans(as.vector(evi.r["20170210_EVI"]), centers = 5, nstart = 5)
print(k5)
print(k5$centers)

## Visualise the clusters
k5.rast <- rast(evi.r[[1]]) # create a new raster the same properties as the evi raster
values(k5.rast) <- k5$cluster # overwrite the values with the K means cluster
plot(k5.rast) # plot the clusters

## Repeat the clustering using multiple images

## Select the images
names(evi.r)
evi.r.3 <- evi.r[[26:31]] # select the images
summary(evi.r.3)

## Run the clustering
k5 <- kmeans(as.matrix(evi.r.3), centers = 5, nstart = 5)

## Check the clustering
print(k5$centers) # check the clusters for each image
k5$size

## Calculate summary metrics to analyse the accuracy
k5$betweenss
k5$tot.withinss
k5$betweenss/k5$totss # The proportion of variance explained

## View the clustering
k5.rast <- rast(evi.r[[1]])
values(k5.rast) <- k5$cluster
plot(k5.rast)
















