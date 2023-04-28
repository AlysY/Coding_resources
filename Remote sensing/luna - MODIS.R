## Luna: the rspatial package for remote sensing


library(luna)

library(terra)
library(luna)
# lists all products that are currently searchable
prod <- getProducts()
head(prod)
##      provider             concept_id                          short_name
## 1        GHRC             C1000-GHRC                            dc8capac
## 5       CDDIS      C1000000000-CDDIS              CDDIS_DORIS_data_cycle
## 18 LANCEAMSR2 C1000000000-LANCEAMSR2                      A2_RainOcn_NRT
## 19  NSIDC_ECS  C1000000000-NSIDC_ECS                            NmHRIR3H
## 23  ORNL_DAAC  C1000000000-ORNL_DAAC GLOBAL_MICROBIAL_BIOMASS_C_N_P_1264
## 25      SEDAC      C1000000000-SEDAC               CIESIN_SEDAC_EPI_2012
##    version
## 1        1
## 5        1
## 18       0
## 19       1
## 23       1
## 25 2012.00
# to find the MODIS products
modis <- getProducts("^MOD|^MYD|^MCD")
head(modis)
##        provider             concept_id short_name version
## 806  LPDAAC_ECS C1000000120-LPDAAC_ECS     MOD44B     051
## 1433 LPDAAC_ECS C1000000400-LPDAAC_ECS   MCD43D10     006
## 1447 LPDAAC_ECS C1000000401-LPDAAC_ECS   MCD43D33     006
## 1452 LPDAAC_ECS C1000000402-LPDAAC_ECS   MCD43D45     006
## 1454 LPDAAC_ECS C1000000403-LPDAAC_ECS   MCD43D26     006
## 1456 LPDAAC_ECS C1000000404-LPDAAC_ECS   MCD43D49     006

product <- "MOD09A1"
productInfo(product) ## launches web page


## define parameters
# product name, start and end date, and area of interest.

start <- "2010-01-01"
end <- "2010-01-07"

# Location is Marsabit, Kenya
ken <- geodata::gadm(country = "Kenya", level=1, path=".")
ken
##  class       : SpatVector
##  geometry    : polygons
##  dimensions  : 47, 11  (geometries, attributes)
##  extent      : 33.90959, 41.92622, -4.720417, 5.061166  (xmin, xmax, ymin, ymax)
##  coord. ref. : lon/lat WGS 84 (EPSG:4326)
##  names       :   GID_1 GID_0 COUNTRY  NAME_1 VARNAME_1 NL_NAME_1 TYPE_1
##  type        :   <chr> <chr>   <chr>   <chr>     <chr>     <chr>  <chr>
##  values      : KEN.1_1   KEN   Kenya Baringo        NA        NA County
##                KEN.2_1   KEN   Kenya   Bomet        NA        NA County
##                KEN.3_1   KEN   Kenya Bungoma        NA        NA County
##  ENGTYPE_1  CC_1 HASC_1 ISO_1
##      <chr> <chr>  <chr> <chr>
##     County    30  KE.BA KE-01
##     County    36  KE.BO KE-02
##     County    39  KE.BN KE-03

pol <- ken[ken$NAME_1 == "Marsabit", ]

# check on the map
plot(ken,  col = "light gray")
lines(pol, col = "red", lwd = 2)

## Search the NASA server for MODIS data
## download one tile
mf <- luna::getModis(product, start, end, aoi = pol, download = FALSE)
mf
## [1] "MOD09A1.A2009361.h21v08.061.2021149144347.hdf"
## [2] "MOD09A1.A2010001.h21v08.061.2021150093948.hdf"
## [3] "MOD09A1.A2009361.h21v08.006.2015198070255.hdf"
## [4] "MOD09A1.A2010001.h21v08.006.2015198080805.hdf"


## use the terra package to visusalise

## turn into a spat raster
datadir <- file.path(dirname(tempdir()), "_modis")
mf <- file.path(datadir, "MOD09A1.A2009361.h21v08.006.2015198070255.hdf")
library(terra)
r <- rast(mf[1])
r
## class       : SpatRaster
## dimensions  : 2400, 2400, 13  (nrow, ncol, nlyr)
## resolution  : 463.3127, 463.3127  (x, y)
## extent      : 3335852, 4447802, 0, 1111951  (xmin, xmax, ymin, ymax)
## coord. ref. : +proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs
## sources     : MOD09A1.A2009361.h21v08.006.2015198070255.hdf:MOD_Grid_500m_Surface_Reflectance:sur_refl_b01
##               MOD09A1.A2009361.h21v08.006.2015198070255.hdf:MOD_Grid_500m_Surface_Reflectance:sur_refl_b02
##               MOD09A1.A2009361.h21v08.006.2015198070255.hdf:MOD_Grid_500m_Surface_Reflectance:sur_refl_b03
##               ... and 10 more source(s)
## varnames    : MOD09A1.A2009361.h21v08.006.2015198070255
##               MOD09A1.A2009361.h21v08.006.2015198070255
##               MOD09A1.A2009361.h21v08.006.2015198070255
##               ...
## names       : sur_refl_b01, sur_refl_b02, sur_refl_b03, sur_refl_b04, sur_refl_b05, sur_refl_b06, ...

## Check the properties to check it makes sense
crs(r)
## [1] "PROJCRS[\"unnamed\",\n    BASEGEOGCRS[\"Unknown datum based upon the custom spheroid\",\n        DATUM[\"Not specified (based on custom spheroid)\",\n            ELLIPSOID[\"Custom spheroid\",6371007.181,0,\n                LENGTHUNIT[\"metre\",1,\n                    ID[\"EPSG\",9001]]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]],\n    CONVERSION[\"unnamed\",\n        METHOD[\"Sinusoidal\"],\n        PARAMETER[\"Longitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"Meter\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"Meter\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"Meter\",1]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"Meter\",1]]]"
res(r)
## [1] 463.3127 463.3127
names(r)
##  [1] "sur_refl_b01"         "sur_refl_b02"         "sur_refl_b03"
##  [4] "sur_refl_b04"         "sur_refl_b05"         "sur_refl_b06"
##  [7] "sur_refl_b07"         "sur_refl_qc_500m"     "sur_refl_szen"
## [10] "sur_refl_vzen"        "sur_refl_raz"         "sur_refl_state_500m"
## [13] "sur_refl_day_of_year"


## Plot
plotRGB(r, r = 1, g = 4, b = 3, stretch="lin"


## QA pixels
from <- c(1,3,11,14)
to   <- c(2,3,11,14)
reject <- c("01,10", "1", "1", "1")
qa_bits <- cbind(from, to, reject)
qa_bits
##      from to   reject
## [1,] "1"  "2"  "01,10"
## [2,] "3"  "3"  "1"
## [3,] "11" "11" "1"
## [4,] "14" "14" "1"

qc <- r[[12]]
plot(qc, main = "Quality")

## luna package has a modis_mask method to create a mask from the quality band and the parameters defined above.

library(luna)
quality_mask <- modis_mask(qc, 16, qa_bits)
plot(quality_mask, main="Quality mask")


# mask
rmask <- mask(r, quality_mask)

## Plot as false colour composite
plotRGB(rmask, r = 2, g = 1, b = 4, main='False color composite', stretch = "lin")




## NDVI

# Change vector boundary coordinate reference system, so that it matches that of the MODIS data.
prj <- crs(rmask)
prj
## [1] "PROJCRS[\"unnamed\",\n    BASEGEOGCRS[\"Unknown datum based upon the custom spheroid\",\n        DATUM[\"Not specified (based on custom spheroid)\",\n            ELLIPSOID[\"Custom spheroid\",6371007.181,0,\n                LENGTHUNIT[\"metre\",1,\n                    ID[\"EPSG\",9001]]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]],\n    CONVERSION[\"Sinusoidal\",\n        METHOD[\"Sinusoidal\"],\n        PARAMETER[\"Longitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]]]"
poly <- project(pol, prj)

# crop to boundaries
rcrop <- crop(rmask, poly)

# plot the boundary
plotRGB(rcrop, r = 2, g = 1, b = 4,  main = "False color composite", stretch = "lin" )
lines(poly, col="blue")

## We expect the reflectance to be between 0 (very dark areas) and 1 (very bright areas). Due to various reasons, there may values slightly outside this rnage.
# First clamp values of the image between 0 and 1.
r <- clamp(rcrop, 0, 1)


ndvi <- (r[[2]] - r[[1]]) /(r[[2]] + r[[1]])
plot(ndvi, main="NDVI")











