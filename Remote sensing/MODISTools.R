################
## MODISTools ##
################

# introduction to the MODISTools package
# https://cran.r-project.org/web/packages/MODISTools/vignettes/modistools-vignette.html

library("MODISTools")

## View the products
products <- mt_products()
head(products, n=10)

## For your product, select the band
bands <- mt_bands(product = "MOD13Q1")
head(bands)


