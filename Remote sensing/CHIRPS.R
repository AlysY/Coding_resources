## CHRIPS
## from https://cran.r-project.org/web/packages/chirps/vignettes/Overview.html


library("chirps")
library("sf")

data("tapajos", package = "chirps")

# sample three points within the Tapajos area
set.seed(1234)
tp_point <- st_sample(tapajos, 3)

# coerce as sf points
tp_point <- st_as_sf(tp_point)

dat <- get_chirps(tp_point, # returns a dataframe
                  dates = c("2013-01-01","2018-12-31"), 
                  server = "ClimateSERV")
dat
dat_sf <- get_chirps(tp_point,
                     dates = c("2017-12-15","2017-12-31"),
                     server = "ClimateSERV",
                     as.sf = TRUE)
# precip_indices()
# assess how the precipitation changes across a time series using precipitation variability indices
p_ind <- precip_indices(dat,
                        timeseries = TRUE,
                        intervals = 15) # 15 days

p_ind