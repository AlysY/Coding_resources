library(raster)
library(terra)

img_dir <- "~/Downloads"

sent2 <- terra::rast("TiwiSentinel.tif")
terra::plotRGB(sent2, r = 4, g = 3, b = 2 , stretch = 'lin')
sent2
print(sent2)

plot(sent2[[4]], main = "Red", col = gray(0:100 / 100))
plot(sent2[[3]], main = "Green", col = gray(0:100 / 100))
plot(sent2[[2]], main = "Blue", col = gray(0:100 / 100))

B4 <- terra::rescale( x = sent2[[4]], 0.02)
B3 <- terra::rescale( x = sent2[[3]], 0.02)
B2 <- terra::rescale( x = sent2[[2]], 0.02)


plot(B4, main = "Red", col = gray(0:100 / 100))
plot(B3, main = "Green", col = gray(0:100 / 100))
plot(B2, main = "Blue", col = gray(0:100 / 100))

sentRGB <- c(B4, B3, B2)
terra::plotRGB(sentRGB, stretch = 'lin')
