## problems

##byte projection ;
##projection:proj4 = "+lonc=11.000000 +y_0=0 +proj=omerc +x_0=0 +units=m +alpha=2.000000 +k=1 +gamma=31.000000 +lat_0=2.300000" ;


library(raadtools)

f <- "/rdsi/PRIVATE/data/geoservices.knmi.nl/viirsdemo/viirs_cmd_14983_merc.nc"        
library(raster)
prj <- "+proj=omerc +y_0=0 +x_0=0 +k=1 +units=m +alpha=2.400000 +gamma=26.300000 +lonc=-15.000000 +lat_0=81.000000"

prj <- "+proj=omerc +x_0=0 +lonc=11.000000 +y_0=0 +units=m +alpha=2.000000 +k=1 +gamma=31.000000 +lat_0=2.300000" ;


r <- flip(raster(f, varname = "truecolor"), direction = "y")

# r3 <- function(x) {
#      b <- bitShiftR(x, 24)
#      g <- bitShiftR(x - b * 256^3, 16)
#      r <- x - g * 256^2 - b * 256^3
#      cbind(r, g, b)
#    }



col24ToRGBA <- function(col24) {
  alpha <- col24 %/% 2^32
  col24 <- col24 -  ifelse(alpha == 0, 255, alpha) * 2^32
  blue <- (col24 %/% 2^24) + 255
  col24 <- col24 - blue * 2^24 
  green <- col24  %/% 2^16 + 255 
  red <- (col24 - green * 2^16) 
  cbind(red, green, blue, alpha)
}
#x <- integer64(4) ## c(69, 5, 163, 155)
xdat <- c(250, 100, 90, 240)
rgbaToCol <- function(num) sum(num * 256 ^ (seq(length(num)) - 1))



col24ToRGBA (rgbaToCol(xdat))

unpackColor <- function(num) {
  a = floor((num / 256 / 256 / 256)/256);
  b = floor((num - a * 256.0 * 256.0 * 256) /256);
  g = floor((num - a * 256.0 * 256.0 * 256 - b * 256  *  256.0)/256);
  r = floor((num - a * 256.0 * 256.0 * 256 - b * 256  *  256.0 - g * 256)/256);
  cbind(r, g, b, a)
}

unpackColor(rgbaToCol(xdat))


e <-new("Extent"
        , xmin = 3693574.21174868
        , xmax = 3847596.19822783
        , ymin = 5719282.63935744
        , ymax = 6519216.79666081
)

r2 <- crop(flip(r, direction = "y"), e)
v <- values(r2)

m <- col24ToRGBA(v)

b <- brick(setValues(r2, m[,1]), setValues(r2, m[,2]), setValues(r2, m[,3]))

plotRGB(b/256)
