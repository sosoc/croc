## baseline grid
library(raadtools)
library(rgdal)
ex <- extent(110, 230, -80, -30)
midpoint <- round( apply(coordinates(as(ex, "SpatialPoints")), 2, mean), digits = 0)
prj <- sprintf("+proj=laea +lon_0=%i +lat_0=%i", midpoint[1], midpoint[2])
basegrid <- projectExtent(raster(ex, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"), prj)
res(basegrid) <- c(15e3, 15e3)

## context
wmap <- spTransform(intersect(coastmap("world2"), ex + 15), CRS(prj))

daterange <- as.POSIXct(c(as.Date("2015-03-01"), as.Date("2015-03-31")), tz = "UTC")

## xylimit in longlat, MODISA, CHL_RJ for Johnson 2013
## basegrid to bin into desired space
chl <- readoc(daterange, xylim = ex, grid = basegrid, platform = "MODISA")
pal <- chl.pal(palette = TRUE)
#plot(chl, col = pal$cols, breaks = pal$breaks, legend = FALSE)
 
ice <- projectRaster(readice(mean(daterange), setNA = FALSE, time.resolution = "monthly"), chl)

plot(extent(chl), type = "n", xlab = "", ylab = "", axes = FALSE, asp = 1) 
plot(ice, add = TRUE, col = grey(seq(0, 0.8, length = 100)), breaks = 1:99, legend = FALSE)
contour(ice, add = TRUE, lev = 15, col = "red", lwd = 2)
plot(chl, col = pal$cols, breaks = pal$breaks, legend = FALSE, add = TRUE)

pp <- as(aggregate(crop(chl, extent(chl) - 3e6), 12), "SpatialPoints")

llgridlines(pp, col = "grey20", ndiscr = 50, side = "EN", easts = c(120, 180, -120))

