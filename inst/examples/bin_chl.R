library(dplyr)
library(raster)
library(roc)
library(palr)

xlim <- c(55, 100)
ylim <- c(-70, -45)
gridmap <- raster(extent(-180, 180, -90, 0), ncol = 8640, nrow = 2160, crs = "+init=epsg:4326")

d <- readRDS("/mnt/k-axis-chlorophyll/k-axis_modis_chla2.rds")

d1 <- as.POSIXct("2016-01-18",tz="GMT") # yday 18
d2 <- as.POSIXct("2016-02-18",tz="GMT") # yday 49

ss <- d %>% filter(date >= d1 & date <= d2) %>% group_by(bin_num) %>% 
  summarize(chl = mean(chla_johnson),n=n(),mx = max(chla_johnson)) # voyage period

## not this way
##tmp <- bin2lonlat(ss$bin_num,4320)
## Rbig <- rasterize(cbind(tmp$x,tmp$y),gridmap,ss$chl,fun=mean,na.rm=T) 
## Rsm<- crop(Rbig,extent(xlim, ylim))

#' bin the L3bins into the standard MODIS grid
#' and crop to the local extent
bin_chl <- function(bins, value, platform = "MODISA") {
  bins <- tibble(bin_num = bins, value = value)
  if (!platform == "MODISA") stop("only MODISA  platform currently supported")
  gridmap <- raster(extent(-180, 180, -90, 0), ncol = 8640, nrow = 4320, crs = "+init=epsg:4326")
  ll <- coordinates(gridmap)
  bins <- tibble(bin_num = lonlat2bin(ll[,1], ll[, 2], NUMROWS = 4320), gridcell = seq(ncell(gridmap))) %>% inner_join(bins, "bin_num")
  gridmap[bins$gridcell] <- bins$value
  trim(gridmap)
}

Rsm <- with(ss, bin_chl(bin_num, chl))
plot(extent(Rsm))
plot(log(Rsm),add=T,col=chlPal(56))

