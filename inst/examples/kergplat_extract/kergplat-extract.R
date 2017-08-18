# Could you please extract average (and SD) chla- for the following calendar years around the Kerguelen Plateau.
# fish_yrs<-c(2002, 2006,2010,2013,2016)
# The extent and resolution of the grid I would like to work from is:
#   class       : RasterLayer 
# dimensions  : 115, 230, 26450  (nrow, ncol, ncell)
# resolution  : 0.1, 0.1  (x, y)
# extent      : 61.05, 84.05, -55.95, -44.45  (xmin, xmax, ymin, ymax)
bin_chl <- function(bins, value, platform = "MODISA", ex = NULL) {
  bins <- tibble(bin_num = bins, value = value)
  if (!platform == "MODISA") stop("only MODISA  platform currently supported")
  gridmap <- raster(extent(-180, 180, -90, 0), ncol = 8640, nrow = 4320, crs = "+init=epsg:4326")
  if (!is.null(ex)) gridmap <- crop(gridmap, ex, snap = "out")
  ll <- coordinates(gridmap)
  bins <- tibble(bin_num = lonlat2bin(ll[,1], ll[, 2], NUMROWS = 4320), gridcell = seq(ncell(gridmap))) %>% inner_join(bins, "bin_num")
  gridmap[bins$gridcell] <- bins$value
  trim(gridmap)
}

library(raadtools)
library(roc)
library(tibble)  ## better than data.frame, do it
library(dplyr)
xlim <- c(61.05, 84.05)
ylim <- c(-55.95, -44.45 )
NROWS <- 4320
domain_raster <- raster(extent(xlim, ylim), crs = "+init=epsg:4326" )
init <- initbin(NUMROWS = NROWS)
rowbins <- seq(init$basebin[findInterval(ylim[1], init$latbin)], 
               init$basebin[findInterval(ylim[2], init$latbin) + 1])
xybin <- as_tibble(bin2lonlat(rowbins,
                              nrows = NROWS)) %>%
  dplyr::mutate(bin_num = rowbins) %>% 
  dplyr::filter(x >= xlim[1], x <= xlim[2])

bins0 <- dplyr::select(xybin, "bin_num")


read1 <- function(ifile) {readRDS(ifile) %>% inner_join(bins0, "bin_num")}

for (YEAR in c("2002", "2006", "2010", "2013", "2016")) {
  files <- chla_johnsonfiles() %>% dplyr::filter(format(date, "%Y") == YEAR)
print(range(files$date))
  dd <- purrr::map(files$fullname, read1) %>% dplyr::bind_rows()
  chl <- dd %>%  group_by(bin_num) %>% 
  summarize(chl = mean(chla_johnson, na.rm = TRUE), chlsd = sd(chla_johnson, na.rm = TRUE), nn = sum(!is.na(chla_johnson)))

  chlmap <- bin_chl(chl$bin_num, chl$chl, ex = extent(xlim, ylim))
  sdmap <- bin_chl(chl$bin_num, chl$chlsd, ex = extent(xlim, ylim))
  nnmap <- bin_chl(chl$bin_num, chl$nn, ex = extent(xlim, ylim))

  writeRaster(brick(list(mean = chlmap, sd = sdmap, nn = nnmap)), sprintf("kerg_chl_%s.tif", YEAR))

}



bin_chl2 <- function(bins, value, ex = NULL) {
  bins <- tibble(bin_num = bins, value = value)
  gridmap <- raster(extent(-180, 180, -90, 0), ncol = 8640/2, nrow = 4320/2, crs = "+init=epsg:4326")
  if (!is.null(ex)) gridmap <- crop(gridmap, ex, snap = "out")
  ll <- coordinates(gridmap)
  bins <- tibble(bin_num = lonlat2bin(ll[,1], ll[, 2], NUMROWS = 4320/2), gridcell = seq(ncell(gridmap))) %>% inner_join(bins, "bin_num")
  gridmap[bins$gridcell] <- bins$value
  trim(gridmap)
}

xlim <- c(61.05, 84.05)
ylim <- c(-55.95, -44.45 )
NROWS <- 2160
domain_raster <- raster(extent(xlim, ylim), crs = "+init=epsg:4326" )
init <- initbin(NUMROWS = NROWS)
rowbins <- seq(init$basebin[findInterval(ylim[1], init$latbin)], 
               init$basebin[findInterval(ylim[2], init$latbin) + 1])
xybin <- as_tibble(bin2lonlat(rowbins,
                              nrows = NROWS)) %>%
  dplyr::mutate(bin_num = rowbins) %>% 
  dplyr::filter(x >= xlim[1], x <= xlim[2])

bins0 <- dplyr::select(xybin, "bin_num")


read1 <- function(ifile) {readRDS(ifile) %>% inner_join(bins0, "bin_num")}

YEAR <- "2002"
files <- chla_johnsonfiles(product = "SeaWiFS") %>% dplyr::filter(format(date, "%Y") %in% YEAR)
print(range(files$date))
dd <- purrr::map(files$fullname, read1) %>% dplyr::bind_rows()
chl <- dd %>%  group_by(bin_num) %>% 
  summarize(chl = mean(chla_johnson, na.rm = TRUE), chlsd = sd(chla_johnson, na.rm = TRUE), nn = sum(!is.na(chla_johnson)))

chlmap <- bin_chl2(chl$bin_num, chl$chl, ex = extent(xlim, ylim))
sdmap <- bin_chl2(chl$bin_num, chl$chlsd, ex = extent(xlim, ylim))
nnmap <- bin_chl2(chl$bin_num, chl$nn, ex = extent(xlim, ylim))

writeRaster(brick(list(mean = chlmap, sd = sdmap, nn = nnmap)), sprintf("kerg_chl_SeaWiFS_%s.tif", YEAR))
