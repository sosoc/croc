library(tidyverse)

files  <- tibble(fullname = list.files("/tmp/roc", full.names = TRUE, pattern = "nc$"))

#devtools::install_github("r-gris/ncdump")
#nctab <- ncdump::NetCDF(fs[1])
library(raster)
files <- files %>% mutate(ncell = NA_integer_, ncount = NA_integer_, xmin = NA_integer_, xmax = NA_integer_, ymin = NA_integer_, ymax = NA_integer_)
for (i in seq_len(nrow(files))) {
  b <- brick(files$fullname[i], varname = "geophysical_data/Rrs_443")
  files$ncell[i] <- ncell(b)
  files$ncount[i] <- sum(!is.na(values(b[[1]])))
  lonlat <- brick(lapply(c("navigation_data/longitude", "navigation_data/latitude"), function(x) brick(files$fullname[i], varname = x)))
  lonrange <- range(values(lonlat[[1]]), na.rm = TRUE)
  latrange <- range(values(lonlat[[2]]), na.rm = TRUE)
  files$xmin <- lonrange[1]
  files$xmax <- lonrange[2]
  files$ymin <- latrange[1]
  files$ymax <- latrange[2]
  
  print(i)
}
plot(files$ncell, ylim = c(0, max(files$ncell)))
lines(files$ncount)
plot(c(-180, 180), c(-90, 90))
rect(files$xmin, files$ymin, files$xmax, files$ymax)
## extract MODIS/johnson rrs
varnames <- file.path("geophysical_data", c("Rrs_443",  "Rrs_488", "Rrs_555"))
rrs <- brick(lapply(varnames, function(x) brick(files$fullname[111], varname = x)))
rrs <- setNames(rrs, sprintf("%s_sum", basename(varnames)))
## get the coords
lonlat <- brick(lapply(c("navigation_data/longitude", "navigation_data/latitude"), function(x) brick(files$fullname[111], varname = x)))
d <- as_tibble(as.data.frame(rrs)) %>% mutate(cell = row_number())

d <- d %>% filter(!is.na(Rrs_443_sum))  
## calculate
d$chl <- chla(as.list(d), "MODISA", algo = "johnson")

d <- d %>% filter(chl > 0)
xy <- raster::extract(lonlat, d$cell)
plot(xy, col = palr::chlPal(d$chl), pch = "." )




d$long <- xy[, 1]
d$lat <- xy[, 2]
library(ggplot2)
ggplot(d, aes(long, lat, colour = chl)) + geom_point(pch = ".")
