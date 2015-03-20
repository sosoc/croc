carbon[j, i] <-  nasum(iceok(exIce[apoly, i]) * areavals[apoly]) *
  ( nasum(areavals[apoly] * expoc[apoly, i] / exKd[apoly, i]) / nasum(areavals[apoly]) +
      nasum(7 * areavals[apoly] * exProd[apoly, i] * 50 / exKd[apoly, i]) / nasum(areavals[apoly]) ) -
  
  nasum(iceok(exIce[apoly, i+1]) * areavals[apoly]) *
  ( nasum(areavals[apoly] * expoc[apoly, i+1] / exKd[apoly, i+1]) / nasum(areavals[apoly]) )


poc / kd / chla

platform <- "MODISA"  ## or SeaWiFS
nr <- c(MODISA = 4320, SeaWiFS = 2160)
## these should all be the same number of rows (days since 2002-07-04) - see range(fpoc$date)
fpoc <- ocfiles(varname = "POC", product = platform)
fkd <- ocfiles(varname = "KD490", product = platform)
frrs <- ocfiles(varname = "RRS", product = platform)


## choose a date
date <- ISOdatetime(2010, 1, 1, 0, 0, 0, tz = "GMT")
i <- which.min(abs(fpoc$date - date))
library(raster)
ext <- extent(0, 100, -80, -50)
bins <- crop_init(initbin(nr[platform]), ext)

rawpoc <- readL3(fpoc$fullname[i], check = FALSE)
rawkd <- readL3(fkd$fullname[i], check = FALSE)
## we need raw wavelengths to calculate our own chlorophyll-a
rrs <- readL3(frrs$fullname[i], check = FALSE)
rrs$chla <- chla(rrs, "MODISA", algo = "johnson")

## poc has a different set of bins, so intersect carefully
commonbins <- intersect(bins, intersect(intersect(rawpoc$bin_num, rawkd$bin_num), rrs$bin_num))

day <- list(bin_num = commonbins, poc = (rawpoc$poc_sum/rawpoc$weights)[match(commonbins, rawpoc$bin_num)], 
            kd = (rawkd$Kd_490_sum/rawkd$weights)[match(commonbins, rawkd$bin_num)], 
            chla = rrs$chla[match(commonbins, rrs$bin_num)]
)

par(mfrow = c(2, 2))
 xy <- bin2lonlat(day$bin_num, nr[platform])
 plot(xy, pch = ".", col = sst.pal(100)[scl(day$poc) * 99 + 1])
 plot(xy, pch = ".", col = sst.pal(100)[scl(day$kd) * 99 + 1])
 plot(xy, pch = ".", col = chl.pal(day$chla))

  