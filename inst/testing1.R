#  readoc <- function(daterange,  
#                    xylim = NULL, lon180 = TRUE, 
#                    varname = "CHL_RJ", grid = NULL, platform = "MODISA", ...) {
#   daterange <- range(timedateFrom(daterange))
#   ## check daterange, sequence of files
#   files <- subset(ocfiles(), date >= daterange[1] & date <= daterange[2])
#   init <- initbin(4320)
#   nobs <- sum0 <- n0 <- numeric(init$totbins)
#   if (!is.null(xylim)) ibin <- crop_init(init, extent(xylim))
#   
#   for (i in seq(nrow(files))) {
#     x <- readL3(files$fullname[i], check = FALSE)
#     if (!is.null(xylim)) x <- .subsetL3(x, x$bin_num %in% ibin)
#     x$chl0 <- chla(x, sensor = platform, algo = "johnson")
#     sum0[x$bin_num] <- sum0[x$bin_num] + x$chl0
#     n0[x$bin_num] <- n0[x$bin_num] + 1
#     nobs[x$bin_num] <- nobs[x$bin_num] + x$nobs
#     if (i %% 40 == 0) print(i)
#   }
#   bins <- which(n0 > 0)
#   return(list(NUMROWS = x$NUMROWS, bin_num = bins, sum = sum0[bins], n = n0[bins], nobs = nobs[bins]))
#   if (is.null(grid))
#   list(NUMROWS = x$NUMROWS, bin_num = bins, chlor_a = (sum0/n0)[bins]) else grid
#                  
#   }
#    
 library(raadtools)
f <- raadtools:::.allfilelist()                
pal <- chl.pal(palette = TRUE)
                  
brks <- c(0, 10^seq(-2, log10(20), length  = length(pal$cols) - 1), 1000)               
ext <- extent(135, 150, -50, -38);asp <- 1/cos(43 * pi/180)
## calculate 31 days for RJ chlorophyll from L3 bins
x <- readoc(as.Date(c("2010-01-01", "2010-01-31")), xylim = ext)
x$chlor_a <- x$sum/x$n
## download the oceancolor CHL browser image
a <- readL3img("2010-01-01", platform = "MODISA", tres = "1 month")
## read the oceancolor CHL from HDF4
b <- readL3("/rdsi/PRIVATE/data/oceandata.sci.gsfc.nasa.gov/MODISA/L3BIN/2010/001/A20100012010031.L3b_MO_CHL.main", check = FALSE)
b <- .subsetL3(b, b$bin_num %in% x$bin_num)
par(mfrow = c(3, 1), bg = "black")
sf <- 1/3
plot(ext, asp = asp);plotRGB(crop(a, ext), add = TRUE)
col1 <- pal$cols[findInterval(b$chlor_a_sum/b$weights, brks)]
pts1 <- bin2lonlat(b$bin_num, 4320)
plot(ext, asp = asp); points(pts1, col = col1, pch = 16, cex = cos(pts1$y * pi/180) * sf)
col2 <- pal$cols[findInterval(x$chlor_a, brks)]
pts2 <- bin2lonlat(x$bin_num, 4320)
plot(ext, asp = asp); points(pts2, col = chl.pal(x$chlor_a), pch = 16, cex = cos(pts2$y * pi/180) * sf)


library(roc)
library(raadtools)
source("inst/crop.R")
platform <- "SeaWiFS"
smap <- c(MODISA = "A", SeaWiFS = "S")
init <- initbin(c(S = 2160, A = 4320)[smap[platform]])
fdayname <- sprintf("%s2007001.L3b_DAY_CHL.main", smap[platform])
fyearname <- sprintf("%s20070012007365.L3b_YR_CHL.main", smap[platform])
files <- subset(ocfiles(product =  platform), format(date, "%Y") %in% "2007")

chldayfile <- file.path(sprintf("/rdsi/PRIVATE/data/oceandata.sci.gsfc.nasa.gov/%s/L3BIN/2007/001", platform), fdayname)
chlyearfile <- file.path(sprintf("/rdsi/PRIVATE/data/oceandata.sci.gsfc.nasa.gov/%s/L3BIN/2007/001", platform), fyearname)

cdf <- readL3(chldayfile, check = FALSE)
rrsdayfile <- files$fullname[1]
rdf <- readL3(rrsdayfile, check = FALSE)
rdf$chla <- chla(rdf, sensor = platform, algo = "oceancolor")

ext <- extent(135, 150, -50, -38)
ibin <- crop_init(init, ext)

oc1 <- .subsetL3(cdf, cdf$bin_num %in% ibin)
roc1 <- .subsetL3(rdf, rdf$bin_num %in% ibin)
bb <- bin2bounds(oc1$bin_num,oc1$NUMROWS)
names(bb)
plot(ext)
rect(bb$east, bb$south, bb$west, bb$north, col = chl.pal(roc1$chla), border = NA)
rect(bb$east, bb$south, bb$west, bb$north, col = chl.pal(oc1$chlor_a_sum/oc1$weights), border = NA)



cyf <- readL3(chlyearfile, check = FALSE)
init <- initbin(cyf$NUMROWS)
sum0 <- n0 <- numeric(init$totbins)
for (i in seq_along(files$date)) {
  x <- readL3(files$fullname[i], check = FALSE)
  x <- .subsetL3(x, x$bin_num %in% ibin)
  x$chl0 <- chla(x, sensor = platform, algo = "oceancolor")
  sum0[x$bin_num] <- sum0[x$bin_num] + x$chl0
  n0[x$bin_num] <- n0[x$bin_num] + 1
  if (i %% 40 == 0) print(i)
}

bins <- which(n0 > 0)
bb <- bin2bounds(bins, x$NUMROWS)
par(mfrow = c(2, 1), mar = rep(0, 4))
plot(ext)
rect(bb$east, bb$south, bb$west, bb$north, col = chl.pal((sum0/n0)[bins]), border = NA)
cyf2 <- .subsetL3(cyf, cyf$bin_num %in% ibin)
bb2 <- bin2bounds(cyf2$bin_num, cyf2$NUMROWS)
plot(ext)
rect(bb2$east, bb2$south, bb2$west, bb2$north, col = chl.pal((cyf2$chlor_a_sum/cyf2$weights)), border = NA)




$ NUMROWS    : int 2160
$ bin_num    : int [1:4099307] 58806 58807 58808 59655 59656 59657 59658 59659 59660 59661 ...
$ nobs       : int [1:4099307] 9 42 18 4 14 17 20 34 28 37 ...
$ nscenes    : int [1:4099307] 9 32 17 4 13 14 18 24 26 27 ...
$ weights    : num [1:4099307] 9 36 17.4 4 13.4 ...
$ chlor_a_sum: num [1:4099307] 4.18 42.05 9.81 2.36 7.34 ...
$ chlor_a_ssq: num [1:4099307] 2.42 547.6 6.16 1.98 5.38 ...
$ filename   : chr "/rdsi/PRIVATE/data/oceandata.sci.gsfc.nasa.gov/SeaWiFS/L3BIN/2007/001/S20070012007365.L3b_YR_CHL.main"



b <- readL3img("2007-01-01", tres = "1 year")
plotRGB(b)

xy <- bin2lonlat(cyf$bin_num, cyf$NUMROWS)
plot(xy, pch = ".", col =chl.pal(cyf$chlor_a_sum/cyf$weights))


