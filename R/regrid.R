.defaultgrid <- function(sensor = c("MODISA", "SeaWiFS")) {
  dims <- c(4320L, 720L)
  sensor <- match.arg(sensor)
  if(sensor == "SeaWiFS") dims <- dims/2L
  raster(extent(-180, 180, -90, -30), nrows = dims[2L], ncols = dims[1L], crs = "+proj=longlat +datum=WGS84")
}

##' Regrid ocean colour. 
##' 
##' Regrid NASA ocean colour RRS values to standard Mapped image. 
##' @param file L3 bin file name with raw RRS wavelengths
##' @export
regrid <- function(file) {
    x <- readL3(file)
    ## raw bin points
    ##xy <- do.call(cbind, bin2lonlat(x$bin_num, x$NUMROWS))
    sens <- if(x$NUMROWS == 4320) "MODISA" else "SeaWiFS"
    rtemp <- .defaultgrid(sensor = sens)
    ## subset to Southern Ocean
    ##asub <- xy[,2] <= ymax(rtemp)
    ##xy <- xy[asub, ]
    
    ## raw grid points
    xy <- coordinates(rtemp)
    ## convert raster to bin 
    binmap <- lonlat2bin(xy[,1], xy[,2], x$NUMROWS)
    setValues(rtemp, chla(x, sensor = sens, algo = "johnson")[match(binmap, x$bin_num)])
}

# start <- 120
# n <- 50
# x <- regrid(oc$fullname[start])
# x[is.na(x)] <- 0
# x <- x/n
# 
# pal <- chl.pal(palette = TRUE)
# pal$cols[1] <- rgb(1, 1, 1)
# for (i in seq(start+1, length = n)) {
#   print(i)
#   x1 <- regrid(oc$fullname[i])
#   x1[is.na(x1)] <- 0
#   x <- x + x1/n
#   
#   plot(x, col = pal$cols, breaks = pal$breaks, legend = FALSE)
# }
# 
