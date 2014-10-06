## readL3.R
#' @export
readL3 <- function(x, vname, bins = TRUE) {
  if (length(x) > 1L) warning("only first file considered")
  x <- iz2(x[1])
  vdatalist <- vdatainfo(x)
  if (missing(vname)) {
    vdatalist <- vdatalist[!names(vdatalist) %in% c("SEAGrid", "BinList", "BinIndex")]
  } else {
    vdatalist <- vdatalist[names(vdatalist) == vname]
  }
  
  bl <- binlist(x, names(vdatalist), bins = bins)
  bl$filename <- x
  bl
}


iz2 <- function(x) {
  needsdecompress <- grepl(".bz2$", x) 
  if (needsdecompress) {
    f <- gsub(".bz2$", "", x)
    if (file.exists(x)) {
      message(sprintf("Attempting to decompress:\n %s", x))
      system(sprintf("bunzip2 %s", x))
      ##print("bunzip2")
      return(f)
    } else {
      if (file.exists(f)) return(f)
      stop(sprintf("no file %s:", x))
    }
  }
  ## return without change
  x
}


# chl.pal <- raadtools::chl.pal
# 
# plotbin <- function(x, pal = chl.pal) {
#   ll <- bin2lonlat(x$bin_num)
#   function(x, xlim, ylim, ...) {
#     asub <- ll[,1] >= xlim[1] & ll[,1] <= xlim[2] & ll[,2] >= ylim[1] & ll[,2] <= ylim[2]
#      plot(ll[asub, ], col = pal(), ...)
#   }
# }

## supported sensors
.sensornames <- function() {
  c("SeaWiFS", "MODISA")
}
.sensor <- function(x) {
  nms <- names(x)
}


#' Estimate chlorophyll-a from NASA ocean colour. 
#' 
#' Estimate chlorophyll-a from Remote Sensing Reflectance wavelengths. Use SeaWiFS or MODISA with original or Johnson algorithm
#' for the Southern Ocean. 
#' @examples
#' \dontrun{
#' f <- "S1998001.L3b_DAY_RRS.main"
#' x <- readL3(f)
#' asub <- x$bin_num < initlist()$totbins / 2
#' ll <- bin2lonlat(x$bin_num[asub])
#' sw <- swchl(x)[asub]
#' js <- swchl(x, johnson = TRUE)[asub]
#' par(mfrow = c(2,1))
#' plot(ll, col = raadtools::chl.pal(sw), pch = ".")
#' plot(ll, col = raadtools::chl.pal(js), pch = ".")
#' 
#' ## setup a polar raster
#' require(raster)
#' require(rgdal)
#' prj <- "+proj=laea +lat_0=-90 +lon_0=147 +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
#' p <- project(cbind(ll[[1]], ll[[2]]), prj)
#' r <- raster(extent(p), crs = prj, res = c(72000, 72000))
#' sr <- rasterize(p, r, field = sw, fun = mean)
#' jr <- rasterize(p, r, field = js, fun = mean)
#' plot(brick(sr, jr), col = pal$cols, breaks = pal$breaks, legend = FALSE)
#' }
#' @param x list object with Remote Sensing Reflectance wavelengths (see Details)
#' @param sensor which satellite sensor
#' @param johnson use the Johnson et al. (2013) algorithm (FALSE by default)
#' @export
##http://onlinelibrary.wiley.com/doi/10.1002/jgrc.20270/abstract
chla <- function(x, 
                    sensor = c("SeaWiFS", "MODISA"), 
                    algo = c("oceancolor", "johnson")) {
  sensor <- match.arg(sensor)
  algo <- match.arg(algo)
  if (sensor == "MODISA") {
    if (algo == "oceancolor") {
      ocr <-  log10(pmax(
        (x$Rrs_443_sum / x$Rrs_547_sum), 
        (x$Rrs_490_sum / x$Rrs_547)
      ))
    } else {
      ocr <-  log10(pmax(
        (x$Rrs_443_sum / x$Rrs_555_sum), 
        (x$Rrs_490_sum / x$Rrs_555)
      )) 
    }
    if (algo == "johnson") {
      val <- 10 ^ (0.6994 - 2.0384 * ocr - 0.4656 * ocr^2 + 0.4337 * ocr^3)
    }
    if (algo == "oceancolor") {
      val <- 10 ^ (0.2424 - 2.7423 * ocr + 1.8017 * ocr^2 + 0.0015 * ocr^3 - 1.2280 * ocr^4)
    }
  }
  if (sensor == "SeaWiFS") {
   ocr <- log10(pmax(
      (x$Rrs_443_sum / x$Rrs_555_sum), 
      (x$Rrs_490_sum / x$Rrs_555_sum), 
      (x$Rrs_510_sum / x$Rrs_555_sum)))
   if (algo == "johnson") {
     val <- (10 ^ (0.6736 - 2.0714 * ocr - 0.4939* ocr^2 + 0.4756 * ocr^3))
   }
   if (algo == "oceancolor") {
     val <- (10 ^ (0.3272 - 2.9940 * ocr + 2.7218 * ocr^2 - 1.2259 * ocr^3 - 0.5683 * ocr^4))
   }
  }
  val
}



# chla <- function(x, johnson = FALSE) {
#   ## need cascade of checks here, for Rrs values etc. 
#   sensor <- "SeaWiFS"
#   if ("Rrs_547" %in% names(x)) sensor <- "MODISA"
#   






#' Bin map
#' 
#' mapping between bins and a given raster
#' @export
binmap <- function(bin, ras, init = NULL) {
  if (is.null(init)) init <- initbin()
  ## TODO do this smarter
  ll <- SpatialPoints(do.call(cbind, bin2lonlat(bin)), proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  
  extract(ras, ll, cellnumbers = TRUE)[,"cells"]
}


#' Initialize values for a particular binning
#' 
#' Set up the basic values for the bin scheme for given number of rows. 
#' @export 
initbin <- function(NUMROWS = 2160) {
## TODO options for lon-lat sub-sets
  latbin <- (((seq(NUMROWS) - 1) + 0.5) * 180 / NUMROWS ) - 90
  numbin <- as.integer(2 * NUMROWS * cos(latbin * pi/180) + 0.5)
  basebin <- cumsum(c(1L, numbin[-length(numbin)]))
  totbins = basebin[NUMROWS] + numbin[NUMROWS] - 1
  list(latbin = latbin, numbin = numbin, basebin = basebin, totbins = totbins)
}
# 
# bin2lonlat <- function(bin) {
#   row = NUMROWS - 1;
#   fint <- findInterval(bin, basebin)
#   clat = latbin[fint];
#   clon = 360.0*(bin - basebin[fint] + 0.5)/numbin[fint] - 180.0;
#   cbind(clon, clat)
# }
#' @export
bin2bounds <- function(bin, NUMROWS = 2160) {
  row = NUMROWS - 1;
  latbin <- (((seq(NUMROWS) - 1) + 0.5) * 180 / NUMROWS ) - 90
  numbin <- as.integer(2 * NUMROWS * cos(latbin * pi/180) + 0.5)
  basebin <- cumsum(c(1L, numbin[-length(numbin)]))
  fint <- findInterval(bin, basebin)
  north <- latbin[fint] + 90.0/NUMROWS
  south <- latbin[fint] - 90.0/NUMROWS
    ##*north = latbin[row] + 90.0/NUMROWS;
    ##*south = latbin[row] - 90.0/NUMROWS;
    lon = 360.0*(bin - basebin[fint] + 0.5)/numbin[fint] - 180.0;
    west = lon - 180.0/numbin[fint];
    east = lon + 180.0/numbin[fint];
list(east = east, south = south, west =   west, north = north)
  }


