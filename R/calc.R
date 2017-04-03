
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
#' @param algo algorithm to use, oceancolor or Johnson et al. (2013) 
#' @param quiet logical to emit warnings or not
#' @param ... not used
#' @export
##http://onlinelibrary.wiley.com/doi/10.1002/jgrc.20270/abstract
chla <- function(x, 
                 sensor, 
                 algo = c("oceancolor", "johnson"), quiet = TRUE, ...) {
  # http://oceancolor.gsfc.nasa.gov/REPROCESSING/R2009/ocv6/
  #   * Rrs1 = blue wavelength Rrs (e.g., 443, 490, or 510-nm)
  #   * Rrs2 = green wavelength Rrs (e.g., 547, 555, or 565-nm)
  #   * X = log10(Rrs1 / Rrs2)
  #   * chlor_a = 10^(a0 + a1*X + a2*X^2 + a3*X^3 + a4*X^4
  #                     
  ## OC4  SeaWiFS  Y  443>489>510  555	0.3272	-2.9940	2.7218	-1.2259	-0.5683
  ## OC3M-547  MODIS	Y	443>489	547	0.2424	-2.7423	1.8017	0.0015	-1.2280
  if (missing(sensor)) sensor <- .filesensor(basename(x$filename))
  algo <- match.arg(algo)
  params <- list(johnson = list(MODISA = c(0.6994, -2.0384, -0.4656, 0.4337, 0), 
                                SeaWiFS = c(0.6736, -2.0714, -0.4939, 0.4756, 0)),   
                 oceancolor = list(MODISA = c(0.2424, -2.7423, 1.8017, 0.0015, -1.2280), 
                                   SeaWiFS = c(0.3272, -2.9940, 2.7218, -1.2259, -0.5683)))
  p0 <- params[[algo]][[sensor]]
#   form<- function(X, a) {
#     10^(a[1L] + a[2L]*X + a[3L]*X^2 + a[4L]*X^3 + a[5L]*X^4)
#   }
#   
  form <- function(X, a) {
    10^(a[1] + X*(a[2]+X*(a[3]+ X*(a[4] + X*a[5]))))
    
  }
  ## rarely, some wavelengths are less than 0 one example is
  ##   a bin at index 1569 in something like the (15 * 28)th day in the MODISA - 
  ##   Rrs_555_sum is < 0 so log is bung)
  ocr <- switch(sensor, 
                SeaWiFS = pmax(x$Rrs_443_sum, x$Rrs_490_sum, x$Rrs_510_sum)/x$Rrs_555_sum, 
                MODISA = switch(algo, 
                                oceancolor = pmax(x$Rrs_443_sum, x$Rrs_488_sum)/x$Rrs_547_sum, 
                                johnson =    pmax(x$Rrs_443_sum, x$Rrs_488_sum)/x$Rrs_555_sum))

  minmaxrat <- c(0.21, 30)
  bad2 <- ocr < minmaxrat[1L]
  if (any(bad2) & !quiet) warning("some band ratios less than minimum (%f", minmaxrat[1L])
  bad3 <- ocr > minmaxrat[2L]
  if (any(bad3) & !quiet) warning("some band ratios greater than maximum (%f", minmaxrat[2L])
  ocr[!(bad2 | bad3)] <- log10(ocr[!(bad2 | bad3)])
  out <- form(ocr, p0)
  out[bad2 | bad3] <- 0
  out
}



# chla <- function(x, johnson = FALSE) {
#   ## need cascade of checks here, for Rrs values etc. 
#   sensor <- "SeaWiFS"
#   if ("Rrs_547" %in% names(x)) sensor <- "MODISA"
#   
