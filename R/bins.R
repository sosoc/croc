.lat2row <- function(lat, NUMROWS) {
  row <- as.integer((90 + lat) * NUMROWS/180.0)
  row[row >= NUMROWS] <- NUMROWS - 1;
  row + 1
}

##' Generate bin number from longitude latitude. 
##' 
##' Bin number from longitude and latitude for a given grid with NUMROWS unique latitudes. 
##' @param lon longitude
##' @param lat latitude
##' @param NUMROWS number of rows
##' @export
lonlat2bin <- function(lon, lat, NUMROWS) {
  ibin <- initbin(NUMROWS)
  row <- .lat2row(lat, NUMROWS)
  col <- (lon + 180) * ibin$numbin[row] / 360
  ##col[col >= ibin$numbin[row]] <- ibin$numbin[row] - 1
  as.integer(ibin$basebin[row] + col)
}




#' Bin map
#' 
#' mapping between bins and a given raster
#' @param bin bin number
#' @param ras RasterLayer
#' @param init optional initial values for bin structure 
#' @export
#' @importFrom sp SpatialPoints CRS
#' @importFrom raster extract
binmap <- function(bin, ras, init = NULL) {
  if (is.null(init)) init <- initbin()
  ## TODO do this smarter
  ll <- SpatialPoints(do.call(cbind, bin2lonlat(bin)), proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  
  extract(ras, ll, cellnumbers = TRUE)[,"cells"]
}


#' Initialize values for a particular binning
#' 
#' Set up the basic values for the bin scheme for given number of rows. 
#' @param NUMROWS relevant number of L3 bin rows
#' @export 
initbin <- function(NUMROWS = 2160) {
  ## TODO options for lon-lat sub-sets
  latbin <- (((seq(NUMROWS) - 1) + 0.5) * 180 / NUMROWS ) - 90
  ## this will overflow at 2^31-1
  #numbin <- as.integer(2 * NUMROWS * cos(latbin * pi/180) + 0.5)
  numbin <- trunc(2 * NUMROWS * cos(latbin * pi/180) + 0.5)
  basebin <- cumsum(c(1L, numbin[-length(numbin)]))
  totbins = basebin[NUMROWS] + numbin[NUMROWS] - 1
  list(latbin = latbin, numbin = numbin, basebin = basebin, totbins = totbins)
}

#' Calculate bin boundaries from bin number
#' 
#' Calculate bin boundaries from bin number
#' @param bin bin number
#' @param NUMROWS relevant number of L3 bin rows
#' @export
bin2bounds <- function(bin, NUMROWS) {
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


