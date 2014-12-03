.defaultgrid <- function(sensor = c("MODISA", "SeaWiFS")) {
  dims <- c(4320L, 720L)
  sensor <- match.arg(sensor)
  if(sensor == "SeaWiFS") dims <- dims/2L
  setValues(raster(extent(-180, 180, -90, -30), nrows = dims[2L], ncols = dims[1L], crs = "+proj=longlat +datum=WGS84"), 
            rep(0, prod(dims)))
}


##' Extract longitude and latitude of raster cells.
##'
##' Extract the longitude and latitude of the center of the requested
##' cells of a Raster* object, similar to \code{xyFromCell}.
##' @title Raster cell longitude and latitudes
##' @param object a raster object
##' @param cell the cell numbers
##' @param spatial return locations as SpatialPoints object instead of a matrix.
##' @return the lon,lat locations for the requested cells.
##' @details  from SGAT
.lonlatFromCell <- function(object, cell = NULL, spatial = FALSE) {
  if (is.null(cell)) cell <- seq(ncell(object))
  if(is.na(projection(object)) || isLonLat(object)) {
    xyFromCell(object, cell, spatial = spatial)
  } else {
    p <- spTransform(xyFromCell(object, cell, spatial=TRUE),
                     CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
    if(spatial) p else coordinates(p)
  }
}

##' Regrid ocean colour. 
##' 
##' Regrid NASA ocean colour RRS values to standard Mapped image with the Johnson chl-a algorithm. 
##' @param tgrid target grid
##' @param file L3 bin file name with raw RRS wavelengths
##' @export
regridder <- function(tgrid, file, agg = FALSE, fun = mean) {
  xy <- .lonlatFromCell(tgrid, spatial = FALSE)
  NROWS <- readL3(file, vname = "NUMROWS", bins = FALSE)$NUMROWS
  binmap <- lonlat2bin(xy[,1], xy[,2], NROWS)
  function(file) {
    x <- readL3(file)
    sens <- if(x$NUMROWS == 4320) "MODISA" else "SeaWiFS"
    if(x$NUMROWS != NROWS)  stop("file doesn't match this regridder\nfile NUMROWS: ", x$NUMROWS, "\nfunction NUMROWS:", NROWS)
   
    if (agg) {
     vals <-  chla(x, sensor = sens, algo = "johnson")
     
     binxy <- do.call(cbind, bin2lonlat(x$bin_num, x$NUMROWS))
     if (!is.na(projection(tgrid)) & !isLonLat(tgrid)) binxy <- project(binxy, projection(tgrid))
     gcell <- extract(tgrid, binxy, cellnumbers = TRUE)[,"cells"]
     tvals <- tapply(vals, gcell, fun)
     
     r <- tgrid
     r[unique(gcell)] <- tvals
     r[!r>0] <- NA
    } else {
      r <- setValues(tgrid, chla(x, sensor = sens, algo = "johnson")[match(binmap, x$bin_num)])
    }
    r
  }
  
  
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
