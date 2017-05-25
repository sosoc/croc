
#' @importFrom raster raster extent 
.defaultgrid <- function(sensor = c("MODISA", "SeaWiFS")) {
  dims <- c(4320L, 720L)
  sensor <- match.arg(sensor)
  if(sensor == "SeaWiFS") dims <- dims/2L
  raster::setValues(raster(extent(-180, 180, -90, -30), nrows = dims[2L], ncols = dims[1L], crs = "+proj=longlat +datum=WGS84"), 
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
##' @importFrom  raster isLonLat
##' @export
lonlatFromCell <- function(object, cell = NULL, spatial = FALSE) {
  if (is.null(cell)) cell <- seq(raster::ncell(object))
  if(is.na(projection(object)) || raster::isLonLat(object)) {
    raster::xyFromCell(object, cell, spatial = spatial)
  } else {
    p <- spTransform(raster::xyFromCell(object, cell, spatial=TRUE),
                     sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
    if(spatial) p else sp::coordinates(p)
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
