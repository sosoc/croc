#' Standard Mapped Image
#' 
#' SMI is the Standard Mapped Image, a longitude-latitude affine grid
#' at zonally-native resolution. See details. 
#' 
#' SeaWiFS is 2160 rows for 9.2 km resolution
#' MODISA is 4320 rows for 4.6 km resolution (also provided in 9.2km)
#' VIIRS is 4320 rows like MODIS. 
#' The zonal spread of the L3 bins is applied to the longitude-latitude 
#' regular grid by _a simple method that changed slightly in 2017 ..._
#' is trivial but inherently lossy. The initial focus of `roc` on the L3 bins was
#' precisely to put focus on the statistical fidelity features of the L3 bins, as
#' per Technical Report No. 32. 
#' 
#' (There's no reason to differentiate MODISA and MODIST in terms of the grid it's 
#' just that this token was chosen to distinguish from SeaWiFS in the first versions,
#'  and this might be generalized in future). 
#'
#' @param bins bin index values
#' @param value values with bins
#' @param platform name of platform, e.g. MODISA, SeaWiFS
#'
#' @name oceandata-grids
#' @importFrom tibble tibble
#' @importFrom sp coordinates
bin_smi <- function(bins, value, platform = "MODISA") {
  bins <- tibble::tibble(bin_num = bins, value = value)
  if (!platform == "MODISA") stop("only MODISA  platform currently supported")
  gridmap <- raster(extent(-180, 180, -90, 0), ncol = 8640, nrow = 4320, crs = "+init=epsg:4326")
  ll <- coordinates(gridmap)
  bins <- tibble(bin_num = lonlat2bin(ll[,1], ll[, 2], NUMROWS = 4320), 
                 gridcell = seq(raster::ncell(gridmap))) %>% dplyr::inner_join(bins, "bin_num")
  gridmap[bins$gridcell] <- bins$value
  raster::trim(gridmap)
}


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
