
## function to ensure 1st and last values are 1/min or
#' @importFrom utils tail
.snapout1 <- function(x, min, max) {
  if (x[1] > min) x <- c(x[1] - 1, x)
  if (tail(x, 1L) < max) x <- c(x, tail(x, 1) + 1)
  x
}
.seqfl <- function(fl) seq(fl[1], fl[length(fl)])

#' Crop L3 init object with an extent
#' 
#' Crop L3 list, returns bins that fall within the extent. 
#' @param x initbin object
#' @param ext raster extent object, or object to create an extent from
#' @return  integer vector of bins
#' @examples
#' \dontrun{ init <- initbin(24)
#' crop_init(init, extent(100, 110, -50, -45))
#' }
#' @export
#' @importMethodsFrom raster extent
crop_init <- function(x, ext) {
  ext <- extent(ext)
  nrows <- length(x$basebin)
  ilat <- which(x$latbin >= raster::ymin(ext) & x$latbin <= raster::ymax(ext) )
  ilat <- .snapout1(ilat, 1L, nrows)
  
  basebin <- x$basebin[ilat]
  latbin <- x$latbin[ilat]
  listofbins <- vector("list", length(basebin))
  for (i in seq_along(basebin)) {
    firstbin <- lonlat2bin(raster::xmin(ext), latbin[i], nrows)
    lastbin <- lonlat2bin(raster::xmax(ext), latbin[i], nrows)
    firstlast <- .snapout1(c(firstbin, lastbin), basebin[i], basebin[+1] - 1)
    listofbins[[i]] <- .seqfl(firstlast)
  }
  
  listofbins <- unlist(listofbins)
 listofbins 
}

.bb2m <- function(eswn) {
 x <-  expand.grid(eswn[c(1, 3)], eswn[c(2, 4)])[c( 1, 3, 4, 2, 1),]
 #plot(x)
 #polygon(x[,1], x[,2], col = "grey")
 as.matrix(x)
}

## subset L3 list
.subsetL3 <- function(x, sub) {
  lapply(x, function(x) if (length(x) > 1) x[sub] else x)
}

#' Insert redundant vertices to allow reprojection. 
#' 
#' Densify objects. 
#' @param x 2 row matrix of coordinates to densify
#' @param maxdist the maximum distance 
#' @param longlat use longlat on the ellipsoid for distance?
#' @importFrom geosphere gcIntermediate
#' @importFrom sp spDistsN1
#' @export
densify <- function(x, maxdist, longlat = longlat) {
  if (missing(maxdist)) {
    warning("No minimum distance specified, no densifying done")
    return(x)
  }
  dist <- spDistsN1(x[1L,,drop=FALSE], x[2L,,drop=FALSE], longlat = TRUE)
  if (dist >= maxdist) {
    n <- dist %/% maxdist
    x <- gcIntermediate(x[1L,], x[2L,], n = n, addStartEnd = TRUE)
  }
  x
}

#' @importFrom sp Polygon Polygons
# create Polygons from a list of rect coordinates
.bb2poly <- function(x, maxdist = NULL) {
  x1 <- do.call(cbind, x)
  l <- vector("list", nrow(x1))
  cnt <- 0
  for (i in seq(1, nrow(x1), by = 1)) {
    cnt <- cnt+1
    m1 <- .bb2m(x1[i,])
    al <- vector("list", 4)
    if (!is.null(maxdist)) {
      for (j in seq_along(al)) al[[j]] <- densify(m1[j:(j+1), ], maxdist, longlat = TRUE)
      m1 <- do.call(rbind, al)
      eps0 <- sqrt(.Machine$double.eps)
      if (abs(m1[1,1] - 180) < eps0) {
        bad <- abs(m1[,1] + 180) < eps0
        if (any(bad)) m1[bad,1] <- 180
      }
      if (abs(m1[1,1] + 180) < eps0 ) {
        
        bad <- abs(m1[,1] - 180) < eps0
        
        if (any(bad))  m1[bad,1] <- -180
      }
      
    }
    l[[cnt]] <- Polygons(list(Polygon(m1)), as.character(i))
  }
  l
  
}

#' @importFrom graphics polygon
.plotp <- function(x, col = NA , ...) {
  for (i in seq(1, nrow(x), by = 5)) {
    polygon(x[i:(i+4), ], col = col[i], ...)
  }
}

# project_bounds <- function(bb, prj) {
#   coords <- do.call(cbind, bb)
#   allc <- cbind(as.vector(t(coords[,c(1, 1, 3, 3, 1)])), as.vector(t(coords[,c(2, 4, 4, 2, 2)])))
#   
#   rgdal::project(allc, prj)
# }



##setMethod("crop", crop_L3)