
.crossesdateline <- function(x) {
  if (raster::xmax(x) > 180) TRUE else FALSE
}
#' Read and summarize ocean colour variables from specific dates, or between a date range. 
#' 
#' Read and summarize ocean colour variables. 
#'
#' @param xylim extent specification (in longitude/latitude)
#' @param varname variable to read, defaults to Johnson 2013 chlorophyll-a
#' @param grid grid specification to rasterize to (optional)
#' @param platform which satellite source (MODSIA, SeaWiFS, ...)
#' @param daterange if TRUE every day between the min and max dates is used
#' @param dates specific dates to read and summarize
#' @param binsum logical, summarize the raw bins (overrides grid)
#' @param ... unused
#' @param inputfiles the data frame of available files 
#' @importFrom sp CRS spTransform
#' @importFrom dplyr %>% filter
#' @importFrom rlang .data
#' @export
readoc <- function(dates,  
                   xylim = NULL, binsum = TRUE, 
                   varname = "CHL_RJ", grid = NULL, 
                   platform = "MODISA", daterange = FALSE, ..., inputfiles) {
  if (varname == "RRS") {
    stop("RRS not allowed, use CHL_RJ for Johnson chlorophyll or CHL for ocean color chlorophyll")
  }
  if (varname == "CHL_RJ") varname0 <- "RRS" else varname0 <- varname
  dates <- as.POSIXct(dates, tz = "GMT")
  files <- inputfiles
  if (daterange) {
    dates <- range(daterange)
    start0 <- which.min(abs(files$date - daterange[1]))
     end0 <- which.min(abs(files$date - daterange[2]))

      ## check daterange, sequence of files
      files <- files[seq(start0, end0), ]
      } else {
        files <- subset(files, as.Date(date) %in% as.Date(dates))
      }
      if (nrow(files) < 2) warning("only one file found")
      if (nrow(files) < 1) stop("no files found")
  nr <- c(MODISA = 4320, SeaWiFS= 2160)
  init <- initbin(nr[platform])
  sum0 <- n0 <- numeric(init$totbins)
  if (!is.null(xylim)) ibin <- crop_init(init, extent(xylim))
  varmap <- c(CHL = "chlor_a", POC = "poc", KD490 = "Kd_490", PAR = "par")
  for (i in seq(nrow(files))) {
    # tentatively now use rhdf5 function MDS 2017-05-25
    x <- read_L3_file(files$fullname[i])
    if (!is.null(xylim)) x <- .subsetL3(x, x$bin_num %in% ibin)
    if (varname == "CHL_RJ") {
      var <- chla(x, sensor = platform, algo = "johnson")
    } else {
      var <- x[[varmap[varname]]] / x[["weights"]]
    }
    sum0[x$bin_num] <- sum0[x$bin_num] + var
    n0[x$bin_num] <- n0[x$bin_num] + 1
  }
  bins <- which(n0 > 0)
  x <- list(NUMROWS = x$NUMROWS, bin_num = bins, sum = sum0[bins], n = n0[bins])
  if (is.null(grid) & !binsum) return(x)
  if (binsum & is.null(grid)) {
    cat("returning summarized table of bin values, using NUMROWS: ", x$NUMROWS, "\n", sep = "")
    NROWS <- x$NUMROWS
    x$NUMROWS <- NULL
    x <- x %>% tibble::as_tibble() %>% dplyr::group_by(.data$bin_num) %>% 
      dplyr::summarize(chl = mean(sum/dplyr::n(), na.rm = TRUE))
    xy <- bin2lonlat(x$bin_num, NROWS)
    x$binlon <- xy[[1]]
    x$binlat <- xy[[2]]
    return(as.data.frame(x))
  }
  ## zap input grid
  grid[] <- 0
ll <- do.call(cbind, bin2lonlat(x$bin_num, nr[platform]))
  if (!isLonLat(grid)) {
    ll <- spTransform(SpatialPoints(ll, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")), 
                    CRS(projection(grid)))
  } else {
  if (.crossesdateline(grid)) {
    ll[ll[,1] < 0,1] <- ll[ll[,1] < 0,1] + 360
  }
  }
  x$cell <- raster::cellFromXY(grid, ll)
  x$NUMROWS <- NULL
  x <- as_tibble(x)
  x <- x %>% filter(!is.na(.data$cell)) %>% dplyr::group_by(.data$cell) %>% 
    dplyr::summarize(chl = mean(.data$sum / .data$n, na.rm = TRUE)) 
  grid[x$cell] <- x$chl
  grid
}