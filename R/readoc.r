
#' Read and summarize ocean colour variables from specific dates, or between a date range. 
#' 
#' Read and summarize ocean colour variables. 
#' @param daterange two dates, specifying a start and end day
#' @param xylim extent specification (in longitude/latitude)
#' @param varname variable to read, defaults to Johnson 2013 chlorophyll-a
#' @param grid grid specification to rasterize to (optional)
#' @param platform which satellite source (MODSIA, SeaWiFS, ...)
#' @param daterange if TRUE every day between the min and max dates is used
#' @param ... unused
#' @importFrom raadtools timedateFrom ocfiles
#' @importFrom sp CRS spTransform
#' @export
readoc <- function(dates,  
                   xylim = NULL,  
                   varname = "CHL_RJ", grid = NULL, platform = "MODISA", daterange = FALSE, ...) {
  if (varname == "RRS") stop("RRS not allowed, use CHL_RJ for Johnson chlorophyll or CHL for ocean color chlorophyll")
  if (varname == "CHL_RJ") varname0 <- "RRS" else varname0 <- varname
  ##daterange <- range(timedateFrom(daterange))
  dates <- as.POSIXct(dates, tz = "GMT")
  files <- ocfiles(varname = varname0, product = platform)
  if (daterange) {
    dates <- range(daterange)
    start0 <- which.min(abs(files$date - daterange[1]))
     end0 <- which.min(abs(files$date - daterange[2]))

      ## check daterange, sequence of files
      files <- files[seq(start0, end0), ]
      } else {
        files <- subset(files, as.Date(date) %in% as.Date(dates))
      }
  nr <- c(MODISA = 4320, SeaWiFS= 2160)
  init <- initbin(nr[platform])
  sum0 <- n0 <- numeric(init$totbins)
  if (!is.null(xylim)) ibin <- crop_init(init, extent(xylim))
  varmap <- c(CHL = "chlor_a", POC = "poc", KD490 = "Kd_490", PAR = "par")
  listname <- sprintf("%s_sum", varmap[varname])
  for (i in seq(nrow(files))) {
    x <- readL3(files$fullname[i], check = FALSE)
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
  if (is.null(grid)) return(x)
 ## if (is.null(grid))
##    list(NUMROWS = x$NUMROWS, bin_num = bins, chlor_a = (sum0/n0)[bins]) else grid
  ll <- do.call(cbind, bin2lonlat(x$bin_num, nr[platform]))
  if (!isLonLat(grid)) {
    ll <- spTransform(SpatialPoints(ll, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")), 
                    CRS(projection(grid)))
  }
  x$cell <- cellFromXY(grid, ll)
  
  x$NUMROWS <- NULL
  x <- as_data_frame(x)
  x <- x %>% filter(!is.na(cell)) %>% group_by(cell) %>% summarize(chl = mean(sum/n, na.rm = TRUE)) 
  grid[x$cell] <- x$chl
  grid
}
