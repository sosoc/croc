
#' Read and summarize ocean colour variables between a date range. 
#' 
#' Read and summarize ocean colour variables. 
#' @param daterange two dates, specifying a start and end day
#' @param xylim extent specification (in longitude/latitude)
#' @param varname variable to read, defaults to Johnson 2013 chlorophyll-a
#' @param grid grid specification to rasterize to (optional)
#' @param platform which satellite source (MODSIA, SeaWiFS, ...)
readoc <- function(daterange,  
                   xylim = NULL,  
                   varname = "CHL_RJ", grid = NULL, platform = "MODISA", ...) {
  if (varname == "RRS") stop("RRS not allowed, use CHL_RJ for Johnson chlorophyll or CHL for ocean color chlorophyll")
  if (varname == "CHL_RJ") varname0 <- "RRS" else varname0 <- varname
  daterange <- range(timedateFrom(daterange))
  files <- ocfiles(varname = varname0, product = platform)
  start0 <- which.min(abs(files$date - daterange[1]))
  end0 <- which.min(abs(files$date - daterange[2]))

    ## check daterange, sequence of files
  files <- files[seq(start0, end0), ]
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
  return(list(NUMROWS = x$NUMROWS, bin_num = bins, sum = sum0[bins], n = n0[bins]))
 ## if (is.null(grid))
##    list(NUMROWS = x$NUMROWS, bin_num = bins, chlor_a = (sum0/n0)[bins]) else grid
  
}
