#' L3 bin from NetCDF 4 compound types. 
#'
#' WIP
#' @param date date to read
#' @param ... ignored
#' @param inputfiles input file dataframe
#' @return tibble
#' @export
#' @importFrom tibble as_tibble
#' @importFrom rhdf5 h5ls
#' @importFrom dplyr bind_cols
#' @examples
#' root <- "data/oceandata.sci.gsfc.nasa.gov/SeaWiFS/L3BIN"
#' data_dir <- getOption("default.datadir")  ## raadtools 
#' files <- structure(list(file = file.path(root, "1997/247/S1997247.L3b_DAY_RRS.nc"), 
#' fullname = file.path(data_dir, file.path(root, "1997/247/S1997247.L3b_DAY_RRS.nc")), 
#' date = structure(873331200, class = c("POSIXct", "POSIXt"
#' ), tzone = "GMT")), .Names = c("file", "fullname", "date"
#' ), row.names = 1L, class = "data.frame")
#' read_l3(inputfiles = files)
#' 
read_l3 <- function(date, ..., inputfiles) {
  ## somehow make independent of raadtools?
  #files <- raadtools::ocfiles(product = "SeaWiFS", varname = "RRS", type = "L3b", ext = "nc")
  files <- inputfiles
  
  if (missing(date)) date <- min(files$date)
  date <- raadtools::timedateFrom(date)
  dt <- files$date - date
  ##if (min(files$date) - date)
  i <- which.min(abs(dt))
  f <- files$fullname[i]
 read_L3_file(f)
}

