#' Read Level-3 ocean colour.
#'
#' Read the compound types (i.e. tables of variables) from ocean colour L3 NetCDF files.
#'
#' `read_binlist` for just the 'BinList'
#' `read_compound` for just the compound data
#' (not implemented) read_L3_file for everything at once
#' 
#' Compound vars we would usually want for MODISA are "BinList" and `c("Rrs_443", "Rrs_488", "Rrs_555", "Rrs_547")` since
#' these are used for calculating chlorophyll-a. This function is not very general yet, you'll always get the BinList. 
#' This function is specific to files in this form, for any of the platforms (MODISA here): https://oceandata.sci.gsfc.nasa.gov/MODIS-Aqua/L3BIN
#'
#' @param file file path
#' @param compound_vars the variables to read
#' @param select_stat defaults to "sum" and "sum_squared"
#' @return data frame of bin numbers, number of observations and scenes, weights and time record, 
#' a data frame of the compound variables and bin details
#' @export
#' @name read_L3_file
#' @examples
#' f <- system.file("extdata", "ocfiles", "S2008001.L3b_DAY_CHL.nc", package = "roc")
#' bins <- read_binlist(f)
#' bin2lonlat(bins$bin_num, 2160)
#' bin2bounds(bins$bin_num, 2160)
#' 
#' 
#' read_L3_file(system.file("extdata/ocfiles/S2008001.L3b_DAY_RRS.nc", package = "roc"))
read_binlist <- function(file) {
  tibble::as_tibble(rhdf5::h5read(file, name = file.path("/level-3_binned_data", "BinList")))
}
#' @export
#' @name read_L3_file
read_compound <- function(file, compound_vars = NULL, select_stat = c("sum", "sum_squared")) {
  select_stat <- match.arg(select_stat, several.ok = TRUE)
  info <- rhdf5::h5ls(file)
  tab <- table(info$dim); wm <- which.max(tab); test <- names(tab[wm])
  ## get all vars, or just the ones the users wants
  if (is.null(compound_vars))  {
    compound <- setdiff(info$name[info$dim == test], "BinList")
  } else {
    compound <- compound_vars
  }
  compoundpath <- file.path("/level-3_binned_data", compound)
  l <- lapply(compoundpath, function(aname) 
    tibble::as_tibble(rhdf5::h5read(file, name = aname) %>% dplyr::select(select_stat)))
  dplyr::bind_cols(lapply(seq_along(compound), function(i) setNames(l[[i]], sprintf("%s_%s", compound[i], names(l[[i]])))))
}






#' @importFrom stats setNames
read_L3_file <- function(file, compound_vars = NULL, ...) {
  stop("not implemented ")
  ## we need to implement from the binlist/compound reads above
  info <- rhdf5::h5ls(file)
  tab <- table(info$dim); wm <- which.max(tab); test <- names(tab[wm])
  ## get all vars, or just the ones the users wants
  if (is.null(compound_vars))  {
    compound <- info$name[info$dim == test]
  } else {
    compound <- compound_vars
  }
  compoundpath <- file.path("/level-3_binned_data", compound)
  l <- lapply(compoundpath, function(aname) tibble::as_tibble(rhdf5::h5read(file, name = aname)))
  ## not very generalized but ok for L3 RRS
  d <- tibble::as_tibble(rhdf5::h5read(file, name = file.path("/level-3_binned_data", "BinList")))
  d2 <- dplyr::bind_cols(lapply(seq_along(compound), function(i) setNames(l[[i]], sprintf("%s_%s", compound[i], names(l[[i]])))))
  dplyr::bind_cols(d, d2)
}