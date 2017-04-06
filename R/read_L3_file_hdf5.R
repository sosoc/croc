#' Read variables from `oceancolor` L3 bin files. 
#' 
#' Compound vars we would usually want are "BinList" and `c("Rrs_443", "Rrs_488", "Rrs_555", "Rrs_547")` since
#' these are used for calculating chlorophyll-a. This function is not very general yet, you'll always get the BinList. 
#' 
#' This function is specific to files in this form, for any of the platforms (MODISA here): https://oceandata.sci.gsfc.nasa.gov/MODIS-Aqua/L3BIN
#' @param file file name
#' @param compound_vars the variables to read
#'
#' @return
#' @export
#'
#' @examples
read_L3_file <- function(file, compound_vars = NULL) {
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