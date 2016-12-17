#' #' #oc <- ocfiles(time.resolution = "daily", product = "MODISA", varname = "RRS", type = "L3b", ext = "nc")
#' #'
#' #' 
#' read_l3bin <- function(x)
#'   x <- file.path(getOption("default.datadir"), "data/oceandata.sci.gsfc.nasa.gov/MODISA/L3BIN/2015/306/A2015306.L3b_DAY_RRS.nc")
#'   info <- rhdf5::h5ls(x)
#'   tab <- table(info$dim); wm <- which.max(tab); test <- names(tab[wm])
#'   compound <- info$name[info$dim == test]
#'   compoundpath <- file.path("/level-3_binned_data", compound)
#'   l <- lapply(compoundpath, function(aname) rhdf5::h5read(x, name = aname))
#'   for (i in seq_along(l)) if (grepl("sum", names(l[[i]]))) names(l[[i]]) <- paste()
#'   }
#' Basic L3 bin files 
#'
#' Read from L3 bin. 
#' @param x filename path to L3 bin OC file (HDF4)
#' @param vname names of VData parameters to read (will read both _sum and _ssq)
#' @param bins read out the bin number and other metadata (default TRUE)
#' @param check test for presence of auxiliary files, TRUE or FALSE
#' @export
readL3 <- function(x, vname, bins = TRUE, check = TRUE) {
  if (length(x) > 1L) warning("only first file considered")
  x <- iz2(x[1])
  if (check) tst <- .checkAux(x)
  vdatalist <- rrshdf4::vdatainfo(x)
  if (missing(vname)) {
    vdatalist <- vdatalist[!names(vdatalist) %in% c("SEAGrid", "BinList", "BinIndex")]
  } else {
    vdatalist <- vdatalist[names(vdatalist) == vname]
  }
  if (.isNC(x)) stop("NetCDF format not yet supported")
  bl <- rrshdf4:::binlist(x, names(vdatalist), bins = bins)
  bl$filename <- x
  bl
}

.isNC <- function(x) {
  ## end in .nc or not?
  grepl("nc$", x)
}
.checkAux <- function(x) {
  ## if this needs aux files, we better ensure they are present and decompressed
  ## aux periods
  ##c("MO", "R32", "YR", "SNWI")
  ## What is MC, WC for SeaWiFS?  http://oceandata.sci.gsfc.nasa.gov/SeaWiFS/L3BIN/1998/001/
  
  ## season days 
  ## SeaWiFS?  080 (SCSP) (seasonal composite?)
  ## 080 (SNSP), 172 (SNSU), 264 (SNAU), 355 (SNWI)
  ## MODIS
  ## RRS:  x00:x11
  ## NSST: x00
  ## SST:  x00
  ## SST4: x00
  ## CU (only for SST4 in 20020012006181): x00 ?why
  
  ## this is very *&*^ complicated, just check if aux are present and fail if compressed
  ## ewk, this needs to include the temporal tag . . .
  otherfiles <- unique(gsub(".bz2$", "", list.files(dirname(x), pattern = "\\.x0")))
  if (!all(file.exists(file.path(dirname(x), otherfiles)))) stop("not all auxiliary files uncompressed: ", x)
  NULL
  
}
## this really needs testing
## now that I've turned off file delete
iz2 <- function(x) {
  needsdecompress <- grepl(".bz2$", x) 
  if (needsdecompress) {
    x <- gsub(".bz2$", "", x)
    if (!file.exists(x)) stop("uncompressed file not available:", x)
  }
  x
}

# ## this really needs testing
# ## now that I've turned off file delete
# .iz2 <- function(x) {
#   needsdecompress <- grepl(".bz2$", x) 
#   if (needsdecompress) {
#     f <- gsub(".bz2$", "", x)
#     if (file.exists(x)) {
#       message(sprintf("Attempting to decompress:\n %s", x))
#       system(sprintf("bunzip2 %s --keep", x))
#       ##print("bunzip2")
#       return(f)
#     } else {
#       if (file.exists(f)) return(f)
#       stop(sprintf("no file %s:", x))
#     }
#   }
#   ## return without change
#   x
# }


# chl.pal <- raadtools::chl.pal
# 
# plotbin <- function(x, pal = chl.pal) {
#   ll <- bin2lonlat(x$bin_num)
#   function(x, xlim, ylim, ...) {
#     asub <- ll[,1] >= xlim[1] & ll[,1] <= xlim[2] & ll[,2] >= ylim[1] & ll[,2] <= ylim[2]
#      plot(ll[asub, ], col = pal(), ...)
#   }
# }

## supported sensors
.sensornames <- function() {
  c("SeaWiFS", "MODISA")
}
.sensor <- function(x) {
  nms <- names(x)
}








