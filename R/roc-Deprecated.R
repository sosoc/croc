# Regrid ocean colour. 
## 
## Regrid NASA ocean colour RRS values to standard Mapped image with the Johnson chl-a algorithm. 
#### @param tgrid target grid
## @param file L3 bin file name with raw RRS wavelengths
## @param agg aggregate values?
## @param NROWS number of L3 rows
## @param fun function to aggregate by if agg is \code{TRUE}
##
## @importFrom rgdal project
## @importFrom raster setValues
## @export
# regridder <- function(tgrid, file, agg = FALSE, fun = mean, NROWS) {
#   .Deprecated()
#   xy <- lonlatFromCell(tgrid, spatial = FALSE)
#   
#   stop("need to use read_l3_file somehow")   #MDS 2017-05-25
#   #NROWS <- readL3(file, vname = "NUMROWS", bins = FALSE)$NUMROWS
#   binmap <- lonlat2bin(xy[,1], xy[,2], NROWS)
#   function(file) {
#     sens <- if(x$NUMROWS == 4320) "MODISA" else "SeaWiFS"
#     if(x$NUMROWS != NROWS)  stop("file doesn't match this regridder\nfile NUMROWS: ", x$NUMROWS, "\nfunction NUMROWS:", NROWS)
#     
#     if (agg) {
#       vals <-  chla(x, sensor = sens, algo = "johnson")
#       
#       binxy <- do.call(cbind, bin2lonlat(x$bin_num, x$NUMROWS))
#       if (!is.na(projection(tgrid)) & !isLonLat(tgrid)) binxy <- project(binxy, projection(tgrid))
#       gcell <- extract(tgrid, binxy, cellnumbers = TRUE)[,"cells"]
#       tvals <- tapply(vals, gcell, fun)
#       
#       r <- tgrid
#       r[unique(gcell)] <- tvals
#       r[!r>0] <- NA
#     } else {
#       r <- setValues(tgrid, chla(x, sensor = sens, algo = "johnson")[match(binmap, x$bin_num)])
#     }
#     r
#   }
# }

