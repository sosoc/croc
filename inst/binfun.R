library(roc)
library(raster)
ibin <- initbin(24)

x <- bin2bounds(seq(ibin$totbins), length(ibin$numbin))
class(x) <- c("sbin", "list")

.extent.sbin <- function(x, ...) extent(range(c(x$west, x$east)), range(c(x$south, x$north)))
setMethod("extent", "sbin", .extent.sbin)

plot.sbin <- function(x, ...) {
  ex <- extent(x)
  plot(c(xmin(ex), xmax(ex)), c(ymin(ex), ymax(ex)), type = "n")
  rect(x$west, x$south, x$east, x$north, ...)
  invisible(NULL)
}

plot(x)


