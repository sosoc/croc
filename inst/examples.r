
files <- subset(ocfiles(product =  "SeaWiFS"), format(date, "%Y") %in% "2007")

ext <- extent(140, 160, -48, -30)
bins <- crop_init(initbin(2160), ext)
sums <- nns <- numeric(length(bins))
xy <- bin2lonlat(bins, 2160)
for (i in seq_along(files$date)) {
  x <- readL3(files$fullname[i])
  asub <- x$bin_num %in% bins
  x <- .subsetL3(x, asub)
  x$chla <- chla(x, sensor = "SeaWiFS", algo = "oceancolor")
  mb <- match(x$bin_num, bins)
  sums[mb] <- sums[mb] + x$chla
  nns[mb] <- nns[mb] + 1

  if (i %% 15 == 0) plot(xy, pch = 16, cex = 0.4 * 1/cos(xy$y * pi / 180), col = chl.pal(sums/nns))
  }



plot(xy, pch = 16, cex = 0.4 * 1/cos(xy$y * pi / 180), col = chl.pal(sums/nns))

## use geographic extent to limit bins
crop_init()
## match read bins with desired set
readbins %in% croppedbins
## sum desired parameter into croppedbins
++
## create bounds from croppedbins
bin2bounds
## project boundaries to desired projection
project_bounds

#ext <- extent(-59.89167, -59.27116, -54.52204, -54.1988)
ext <- extent(-62, -58, -56, -53)

m <- initbin(4320)
s <- initbin(2160)
sbins <- crop_init(s, ext)
mbins <- crop_init(m, ext)

sll <- bin2lonlat(sbins, 2160)
mll <- bin2lonlat(mbins, 4320)

sb <- bin2bounds(sbins, 2160)
mb <- bin2bounds(mbins, 4320)

plot(mll)
points(sll, pch = 16)
# plot(mll, ylim = c(-90, -89.8), xlim = c(-180, 180))
# points(sll, pch = 16)

rect(sb$east, sb$south, sb$west, sb$north, col = rgb(c(0.2, 0.8), c(0.2, 0.8), c(0.2, 0.8), 0.7))
rect(mb$east, mb$south, mb$west, mb$north, border = "firebrick")

pext <- extent(projectExtent(raster(ext, crs = "+proj=longlat"), prj))
pmll <- project(do.call(cbind, mll), prj)
psll <- project(do.call(cbind,sll), prj)
psb <- project_bounds(sb, prj)
pmb <- project_bounds(mb, prj)
plot(pmll)
points(psll, pch = 16)


plot(extent(pext))
.plotp(psb, col = rep(rgb(c(0.2, 0.8), c(0.2, 0.8), c(0.2, 0.8), 0.7), length = nrow(psb), each = 5))
.plotp(pmb)



library(rworldmap)
data(countriesLow)

plot(ext)
points(bin2lonlat(listofbins, nrows), pch = ".")

files <- tail(ocfiles(product = "SeaWiFS"), 12)
library(roc)
ext <- extent(-80, -52, -75, -53)
ext <- extent(-59.89167, -59.27116, -54.52204, -54.1988)

prj <- "+proj=laea +lon_0=-65 +lat_0=-58"
pext <- projectExtent(raster(ext, crs = "+proj=longlat"), prj)

plot(extent(pext))
##plot(ext)
sinit <- initbin(2160)

library(rgdal)
for (i in seq(nrow(files))) {
  listofbins <- crop_init(sinit, ext)
  x0 <- readL3(files$fullname[i])
  asub <- x0$bin_num %in% listofbins
  ## points(bin2lonlat(x0$bin_num,nrows), pch = ".")
  if (any(asub)) {  x1 <- .subsetL3(x0, asub)
  x1$chla <- chla(x1, sensor = "SeaWiFS", algo = "oceancolor")
  bb <- project_bounds(bin2bounds(x1$bin_num, nrows), prj)
.plotp(bb, col = chl.pal(x1$chla)) 
  #rect(pbb$east, pbb$south, pbb$west, pbb$north, col = chl.pal(x1$chla),  border = NA)
  ##points(bin2lonlat(x1$bin_num, nrows), col = chl.pal(x1$chla), pch = ".")
  }
}

