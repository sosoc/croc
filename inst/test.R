g <- roc:::.defaultgrid()
oc <- ocfiles(product = "MODISA", time.resolution = "daily")

rg <- regridder(g, oc$fullname[220])
rgs <- g; res(rgs) <- res(g) * c(8, 1.5)


rgs <- setValues(rgs, 0)
rg2 <- regridder(rgs, oc$fullname[220], agg = TRUE)
g1 <- rg(oc$fullname[220])
g2 <- rg2(oc$fullname[220])

rg3 <- regridder(setValues(projectExtent(rgs, "+proj=laea +lat_0=-90"), 0), oc$fullname[220], agg = TRUE)
g3 <- rg3(oc$fullname[220])


library(maptools);data(wrld_simpl)
pal <- chl.pal(palette = TRUE)
plot(g1, col = pal$cols, breaks = pal$breaks, addfun = function() plot(wrld_simpl, add = TRUE), legend = FALSE)
plot(g2, col = pal$cols, breaks = pal$breaks, add = TRUE, legend = FALSE)

plot(g3, col = pal$cols, breaks = pal$breaks, legend = FALSE)


## 1. weekly
fname <- "/rdsi/PRIVATE/data/oceandata.sci.gsfc.nasa.gov/MODISA/L3BIN/2012/001/A20120012012008.L3b_8D_RRS.main"
x <- readL3(fname, check = FALSE)
## THIS WORKS
x$chla <- chla(x, sensor = "MODISA", algo = "johnson")
xy1 <- bin2lonlat(x$bin_num, x$NUMROWS)
points(xy1, pch = ".", col = chl.pal(x$chla))
## AND THIS
g <- roc:::.defaultgrid()
xy <- coordinates(g) ##.lonlatFromCell(tgrid, spatial = FALSE)
NROWS <- readL3(fname, vname = "NUMROWS", bins = FALSE, check = FALSE)$NUMROWS
binmap <- lonlat2bin(xy[,1], xy[,2], NROWS)
x <- readL3(fname, check = FALSE)
sens <- if(x$NUMROWS == 4320) "MODISA" else "SeaWiFS"
if(x$NUMROWS != NROWS)  stop("file doesn't match this regridder\nfile NUMROWS: ", x$NUMROWS, "\nfunction NUMROWS:", NROWS)
g1 <- setValues(g, chla(x, sensor = sens, algo = "johnson")[match(binmap, x$bin_num)])
pal <- chl.pal(palette = TRUE);plot(g1, col = pal$cols, breaks = pal$breaks)     
  
## 2. daily
fname <- "/rdsi/PRIVATE/data/oceandata.sci.gsfc.nasa.gov/MODISA/L3BIN/2012/001/A2012001.L3b_DAY_RRS.main"
x <- readL3(fname, check = FALSE)

x$chla <- chla(x, sensor = "MODISA", algo = "johnson")
xy1 <- bin2lonlat(x$bin_num, x$NUMROWS)
plot(extent(-180, 180, -90, -30))
points(xy1, pch = ".", col = chl.pal(x$chla))

g <- roc:::.defaultgrid()
xy <- coordinates(g) ##.lonlatFromCell(tgrid, spatial = FALSE)
NROWS <- readL3(fname, vname = "NUMROWS", bins = FALSE, check = FALSE)$NUMROWS
binmap <- lonlat2bin(xy[,1], xy[,2], NROWS)

sens <- if(x$NUMROWS == 4320) "MODISA" else "SeaWiFS"
if(x$NUMROWS != NROWS)  stop("file doesn't match this regridder\nfile NUMROWS: ", x$NUMROWS, "\nfunction NUMROWS:", NROWS)
g1 <- setValues(g, chla(x, sensor = sens, algo = "johnson")[match(binmap, x$bin_num)])
pal <- chl.pal(palette = TRUE);
plot(g1, col = pal$cols, breaks = pal$breaks)     

## 3. 
oc <- ocfiles(product = "MODISA", time.resolution = "daily")
file <- oc$fullname[220]
tgrid <- roc:::.defaultgrid()

xy <- coordinates(tgrid) ##.lonlatFromCell(tgrid, spatial = FALSE)
NROWS <- readL3(file, vname = "NUMROWS", bins = FALSE, check = FALSE)$NUMROWS
binmap <- lonlat2bin(xy[,1], xy[,2], NROWS)

  x <- readL3(file)
  sens <- if(x$NUMROWS == 4320) "MODISA" else "SeaWiFS"
  if(x$NUMROWS != NROWS)  stop("file doesn't match this regridder\nfile NUMROWS: ", x$NUMROWS, "\nfunction NUMROWS:", NROWS)
 g2 <-  setValues(tgrid, chla(x, sensor = sens, algo = "johnson")[match(binmap, x$bin_num)])
plot(g2, col = pal$cols, breaks = pal$breaks) 


## 4. weekly
fname <- "/rdsi/PRIVATE/data/oceandata.sci.gsfc.nasa.gov/MODISA/L3BIN/2012/001/A20120012012008.L3b_8D_RRS.main"
x <- readL3(fname, check = FALSE)
## THIS WORKS
x$chla <- chla(x, sensor = "MODISA", algo = "johnson")
xy1 <- bin2lonlat(x$bin_num, x$NUMROWS)
points(xy1, pch = ".", col = chl.pal(x$chla))
## AND THIS
g <- roc:::.defaultgrid()
xy <- coordinates(g) ##.lonlatFromCell(tgrid, spatial = FALSE)
NROWS <- readL3(fname, vname = "NUMROWS", bins = FALSE, check = FALSE)$NUMROWS
binmap <- lonlat2bin(xy[,1], xy[,2], NROWS)
x <- readL3(fname, check = FALSE)
sens <- if(x$NUMROWS == 4320) "MODISA" else "SeaWiFS"
if(x$NUMROWS != NROWS)  stop("file doesn't match this regridder\nfile NUMROWS: ", x$NUMROWS, "\nfunction NUMROWS:", NROWS)
ind <- match(binmap, x$bin_num)

## g2 is divided by the weights
g1 <- setValues(g, (chla(x, sensor = sens, algo = "johnson"))[ind] )
g2 <- setValues(g, (chla(x, sensor = sens, algo = "johnson")/x$weights)[ind] )
pal <- chl.pal(palette = TRUE);
plot(g1, col = pal$cols, breaks = pal$breaks, legend = FALSE)     

       