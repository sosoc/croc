system.time({
  library(raadtools)
library(roc)
library(rgdal)
##library(dplyr)
## raw wavelengths of ocean colour data
prdfiles <- prodfiles()
sens <- c("MODISA" = 4320, SeaWiFS = 2160)
projll <- commonprojections$longlat
isens <- 1

## there is an eight day gap at 2002-07-29
drop <- 26
rrsfiles <- ocfiles(product = names(sens)[isens], varname = "RRS")
prdfiles <- subset(prdfiles, date >= min(rrsfiles$date))
kdfiles <- ocfiles(product = names(sens)[isens], varname = "KD490")
pocfiles <- ocfiles(product = names(sens)[isens], varname = "POC")
asub <- rrsfiles$date >= min(prdfiles$date) & rrsfiles$date <= max(prdfiles$date) & seq(nrow(rrsfiles)) > (drop - 1)
rrsfiles <- rrsfiles[asub, ]
kdfiles <- kdfiles[asub, ]
pocfiles <- pocfiles[asub, ]
all.equal(rrsfiles$date, kdfiles$date)
all.equal(kdfiles$date, pocfiles$date)


ext <- extent(-180, 180, -90, -30)
bins <- crop_init(initbin(4320), ext)

## first date is a dud
date <- as.POSIXct("2002-10-30 00:00:00", tz = "UTC")



## chunk size in days
chunkdays <- 8
dates <- seq(min(prdfiles$date), max(prdfiles$date) + chunkdays * 24 * 3600, by = "1 day")
datefactor <- cut(dates, "8 days")
bigdat <- NULL

jdate <- 27
##for (jdates in seq(nlevels(datefactor))) {
 jdates <- dates[datefactor == levels(datefactor)[jdate]]
  for (idate in seq_along(jdates)) {
    i <- which.min(abs(jdates[idate] - rrsfiles$date))
    x <- readL3(rrsfiles$fullname[i], check = FALSE) 
    x <- roc:::.subsetL3(x, x$bin_num %in% bins)
    suppressWarnings(x$chla <- chla(x, sensor = names(sens[isens]), algo = "johnson"))
  ##prod <- readprod(date)
    dat0 <- as_data_frame(c(list(bin_num = x$bin_num), bin2lonlat(x$bin_num, x$NUMROWS), list(chla = x$chla)))
    x <- readL3(kdfiles$fullname[i], check = FALSE) 
    x <- roc:::.subsetL3(x, x$bin_num %in% bins)
    dat0$kd <- (x$Kd_490_sum / x$weights)[match(dat0$bin_num, x$bin_num)]
    
    x <- readL3(pocfiles$fullname[i], check = FALSE) 
    x <- roc:::.subsetL3(x, x$bin_num %in% bins)
    
    dat0$poc <- (x$poc_sum / x$weights)[match(dat0$bin_num, x$bin_num)]
    
    
    tmpxy <- SpatialPoints(as.matrix(dat0[, c("x", "y")]), proj4string = CRS(projll))  ## project(as.matrix(dat[, c("x", "y")]), projection(prod))
    suppressWarnings({
      dat0$prodstart <- extract(readprod(min(jdates)), tmpxy)
      dat0$prodend <- extract(readprod(max(jdates)), tmpxy)
      dat0$ice <- extract(readice(date, setNA = FALSE), tmpxy)
      dat0$ice[is.na(dat0$ice)] <- 0 ## for values out of bounds
      dat0$mld <- extract(setExtent(readmld(date), extent(-180, 180, -90, -30)), tmpxy)
    })
    bigdat <- bind_rows(bigdat, dat0)
    print(idate)
  }
 
#}

## bigdat %>% arrange(bin_num)
bigdat %>% 
   ## filter to non-missing prod and chla values
   filter(!(is.na(prodstart) & is.na(prodend))) %>% 
   filter( !is.na(chla)) %>% 
   filter( !ice > 0) %>% 
   ## and filter any bad values
   filter(!is.na(mld) & ! is.na(ice) ) %>% 
  mutate(prod = prodend - prodstart)
 
 ## build a grid in equal area so we can smooth out the missing values a little
gr <- setValues(projectExtent(raster(ext, nrow = 50, ncol = 50, crs = projll), "+proj=laea +lat_0=-90"), 0)

bigdat$cell <- cellFromXY(gr, project(as.matrix(bigdat[, c("x", "y")]), projection(gr)))
d1 <- bigdat %>% group_by(cell) %>% summarize(chla = mean(chla, na.rm = TRUE), 
                                              prodstart = sum(prodstart, na.rm = TRUE), 
                                              prodend = sum(prodend, na.rm = TRUE), 
                                              mld = mean(mld), 
                                              kd = mean(kd), 
                                              poc = sum(poc, na.rm = TRUE), 
                                              n = n())
op <- par(mfrow = n2mfrow(ncol(d1)))
for (ivar in names(d1)) {
  gr[1:ncell(gr)] <- 0
  gr[d1$cell[!is.na(d1$cell)]] <- d1[[ivar]][!is.na(d1$cell)]

  pal <- chl.pal(palette  = TRUE)
  plot(gr, col = pal$cols)
  title(ivar)
}
par(op)
##plot(gr, col = pal$cols, breaks = pal$breaks)

})

# 
#    ## aggregate bin values to means
#    group_by(bin_num) %>% summarize(prodstart = mean(prodstart, na.rm = TRUE), prodend = mean(prodend, na.rm = TRUE),  mld = mean(mld), chla = mean(chla)) 
# 
# scl <- function(x) (x - min(x)) / diff(range(x))
# 
# plot(project(cbind(d$x, d$y), projection(readice())), pch = ".", col = chl.pal(100)[scl(d$prodstart - d$prodend) * 99 + 1], asp = 1)
# 

 

##dat  %>% summarize(nprod = sum(!is.na(prod)), nchla = sum(!is.na(chla)), nshare = sum(!is.na(prod) & !is.na(chla)))
