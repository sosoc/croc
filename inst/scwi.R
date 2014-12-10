## generate SCWI composite for SeaWiFS

## benchmark date
library(roc)
library(raadtools)


## composite products are means, 
## so a CHL_ product must be used as x$chlor_a_sum/x$weights
## which is directly comparable to the accumulated sum of all component sum(xi$chlor_a_sum)/ni


## see compare1day.R
## see compare1week.R

sensor <- "SeaWiFS"  
##sensor <- "MODISA"

rrsfiles <- ocfiles(time.resolution = "daily", product = sensor, varname = "RRS")
chlfiles <- ocfiles(time.resolution = "daily", product = sensor, varname = "CHL")

## AAD derived:
## A20023552010079.L3m_SCWI_CHL_chlor_a_9km

## SeaWiFS southern-summer climatology
##S19973552010079.L3b_SCWI_CHL.main.bz2

## MODIS southern-summer climatology (as at 2014-12-09)
##A20023552013079.L3b_SCWI_CHL.main.bz2

library(roc)
library(raadtools)

## allfiles() introduced after 0.2-0
targetfile <- grep("S19973552010079.L3b_SCWI_CHL.main", allfiles()$fullname, value = TRUE)
##targetfile <- "/rdsi/PRIVATE/data/oceandata.sci.gsfc.nasa.gov/SeaWiFS/L3BIN/1998/001/S19980012010031.L3b_MC_CHL.main"
## "oc_CHL" is our target oceancolor
oc_CHL <- readL3(targetfile)

rrsfiles1 <- ocfiles(time.resolution = "daily",product = sensor, varname = "RRS")
chlfiles1 <- ocfiles(time.resolution = "daily",product = sensor, varname = "CHL")

if (!identical(rrsfiles1$date, chlfiles1$date)) {
  stop("wait, the file collections should match!!!")
}

## internal function .filetok
tokens <- roc:::.filetok(basename(rrsfiles1$fullname))
jday <- as.numeric(tokens$jday)
asub <- jday <= 79 | jday >= 355
##asub <- jday >= 1 & jday <= 31
## this is the set of all daily source files
## (we can assume they match since the error was not triggered above)
rrsfiles <- rrsfiles1[asub, ]
chlfiles <- chlfiles1[asub, ]

init0 <- initbin(oc_CHL$NUMROWS)
## vectors to store our cumulative values
rjchlor_a <- nn <- chlor_a <- weights <- numeric(init0$totbins)

files <- rrsfiles
## save a test for any bad files
bad <- logical(nrow(files))
for (i in seq(nrow(files))) {
 d <- try(readL3(files$fullname[i]))
  if (!inherits(d, "try-error")) {
    ##chlor_a[d$bin_num] <- chlor_a[d$bin_num] + chla(d, algo = "oceancolor", sensor = sensor) / weights[d$bin_num] ## clim from RRS
    chlor_a[d$bin_num] <- chlor_a[d$bin_num] + chla(d, algo = "oceancolor", sensor = sensor)  
    rjchlor_a[d$bin_num] <- rjchlor_a[d$bin_num] + chla(d, algo = "johnson", sensor = sensor)  
     nn[d$bin_num] <- nn[d$bin_num] + 1
  } else {
    bad[i] <- TRUE
  }
  if (i %% 100 == 0) print(i)
}

if (any(bad)) {
  print(sprintf("%i were bad", sum(bad)))
}
keep <- chlor_a > 0
chl_mean <- chlor_a[keep]/nn[keep]
rjchl_mean <- rjchlor_a[keep]/nn[keep]
ocxy <- bin2lonlat(oc_CHL$bin_num, oc_CHL$NUMROWS)
xy <- bin2lonlat(which(keep), length(init0$latbin))

par(mfrow = c(3, 1))
plot(xy, col = chl.pal(chl_mean), pch = ".")
text(0, -80, format(sum(chl_mean)))
plot(ocxy, col = chl.pal(oc_CHL$chlor_a_sum/oc_CHL$weights), pch = ".")
text(0, -80, format(sum(oc_CHL$chlor_a_sum/oc_CHL$weights)))
plot(xy, col = chl.pal(rjchl_mean), pch = ".")
text(0, -80, format(sum(rjchl_mean)))





