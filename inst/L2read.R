.L2swathnames <- function() {
  c("longitude", "latitude", "aot_865", "angstrom", "Rrs_412", 
    "Rrs_443", "Rrs_490", "Rrs_510", "Rrs_555", "Rrs_670", "chlor_a", 
    "Kd_490", "pic", "poc", "cdom_index", "par", "l2_flags")
}

.L2metanames <- function() {
  c( "orb_vec", 
  "sun_ref", "att_ang", "sen_mat", "scan_ell", "nflag", "tilt_ranges"
  )
}

.L2template <- function() {
  'HDF4_SDS:%s_L2:"%s":%i'
}
readL2 <- function(file, vartype = c("swath", "meta"), data.frame = TRUE) {
  vartype <- match.arg(vartype)
  sds <- switch(vartype, 
                swath = .L2swathnames(), 
                meta = .L2metanames())
  sdsoffset <- switch(vartype, 
                      swath = 10, 
                      meta = 35)
  bx <- basename(file)
  sdspaths <- sprintf(.L2template(), .filesensor(bx), file, seq_along(sds) + sdsoffset)

  s1 <- stack(sdspaths, quick = TRUE)  
  names(s1) <- sds
  
  if (data.frame) {
   ## s1 <- values(s1)
  ##  s1 <- lapply(seq(ncol(s1)), function(x) s1[,x])
  ##  names(s1) <- sds
    s1 <- lapply(seq(nlayers(s1)), function(x) values(s1[[x]]))
    names(s1) <- sds
    s1 <- as_data_frame(s1)
  }
  s1
}


system.time({

.filesensor <- roc:::.filesensor
library(roc)
library(raster)

# fs <- file.path("/rdsi/PRIVATE/data/oceandata.sci.gsfc.nasa.gov/SeaWiFS/L2/1998/001", 
# c("S1998001013310.L2_GAC_OC", "S1998001013310.L2_MLAC_OC", "S1998001031210.L2_GAC_OC", 
#   "S1998001031210.L2_MLAC_OC", "S1998001045112.L2_GAC_OC", "S1998001045112.L2_MLAC_OC", 
#   "S1998001063009.L2_GAC_OC", "S1998001063009.L2_MLAC_OC", "S1998001080908.L2_GAC_OC", 
#   "S1998001080908.L2_MLAC_OC", "S1998001094808.L2_GAC_OC", "S1998001094808.L2_MLAC_OC", 
#   "S1998001112707.L2_GAC_OC", "S1998001112707.L2_MLAC_OC", "S1998001130607.L2_GAC_OC", 
#   "S1998001130607.L2_MLAC_OC", "S1998001144506.L2_GAC_OC", "S1998001144506.L2_MLAC_OC", 
#   "S1998001162406.L2_GAC_OC", "S1998001162406.L2_MLAC_OC", "S1998001175654.L2_GAC_OC"
# ))

fs <- list.files("/rdsi/PRIVATE/data/oceandata.sci.gsfc.nasa.gov/SeaWiFS/L2/1998/001", full.names = TRUE, pattern = "*OC$")
library(dplyr)
library(raadtools)
d <- data_frame()

## collate up the day's L2 files (there are 34 but this is an early test)
for (i in seq_along(fs)) {
  s2 <- try(readL2(fs[i]))
  if (!inherits(s2, "try-error")) {
    d <- bind_rows(d, s2)
    print(i)
  }
}
## build a map that we will bin 6e7 obs into (very coarse for now)
r <- raster(extent(-180, 180, -90, 90), nrow = 4320 * 2, ncol = 4320)
d$cell <- cellFromXY(r, as.matrix(d[, c("longitude", "latitude")]))

ds <- d  %>% filter(chlor_a > 0)  %>% filter(chlor_a < 1000)  %>% group_by(cell)  %>% summarize(chlor_a = mean(chlor_a))
r[ds$cell] <- ds$chlor_a
pal <- chl.pal(palette = TRUE)
plot(r, col = pal$cols, breaks = pal$breaks, legend = FALSE)
})


library(rbenchmark)
benchmark(tbl = readL2(fs[10], data.frame = TRUE), 
          st = readL2(fs[1], data.frame = FALSE), replications = 1)





