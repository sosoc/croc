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
  bad <- values(s1[[3]]) < 0
  if (data.frame) s1 <- as.data.frame(values(s1)[!bad, ])
  s1
}

x <-  "/rdsi/PRIVATE/data/oceandata.sci.gsfc.nasa.gov/SeaWiFS/L2/1998/001/./S1998001013310.L2_GAC_OC"

s2 <- readL2(x)
head(s2)
with(subset(s2, chlor_a < 1000 & chlor_a > 0), plot(longitude, latitude, pch = ".", col = chl.pal(chlor_a)))

d <- data_frame()

for (i in seq_along(fs)) {
  s2 <- readL2(fs[i]); 
  d <- rbind(d, data_frame(s2))
  ##with(subset(s2, chlor_a < 1000 & chlor_a > 0), points(longitude, latitude, pch = ".", col = chl.pal(chlor_a)))
  print(i)
  }

