
# stat value	Measure calculated	Defined at a given bin as:
#   1	Mean(Default)	s/wt
# 2	Variance	tmp_var = (ssq/wt)-(mean**2) 
# var = (tmp_var * wt**2) / ((wt**2)-ns)
# 3	Standard Deviation	sqrt(var)
# 4	Pixels	number of Level-2 input values that went into creating the Level-3 bin selected for each grid point
# 5	Scenes	number of Level-2 input files that went into creating the Level-3 bin selected for each grid point
# 
# 
# 
# flt_val = sum_buf[k*2] / wgt;
# flt_val = (sum_buf[k*2+1] / wgt) - (flt_val*flt_val);
# 
# flt_val = flt_val*wgt*wgt/(wgt*wgt-nscenes);
# 
# if (input.meas == 3) flt_val = sqrt(flt_val);
# 


## see here
## http://seadas.gsfc.nasa.gov/doc/smigen/smigen.html
## http://oceancolor.gsfc.nasa.gov/forum/oceancolor/topic_show.pl?pid=24595;hl=variance

meanL3 <- function(x, varname = "chlor_a", collapse  = FALSE) {
  sumname <- sprintf("%s_sum", varname)
  if (collapse) {
    sum(x[[sumname]])/sum(x$weights)
  } else {
    x[[sumname]]/x$weights
  }
}
varL3 <- function(x, varname = "chlor_a", collapse = FALSE) {
  ssqname <- sprintf("%s_ssq", varname)
  if (collapse) {
    tmp_var <- (sum(x[[ssqname]])/sum(x$weights)) - (meanL3(x, collapse = TRUE)^2)
    div <- sum(x$weights^2) - sum(x$nscenes)
    v <- tmp_var * sum(x$weights^2) / div
  } else {
    tmp_var <- (x[[ssqname]]/x$weights) - (meanL3(x)^2)
    div <- (x$weights^2) - x$nscenes
    v <- (tmp_var * x$weights^2) / ifelse(div > 0, div, 1)
    v[!v > 0] <- 0
  }
  v
}



# d1 <- structure(list(NUMROWS = 2160L, bin_num = 100335L, nobs = 11L, 
#                      nscenes = 7L, weights = 8.56047821044922, chlor_a_sum = 4.77596616744995, 
#                      chlor_a_ssq = 2.98022437095642, filename = structure(1L, .Label = "/rdsi/PRIVATE/data/oceandata.sci.gsfc.nasa.gov/SeaWiFS/L3BIN/1998/001/S19980011998008.L3b_8D_CHL.main", class = "factor")), .Names = c("NUMROWS", 
#                                                                                                                                                                                                                                "bin_num", "nobs", "nscenes", "weights", "chlor_a_sum", "chlor_a_ssq", 
#                                                                                                                                                                                                                                "filename"), row.names = c(NA, -1L), class = "data.frame")
# ##dput(do.call(rbind, lapply(x1, function(x) {test <- x$bin_num == 100335; if(any(test)) as.data.frame(roc:::.subsetL3(x, test), stringsAsFactors = FALSE) else NULL})))
# d2 <- structure(list(NUMROWS = c(2160L, 2160L, 2160L, 2160L, 2160L), 
#                bin_num = c(100335L, 100335L, 100335L, 100335L, 100335L), 
#                nobs = c(3L, 2L, 2L, 3L, 1L), nscenes = c(1L, 2L, 1L, 2L, 
#                                                          1L), weights = c(1.73205077648163, 2, 1.41421353816986, 2.41421365737915, 
#                                                                           1), chlor_a_sum = c(1.28371012210846, 1.27820611000061, 0.911987781524658, 
#                                                                                               0.952443182468414, 0.349619418382645), chlor_a_ssq = c(1.0292010307312, 
#                                                                                                                                                      0.818253993988037, 0.62619948387146, 0.384336292743683, 0.122233740985394
#                                                                                               ), filename = c("/rdsi/PRIVATE/data/oceandata.sci.gsfc.nasa.gov/SeaWiFS/L3BIN/1998/001/S1998001.L3b_DAY_CHL.main", 
#                                                                                                               "/rdsi/PRIVATE/data/oceandata.sci.gsfc.nasa.gov/SeaWiFS/L3BIN/1998/002/S1998002.L3b_DAY_CHL.main", 
#                                                                                                               "/rdsi/PRIVATE/data/oceandata.sci.gsfc.nasa.gov/SeaWiFS/L3BIN/1998/003/S1998003.L3b_DAY_CHL.main", 
#                                                                                                               "/rdsi/PRIVATE/data/oceandata.sci.gsfc.nasa.gov/SeaWiFS/L3BIN/1998/006/S1998006.L3b_DAY_CHL.main", 
#                                                                                                               "/rdsi/PRIVATE/data/oceandata.sci.gsfc.nasa.gov/SeaWiFS/L3BIN/1998/007/S1998007.L3b_DAY_CHL.main"
#                                                                                               )), .Names = c("NUMROWS", "bin_num", "nobs", "nscenes", "weights", 
#                                                                                                              "chlor_a_sum", "chlor_a_ssq", "filename"), row.names = c(NA, 
#                                                                                                                                                                       5L), class = "data.frame")
# 

# 
# 
# scl <- function (x) 
#   (x - min(na.omit(x)))/diff(range(na.omit(x)))
# 
# 
# d1$chlor_a_sum
# 
# 
# roc:::.subsetL3(x2, x2$bin_num == 100335)
# $NUMROWS
# [1] 2160
# 
# $bin_num
# [1] 100335
# 
# $nobs
# [1] 11
# 
# $nscenes
# [1] 7
# 
# $weights
# [1] 8.560478
# 
# $chlor_a_sum
# [1] 4.775966
# 
# $chlor_a_ssq
# [1] 2.980224
# 
# $filename
# [1] "/rdsi/PRIVATE/data/oceandata.sci.gsfc.nasa.gov/SeaWiFS/L3BIN/1998/001/S19980011998008.L3b_8D_CHL.main"
# 
#  lapply(x1, function(x) roc:::.subsetL3(x, x$bin_num == 100335))
# [[1]]
# [[1]]$NUMROWS
# [1] 2160
# 
# [[1]]$bin_num
# [1] 100335
# 
# [[1]]$nobs
# [1] 3
# 
# [[1]]$nscenes
# [1] 1
# 
# [[1]]$weights
# [1] 1.732051
# 
# [[1]]$chlor_a_sum
# [1] 1.28371
# 
# [[1]]$chlor_a_ssq
# [1] 1.029201
# 
# [[1]]$filename
# [1] "/rdsi/PRIVATE/data/oceandata.sci.gsfc.nasa.gov/SeaWiFS/L3BIN/1998/001/S1998001.L3b_DAY_CHL.main"
# 
# 
# [[2]]
# [[2]]$NUMROWS
# [1] 2160
# 
# [[2]]$bin_num
# [1] 100335
# 
# [[2]]$nobs
# [1] 2
# 
# [[2]]$nscenes
# [1] 2
# 
# [[2]]$weights
# [1] 2
# 
# [[2]]$chlor_a_sum
# [1] 1.278206
# 
# [[2]]$chlor_a_ssq
# [1] 0.818254
# 
# [[2]]$filename
# [1] "/rdsi/PRIVATE/data/oceandata.sci.gsfc.nasa.gov/SeaWiFS/L3BIN/1998/002/S1998002.L3b_DAY_CHL.main"
# 
# 
# [[3]]
# [[3]]$NUMROWS
# [1] 2160
# 
# [[3]]$bin_num
# [1] 100335
# 
# [[3]]$nobs
# [1] 2
# 
# [[3]]$nscenes
# [1] 1
# 
# [[3]]$weights
# [1] 1.414214
# 
# [[3]]$chlor_a_sum
# [1] 0.9119878
# 
# [[3]]$chlor_a_ssq
# [1] 0.6261995
# 
# [[3]]$filename
# [1] "/rdsi/PRIVATE/data/oceandata.sci.gsfc.nasa.gov/SeaWiFS/L3BIN/1998/003/S1998003.L3b_DAY_CHL.main"
# 
# 
# 
# 
# [[6]]
# [[6]]$NUMROWS
# [1] 2160
# 
# [[6]]$bin_num
# [1] 100335
# 
# [[6]]$nobs
# [1] 3
# 
# [[6]]$nscenes
# [1] 2
# 
# [[6]]$weights
# [1] 2.414214
# 
# [[6]]$chlor_a_sum
# [1] 0.9524432
# 
# [[6]]$chlor_a_ssq
# [1] 0.3843363
# 
# [[6]]$filename
# [1] "/rdsi/PRIVATE/data/oceandata.sci.gsfc.nasa.gov/SeaWiFS/L3BIN/1998/006/S1998006.L3b_DAY_CHL.main"
# 
# 
# [[7]]
# [[7]]$NUMROWS
# [1] 2160
# 
# [[7]]$bin_num
# [1] 100335
# 
# [[7]]$nobs
# [1] 1
# 
# [[7]]$nscenes
# [1] 1
# 
# [[7]]$weights
# [1] 1
# 
# [[7]]$chlor_a_sum
# [1] 0.3496194
# 
# [[7]]$chlor_a_ssq
# [1] 0.1222337
# 
# [[7]]$filename
# [1] "/rdsi/PRIVATE/data/oceandata.sci.gsfc.nasa.gov/SeaWiFS/L3BIN/1998/007/S1998007.L3b_DAY_CHL.main"
# 
# 
