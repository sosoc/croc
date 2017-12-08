# croc 0.0.9

* updated web links for ocean colour site to https

* added a c (renamed package) to avoid BioConductor/CRAN clash

* remove some C (Rcpp function bin2lonlat) in favour of R vectorized version

* new functions `read_binlist` and `read_compound` to provide support for
 L3 bin readers
 
* removed dependence on rgdal

* removed dependence on raadtools

* removed regrid, to be replaced 

* use tibble not data_frame

* roc now imports Bioconductor package `rhdf5` for the L3 bin files

* removed dependency on `rrshdf4`

# roc 0.0.8

* roc is now standalone from the HDF4 library, those read functions are in rrshdf4
 