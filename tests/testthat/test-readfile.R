context("Basic read")

##datadir <- getOption("default.datadir")
 
##datadir <- "/rdsi/PRIVATE"
##ftest <- file.path(datadir, "oceandata.sci.gsfc.nasa.gov/SeaWiFS/L3BIN/1998/005/S1998005.L3b_DAY_CHL.main")
ftest <- system.file("extdata", "S2008001.L3b_DAY_CHL.main", package = "roc")

 test_that("file read is successful", {
   expect_that(readL3(ftest),is_a("list"))
 })
 
