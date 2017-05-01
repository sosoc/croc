u <- "https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/S2008001.L3b_DAY_CHL.nc"
download.file(u, file.path("inst/extdata/ocfiles", basename(u), mode = "wb"))
u <- "https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/S2008001.L3b_DAY_RRS.nc"
download.file(u, file.path("inst/extdata/ocfiles", basename(u), mode = "wb"))
