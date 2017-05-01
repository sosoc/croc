# This is all L1B. I get it all from ladsweb.nascom.nasa.gov. The orig files are of this kind of nomenclature:
#   
#   MOD021KM.A2012001.0000.*.hdf
# 
# MOD = Terra, MYD = Aqua MODIS. I used both. MOD/MYD02 = Level 1B, all 36 channels, roughly calibrated. For Sara's 
# project I'd have used Ch 01 (for the visible pics) and either Ch 31 or 32 (for the thermal IR pics). The filenames 
# might give you a clue as to whether I used 31 or 32. The "1KM" indicates 1 km resolution. Some subsets of channels are 
# available at higher resolutions but, mercifully, Sara was happy with 1 km resolution (otherwise the data volume would 
# have been redonkulous).The "A2012001" indicates the year and DOY of acquisition. And the 0000 is the UT time of acquisition. 

fs <- c("ftp://ladsweb.nascom.nasa.gov/allData/6/MOD021KM/2015/091/MOD021KM.A2015091.0010.006.2015091134552.hdf", 
"ftp://ladsweb.nascom.nasa.gov/allData/6/MOD35_L2/2015/091/MOD35_L2.A2015091.0010.006.2015091134916.hdf")



u2f <- function(u) {
  gsub("ftp://", "/rdsi/PRIVATE/data/", u)
}


for (i in seq_along(fs)) {
locfile <- u2f(fs[i])
if (!file.exists(locfile)) {
  if (!file.exists(dirname(locfile))) dir.create(dirname(locfile), recursive = TRUE)
  e <- try(download.file(fs[i], locfile, mode = "wb", method ="wget"))
}
}