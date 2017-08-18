we_are_raady <- function() {
  fp <- getOption("default.datadir")
  #print(fp)
  stat <- FALSE
  if (!is.null(fp) && file.exists(file.path(fp, "data"))) stat <- TRUE
  stat
}


# 
# 
# 
# 
# f <- "http://oceandata.sci.gsfc.nasa.gov/search/file_search.cgi?search=&subID=&sdate=2014-11-01&edate=2014-11-05&dtype=L3b&sensor=all&std_only=1&results_as_file=1&.state=Search&.cgifields=results_as_file&.cgifields=dtype&.cgifields=addurl&.cgifields=std_only&.cgifields=sensor&.cgifields=cksum"
# 
# f <- "http://oceandata.sci.gsfc.nasa.gov/search/file_search.cgi?search=&subID=&sdate=2014-11-01&edate=2014-11-05&dtype=L3b&sensor=all&std_only=1&results_as_file=1&.state=Search&.cgifields=results_as_file&.cgifields=dtype&.cgifields=addurl&.cgifields=std_only&.cgifields=sensor&.cgifields=cksum"
# 
# f <- "http://oceandata.sci.gsfc.nasa.gov/search/file_search.cgi?search=&subID=&sdate=&edate=&dtype=L3b&sensor=czcs&std_only=1&results_as_file=1&.state=Search&.cgifields=results_as_file&.cgifields=dtype&.cgifields=addurl&.cgifields=std_only&.cgifields=sensor&.cgifields=cksum"

## here we should collect any code for file management and downloading
## total volume of uncompressed L3b_DAY_RRS is ~1Tba (2014-11-04)
.filetok <- function(x) {
  x <- basename(x)
  sensortok <- substr(x, 1, 1)
  yeartok <- substr(x, 2, 5)
  jdaytok <- substr(x, 6, 8)
  ## Note: Aquarius is *versioned* so we need some extra handling here
  ## or we'll just smash them all together (might be ok since the files have the version name)
  
  list(sensor = sensortok, year = yeartok, jday = jdaytok)
}
.filesensor <- function(x) {
  x1 <- .filetok(x)
  c(A = "MODISA", C = "CZCS", O = "OCTS", M = "MERIS", 
                          Q = "Aquarius", 
                          S = "SeaWiFS", 
                          T = "MODIST",
                          V = "VIIRS")[x1[["sensor"]]]  
}
.fileloc <- function(x){
  x1 <- .filetok(x)
  ## HICO: L1 only
  ## MERIS: has x00, etc. auxiliarly files
  ## MODISA: has x00, etc. 
  
 
  file.path(.filesensor(x), "L3BIN", x1[["year"]], x1[["jday"]])
}

# 
# fs <- readLines(f)
# .fileloc(fs[-c(1, 2)])



# 
# allfiles <- readLines(file.path(getOption("default.datadir"), "admin", "filelist", "allfiles.txt"))
# logi <- grepl("^S", basename(allfiles)) & grepl("L3b_DAY_RRS", basename(allfiles))
# afiles <- allfiles[logi]
# i <- 1
# 
# l <- vector("list", length(afiles))
# for (i in seq_along(afiles)) {
#   l[[i]] <- names(roc:::vdatainfo(afiles[i]))
# }
# 
# ## um, they are all the same: 
# unique(sapply(l, paste, collapse = ","))
##[1] "SEAGrid,BinList,angstrom,aot_865,Rrs_412,Rrs_443,Rrs_490,Rrs_510,Rrs_555,Rrs_670,BinIndex"

# getroot <- "http://oceandata.sci.gsfc.nasa.gov/cgi/getfile"
# 
# ## seasonal cumulative SeaWiFs
# f1 <- "S19973552010079.L3b_SCWI_RRS.main.bz2"
# f2 <- "S19973552010079.L3b_SCWI_CHL.main.bz2"
# 
# f1 <- f2
# 
# dataroot <- file.path(getOption("default.datadir"), "data", "oceandata.sci.gsfc.nasa.gov")
# localfile <- file.path(dataroot, .fileloc(f1), f1)
# remotefile <- file.path(getroot, f1)
# if (!file.exists(dirname(localfile))) dir.create(dirname(localfile), recursive = TRUE)
# download.file(remotefile, localfile, mode = "wb")
# 
# library(roc)
# d <- readL3(localfile)
# 
# 
# ## please fix
# d$Rrs_488_sum <- d$Rrs_490_sum
# d$Rrs_488_ssq <- d$Rrs_490_ssq
# d$oc <- chla(d, sensor = "SeaWiFS", algo = "oceancolor")
# 
# init0 <- initbin(2160)
# chlor_a <- numeric(init0$totbins)
# weights <- chlor_a
# 
# dates <- seq(as.Date("1997-355", "%Y-%j"), as.Date("2010-079", "%Y-%j"), by = "1 day")
# dates <- dates[as.integer(format(dates, "%j")) >= 355 | as.integer(format(dates, "%j")) <= 79]
# dataroot <- file.path(getOption("default.datadir"), "data", "oceandata.sci.gsfc.nasa.gov")
# 
# 
# logi <- logical(length(dates))
# for (i in seq_along(logi)) {
#   fname <- sprintf("S%s%s.L3b_DAY_CHL.main", format(dates[i], "%Y"), format(dates[i], "%j"))
#   localfile <- file.path(dataroot, .fileloc(fname), fname)
#   ##logi[i] <- file.exists(localfile)
#   d <- try(readL3(localfile))
#   if (!inherits(d, "try-error")) {
#     chlor_a[d$bin_num] <- chlor_a[d$bin_num] + d$chlor_a_sum
#     weights[d$bin_num] <- weights[d$bin_num] + d$weights
#     logi[i] <- TRUE 
#   }
#   if (i %% 100 == 0) print(i)
# }
# 
# 
# ## so all these dates not available
# baddates <- dates[!logi]
# ok <- logical(length(baddates))
# for (i in seq_along(ok)) {
#   fname <- sprintf("S%s%s.L3b_DAY_CHL.main.bz2", format(baddates[i], "%Y"), format(baddates[i], "%j"))
#   localfile <- file.path(dataroot, .fileloc(fname), fname)
#   test <- try(download.file(file.path(getroot, fname), localfile, mode = "wb"))
#   ok[i] <- !inherits(test, "try-error")
# }
# 
