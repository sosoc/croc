
.nasadateformat <- function(x, f = "%Y%j") {
  format(x, f)
}

#' Read NASA ocean colour browser images (PNG format)
#' 
#' Read ocean colour imagery from the NASA site, files will be downloaded and read in with
#' \code{brick}. If a file with an identical name is already present in the current working directory the download
#' is skipped. 
#' @param date date of image to find
#' @param platform which satellite platform (currently MODISA and SeaWiFS)
#' @param tres temporal resolution to read
#' @param varname name of variable (currently chlorophyll-a)
#' @importFrom raster brick projection "extent<-" "projection<-"
#' @export
readL3img <- function(date, platform = c(A = "MODISA", S = "SeaWiFS"), 
                      tres = c("DAY" = "", "8D" = "8 days", "MO" = "1 month", "YR" = "1 year"), 
                               varname = c(CHL = "chlor_a")) {
  if (length(date) > 1L) warning("only the first input date will be used")
  date <- as.POSIXct(date[1L], tz = "GMT")
  ubase <- "http://oceancolor.gsfc.nasa.gov/cgi/l3"
  platform <- match.arg(platform)
  suffix <- "sub=img"
  fdate <- .nasadateformat(date)
  tres <- match.arg(tres)
  sres <- c(A = "9km", S = "4km")[names(platform)]
  edate <- if (nchar(tres) < 1) "" else .nasadateformat(seq(date, length = 2, by = tres)[2L] - 24 * 3600)
  varname <- match.arg(varname)
#   print(platform)
#   print(fdate)
#   print(edate)
#   print(tres)
#   print(varname) 
#   print(sres)       #1#2#3     #4 #5 #6 #7     #8   
   fname <- sprintf("%s%s%s.L3m_%s_%s_%s_%s.png?%s",
                   names(platform), #1
                   fdate,   #2
                   edate,   #3
                   names(tres),     #4
                   names(varname),  #5
                   varname,         #6
                   sres,            #7
                   suffix)          #8
  #print(fname)
  #print("S20090602009090.L3m_MO_CHL_chlor_a_9km.png?sub=img")
  

qurl <- file.path(ubase, fname)
tfile <- file.path(tempdir(), gsub("?sub-img", "", fname))
print(qurl)
print(tfile)
if (!file.exists(tfile)) download.file(qurl, tfile, mode = "wb")

 b <-  brick(tfile)
 extent(b) <- extent(-180, 180, -90, 90)
 projection(b) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
b
 }


##readL3img("2009-03-01")
##b <- readL3img("2007-01-01", tres = "1 year")

