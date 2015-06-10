roc
=====

R package to work with ocean colour data. 


Use rrshdf4 to read variables from any Level-3 binned file (only SeaWiFS and 
MODISA extensively tested). 


```{r}
devtools::install_github("mdsumner/roc")
```

If you need to actually read the legacy HDF4 files, set your system up right and

```{r}
devtools::install_github("AustralianAntarcticDivision/rrshdf4")
```

NASA has moved to a NetCDF4 format using groups and compound types within files. Most people have no trouble with one of these things. 

There are helper functions to generate longitude / latitude coordinates for the bins (centre or corners). These bins
are a sparse, non-rectangular, grid on a Sinusoidal projection, see 

http://oceandata.sci.gsfc.nasa.gov/

http://oceancolor.gsfc.nasa.gov/SeaWiFS/TECH_REPORTS/PreLPDF/PreLVol32.pdf

Package is built using roxygen2 and Rcpp. Only tested on Linux for now - help welcome to port to Windows. 

Basic usage
====

Get a Level-3 bin file  (this one is 15 Mb) of ocean colour, and read all bins. 

```{r}
f <- "http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A2014208.L3b_DAY_KD490.main.bz2"
download.file(f, basename(f), mode = "wb")
library(rrshdf4)
system(sprintf("bunzip2 %s", basename(f)))
x <- readL3(gsub(".bz2", "", basename(f)))
List of 8
 $ NUMROWS   : int 4320
 $ bin_num   : int [1:1537578] 3000338 3006072 3006080 3006081 3006082 3011818 3011827 3011828 3011829 3011830 ...
 $ nobs      : int [1:1537578] 1 1 2 4 1 2 7 8 6 4 ...
 $ nscenes   : int [1:1537578] 1 1 1 1 1 1 1 1 1 1 ...
 $ weights   : num [1:1537578] 1 1 1.41 2 1 ...
 $ Kd_490_sum: num [1:1537578] 0.0566 0.0622 0.0832 0.11 0.0504 ...
 $ Kd_490_ssq: num [1:1537578] 0.0032 0.00387 0.00489 0.00608 0.00254 ...
 $ filename  : chr "A2014208.L3b_DAY_KD490.main"
 
 library(roc)
xy <- bin2lonlat(x$bin_num)
```

