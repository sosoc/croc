roc
=====

R package to work with ocean colour data. Read all variables from any Level-3 binned file (only SeaWiFS and 
MODISA extensively tested). 

There are helper functions to generate longitude / latitude coordinates for the bins (centre or corners). These bins
are a sparse, non-rectangular, grid on a Sinusoidal projection, see 

http://oceandata.sci.gsfc.nasa.gov/

http://oceancolor.gsfc.nasa.gov/SeaWiFS/TECH_REPORTS/PreLPDF/PreLVol32.pdf

Package is built using roxygen2 and Rcpp. Only tested on Linux for now - help welcome to port to Windows. 

Limitations
====
When the x00, x01, etc files are required they *must* be present next to the main file. This package does no checks for this (yet). If they are not present, the relevant variables will be populated with zero values but this otherwise does not affect functionality. 

Basic usage
====

Get a Level-3 bin file  (this one is 15 Mb) of ocean colour, and read all bins. 

```{r}
f <- "http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A2014208.L3b_DAY_KD490.main.bz2"
download.file(f, basename(f), mode = "wb")
library(roc)
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
 
xy <- bin2lonlat(x$bin_num)
```

Default behaviour is to read both sum/ssq of *all* variables. You can limit to just some variables with the *vname* argument. (This can be important for the RRS files which contain many Remote Sensing Reflectance variables. 

TODO
====

- Ability to read sum and/or ssq for individual variables. 
- Distinction between grid index data and variables without hardcoded names. 
- Abstract away the need for explicit decompressing of the file. 
- Build in a "getfile" feature to populate local repository. 
- Extend capacity to deal with bins spatially, including
-- aggregation to larger bins
-- indexing for arbitrary binning to raster or polygon

Dependencies
====

These are the complete steps to install from a basic Ubuntu system.  

```{bash}
## 1) Update and install R

## using Nectar image "NeCTAR Ubuntu 14.04 (Trusty) amd64"
## key for apt-get update, see http://cran.r-project.org/bin/linux/ubuntu/README
sudo chown ubuntu /etc/apt/sources.list
sudo echo 'deb http://cran.csiro.au/bin/linux/ubuntu trusty/' >> /etc/apt/sources.list
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
sudo apt-get update && sudo apt-get upgrade -y
sudo apt-get install -y r-base r-base-dev 

## 2) Install R package dependencies
## brew, digest, Rcpp stringr, and testthat packages
## lib = "/usr/lib/R/site-library"
sudo Rscript -e 'x <- .libPaths();install.packages(c("roxygen2", "testthat", "Rcpp"), lib = x[length(x)-1], repos = "http://cran.csiro.au")'

## 3) Install 3rd party HDF 

## not sure if both required?
sudo apt-get install -y libhdf4-dev
sudo apt-get install -y libhdf4g-dev
sudo apt-get install git -y

## 4) Get the package source and install
git clone https://github.com/mdsumner/roc.git

### 4a) roxygenize
### Rscript -e 'roxygen2::roxygenize("roc")'
### not sure if this is a bug in roxygen2, but ensure methods is loaded first
Rscript -e 'library(methods);roxygen2::roxygenize("roc")'

### 4b) Rcpp attributes (wraps the C++ source for R functions and doc)
Rscript -e 'Rcpp::compileAttributes("roc")'

### 4c) build and install
## check where your hdf.h is, ensure this corresponds to roc/src/Makevars and
export CPATH=/usr/include/hdf
R CMD build roc

echo 'CPATH=/usr/include/hdf' >> .Renviron


=======
##Rscript -e 'roxygen2::roxygenize("roc")'
## not sure if this is a bug in roxygen2, but ensure methods is loaded first
Rscript -e 'library(methods);roxygen2::roxygenize("roc")'
## check where your hdf.h is, ensure this corresponds to roc/src/Makevars and
export CPATH=/usr/include/hdf
R CMD build roc
sudo R CMD INSTALL roc_0.0-4.tar.gz --library=/usr/lib/R/site-library

## 5) Obtain an example file and test it out
wget http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/S2004001.L3b_DAY_KD490.main.bz2
bunzip2 S2004001.L3b_DAY_KD490.main.bz2
Rscript -e 'library(roc);str(readL3("S2004001.L3b_DAY_KD490.main"))'
## List of 8
##  $ NUMROWS   : int 2160
## $ bin_num   : int [1:358121] 73217 74173 74181 74183 75135 75136 75144 75145 75146 75216 ...
## $ nobs      : int [1:358121] 1 1 1 1 2 2 1 2 2 1 ...
## $ nscenes   : int [1:358121] 1 1 1 1 1 1 1 1 1 1 ...
## $ weights   : num [1:358121] 1 1 1 1 1.41 ...
## $ Kd_490_sum: num [1:358121] 0.111 0.154 0.169 0.14 0.25 ...
## $ Kd_490_ssq: num [1:358121] 0.0124 0.0238 0.0284 0.0197 0.0448 ...
## $ filename  : chr "S2004001.L3b_DAY_KD490.main"
```

