roc
=====

R package to work with ocean colour data. Read all variables from any Level-3 binned file (only SeaWiFS and 
MODISA extensively tested). 

There are helper functions to generate longitude / latitude coordinates for the bins (centre or corners). These bins
are a sparse grid on Sinusoidal projection, see 

http://oceandata.sci.gsfc.nasa.gov/

http://oceancolor.gsfc.nasa.gov/SeaWiFS/TECH_REPORTS/PreLPDF/PreLVol32.pdf

Package is built using roxygen2 and Rcpp. Only tested on Linux for now - help welcome to port to Windows. 

Limitations
====
When the x00, x01, etc files are required they *must* be present next to the main file. This package does no checks for this (yet). 

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

Bindings to the HDF4 library are built with Rcpp: 

You'll need at least 

- command line access, e.g. 

```{r} 
system(" hdp dumpvd")
```

- access to the HDF4 source, e.g. 
```{bash}
#include "hdf.h"
```


Build notes for HDF4
=====
I originally used these notes as a guide, building a chain of tools ultimate for use in R - but for roc
you only need HDF4 in your system. 

http://scigeo.org/articles/howto-install-latest-geospatial-software-on-linux.html

```{bash}
## dependencies (on CentOS)
## sudo yum install libjpeg-devel bison flex ## ~3Mb

## HDF4
# download latest release
# check here: http://www.hdfgroup.org/ftp/HDF/HDF_Current/src/
wget http://www.hdfgroup.org/ftp/HDF/HDF_Current/src/[hdf4].tar.gz
tar xvfz [hdf4].tar.gz

## I edited the hlimits.h before compiling, see 
## https://trac.osgeo.org/gdal/wiki/HDF
##cd [hdf4]
## grep MAXFILE hdf/src/hlimits.h


# set compile options
# --disable-netcdf and --disable-fortran are necessary
# when compiling netcdf with hdf4 support
./configure \
  --prefix=/opt/source/$hdf4/build \
  --with-zlib \
  --with-jpeg \
  --enable-shared \
  --disable-netcdf \
  --disable-fortran

# compile
make 
# check build (should all pass)
make check
# install into build dir
make install
make install-examples
# check install
make installcheck

## I added this script to /etc/profile.d to get access to local/lib
##cat /etc/profile.d/custom_exports.sh
export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
sudo ldconfig

```

