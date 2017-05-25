
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/mdsumner/roc.svg?branch=master)](https://travis-ci.org/mdsumner/roc) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mdsumner/roc?branch=master&svg=true)](https://ci.appveyor.com/project/mdsumner/roc)

roc
===

The goal of roc is to allow R users to easily work with ocean colour data.

Installation
------------

You can install roc from github with:

``` r
# install.packages("devtools")
devtools::install_github("mdsumner/roc")
```

Example
-------

This is a basic example to read a L3bin file and map it.

``` r
## basic example code
```

There are helper functions to generate longitude / latitude coordinates for the bins (centre or corners). These bins are a sparse, non-rectangular, grid on a Sinusoidal projection, see

<http://oceandata.sci.gsfc.nasa.gov/>

<http://oceancolor.gsfc.nasa.gov/SeaWiFS/TECH_REPORTS/PreLPDF/PreLVol32.pdf>

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

Note
====

An earlier version of this package imported from <https://github.com/AustralianAntarcticDivision/rrshdf4> to read the old HDF4 L3bin format. This is no longer necessary, and read support is provided for the NetCDF 4.0 (HDF5) version of the files from oceandata.
