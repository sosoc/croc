#' summarize ocean colour from L3 bins
#' raadtools produces daily extractions of Johnson/NASA chlophyll from L3-bins
#' here we focus on a specific region, specific months
#'  load up all values, compute statistics for mean/variance/n grouped by
#'   month, unique bin (L3 bins, MODISA)
#'  

library(raadtools)
library(roc)
library(tibble)  ## better than data.frame, do it
library(dplyr)
files <- chla_johnsonfiles() %>% dplyr::filter(format(date, "%m") %in% c("12", "01", "02"))
xlim <- c(55, 100)
ylim <- c(-70, -45)
NROWS <- 4320
domain_raster <- raster(extent(xlim, ylim), crs = "+init=epsg:4326" )
init <- initbin(NUMROWS = NROWS)
rowbins <- seq(init$basebin[findInterval(ylim[1], init$latbin)], 
               init$basebin[findInterval(ylim[2], init$latbin) + 1])
xybin <- as_tibble(bin2lonlat(rowbins,
                  nrows = NROWS)) %>%
       dplyr::mutate(bin_num = rowbins) %>% 
  dplyr::filter(x >= xlim[1], x <= xlim[2])

bins0 <- dplyr::select(xybin, "bin_num")

dim(files)
library(future)
read1 <- function(ifile) {readRDS(ifile) %>% inner_join(bins0, "bin_num")}
dd <- purrr::map(files$fullname, read1) %>% dplyr::bind_rows()


dd <- purrr::map(files$fullname, 
           function(ifile) {readRDS(ifile) %>% inner_join(bins0, "bin_num")}
) %>% dplyr::bind_rows() 


## output folder
#dp <- "/perm_storage/data/k-axis-ocean"
dp <- "~"
saveRDS(dd, file = file.path(dp, "k-axis_modis_chla2.rds"), compress = FALSE)

## statistical functions to summarize, so we can later
## compute variance (not just mean/n)
#funs <- list(sum = sum, ssq = function(x) sum(x^2), n = length)

funs <- list(mean = mean, var = var, n = length)
dmonth <- dd %>%   mutate(month = format(date, "%B")) %>% 
    group_by(bin_num, month) %>% 
  summarize_if(purrr::is_bare_numeric, funs)

saveRDS(dmonth, file.path(dp, "k-axis_modis_chla_month.rds"), compress = FALSE)

#' In-dev function ...
bin_chl <- function(bins, value, gridmap, platform = "MODISA") {
  bins <- tibble(bin_num = bins, value = value)
  if (!platform == "MODISA") stop("only MODISA  platform currently supported")
  if (missing(gridmap)) { 
    
    gridmap <- raster(extent(-180, 180, -90, 0), ncol = 8640, nrow = 2160, crs = "+init=epsg:4326")
    
    ll <- coordinates(gridmap)
    bins <- tibble(bin_num = lonlat2bin(ll[,1], ll[, 2], NUMROWS = 4320), gridcell = seq(ncell(gridmap))) %>% inner_join(bins, "bin_num")
    gridmap[bins$gridcell] <- bins$value
  } else {
    #roc:::regridder
  }
  gridmap
}

## 
nasa_dec <- with(dmonth %>% filter(month == "December"), 
  bin_chl(bin_num, chla_nasa_mean))
nasa_jan <- with(dmonth %>% filter(month == "January"), 
                 bin_chl(bin_num, chla_nasa_mean))
nasa_feb <- with(dmonth %>% filter(month == "February"), 
                 bin_chl(bin_num, chla_nasa_mean))

rj_dec <- with(dmonth %>% filter(month == "December"), 
                 bin_chl(bin_num, chla_johnson_mean))
rj_jan <- with(dmonth %>% filter(month == "January"), 
                 bin_chl(bin_num, chla_johnson_mean))
rj_feb <- with(dmonth %>% filter(month == "February"), 
                 bin_chl(bin_num, chla_johnson_mean))


nasa_dec_sd <- with(dmonth %>%  filter(month == "December"), 
                    bin_chl(bin_num, sqrt(chla_nasa_var)))



# u <- "https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20023352016366.L3m_MC_CHL_chl_ocx_4km.nc"
# nf <- file.path(dp, basename(u)) 
# download.file(u, nf, mode = "wb")
# bc <- raster(nf)
