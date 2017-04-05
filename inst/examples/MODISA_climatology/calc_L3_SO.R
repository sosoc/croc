library(raadtools)
library(roc)
library(tibble)
library(dplyr)
## data frame of all L3 RRS files for MODISA
files <- ocfiles(time.resolution = "daily", product = "MODISA", varname = "RRS", type = "L3b", bz2.rm = TRUE, ext = "nc") %>% 
  as_tibble() %>% transmute(date, file = basename(fullname), fullname)

## use only complete summer seasons
files <- files %>% mutate(year=as.integer(format(date,"%Y")),month=as.integer(format(date,"%m")),season=year-(month<=6), doy = as.integer(format(date, "%j"))) %>%
  filter(doy >=355 | doy <=80 ) %>% 
  group_by(season) %>% mutate(n = n()) %>% ungroup() %>% filter(n >= 90)


## hacked function to read just the RRS we need
read_L3_file <- function(file, compound_vars = NULL) {
  info <- rhdf5::h5ls(file)
  tab <- table(info$dim); wm <- which.max(tab); test <- names(tab[wm])
  ## get all vars, or just the ones the users wants
  if (is.null(compound_vars))  {
    compound <- info$name[info$dim == test]
  } else {
    compound <- compound_vars
  }
  compoundpath <- file.path("/level-3_binned_data", compound)
  l <- lapply(compoundpath, function(aname) tibble::as_tibble(rhdf5::h5read(file, name = aname)))
  ## not very generalized but ok for L3 RRS
  d <- tibble::as_tibble(rhdf5::h5read(file, name = file.path("/level-3_binned_data", "BinList")))
  d2 <- dplyr::bind_cols(lapply(seq_along(compound), function(i) setNames(l[[i]], sprintf("%s_%s", compound[i], names(l[[i]])))))
  dplyr::bind_cols(d, d2)
}

## initialize the bin logic for MODISA
init <- initbin(NUMROWS = 4320)
## counts up from the south
maxbin <- init$totbin/2
## accumulate sum and count for mean
count_rj <- cumul_rj <- numeric(maxbin)
count_nasa <- cumul_nasa <- numeric(maxbin)
for (ifile in seq(nrow(files))) {
  d <- read_L3_file(files$fullname[ifile], compound_vars = c("Rrs_443", "Rrs_488", "Rrs_555", "Rrs_547")) %>% 
    filter(bin_num <= maxbin) %>% 
    mutate(chla_johnson = chla(., sensor = "MODISA", algo = "johnson")) %>% 
    mutate(chla_nasa = chla(., sensor = "MODISA", algo = "oceancolor")) %>% 
    dplyr::select(bin_num, chla_johnson, chla_nasa) %>% filter(chla_johnson > 0)
  
  ## we accumulate outside of the data frame to ensure speed
  ## the bin_num indexes the vector structure directly since the south pole is bin 1
  count_rj[d$bin_num] <- count_rj[d$bin_num] + 1
  cumul_rj[d$bin_num] <- cumul_rj[d$bin_num] + d$chla_johnson
  
  count_nasa[d$bin_num] <- count_nasa[d$bin_num] + 1
  cumul_nasa[d$bin_num] <- cumul_nasa[d$bin_num] + d$chla_nasa
  
  #if (ifile > 10) break;
  if (ifile %% 100 == 0) print(ifile)
}

result <- tibble(bin_num = seq_len(maxbin), count_nasa = count_nasa, cumul_nasa = cumul_nasa, 
                 count_rj = count_rj, cumul_rj = cumul_rj) %>% filter(count_rj > 0)


#dir.create("/rdsi/PRIVATE/raad/dev/Quantarctica")
#dir.create("/rdsi/PRIVATE/raad/dev/Quantarctica/MODISA_Johnson_chla/")

library(feather)
write_feather(result, "/rdsi/PRIVATE/raad/dev/Quantarctica/MODISA_Johnson_chla/MODISA_SO_summer_355_080.feather")


# f <- "https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/V20120022016091.L3m_CU_SNPP_CHL_chlor_a_9km.nc"
# download.file(f, basename(f), mode = "wb")
# mod <- crop(raster(basename(f)), extent(-180, 180, -90,0))
# pmod <- projectRaster(mod, map)
# #extent(mod) <- extent(-180, 180, -90, 90)
# #projection(mod) <- "+proj=longlat +ellps=WGS84"
# par(mar = rep(0, 4))
# plot(setNames(brick(pmod, map), c("NASA", "Johnson")), col =pal$cols, breaks = pal$breaks, 
#      axes = FALSE, legend = FALSE)
# 


