library(raadtools)
library(roc)
library(tibble)
library(dplyr)
## data frame of all L3 RRS files for MODISA
files <- ocfiles(time.resolution = "daily", product = "MODISA", varname = "RRS", type = "L3b", bz2.rm = TRUE, ext = "nc") %>% 
  as_tibble() %>% transmute(date, file = basename(fullname), fullname)
#files %>% slice(c(1, nrow(files)))

## filter to the NASA "summer" definition (I can't find this anymore ...)
##format(as.Date("2002-12-21"), "%j")
files <- files %>% mutate(doy = as.integer(format(date, "%j"))) %>% filter(doy <= 80 | doy >= 355)

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
count_bins <- cumul_bins <- numeric(maxbin)

for (ifile in seq(nrow(files))) {
  d <- read_L3_file(files$fullname[ifile], compound_vars = c("Rrs_443", "Rrs_488", "Rrs_555")) %>% 
    filter(bin_num <= maxbin) %>% mutate(chl = chla(., sensor = "MODISA", algo = "johnson")) %>% 
    dplyr::select(bin_num, chl) %>% filter(chl > 0)
  
  count_bins[d$bin_num] <- count_bins[d$bin_num] + 1
  cumul_bins[d$bin_num] <- cumul_bins[d$bin_num] + d$chl
  #if (ifile > 10) break;
  if (ifile %% 100 == 0) print(ifile)
}

result <- tibble(bin_num = seq_len(maxbin), count = count_bins, sum = cumul_bins) %>% filter(count > 0)

library(feather)
write_feather(result, "MODISA_SO_summer_355_080.feather")



