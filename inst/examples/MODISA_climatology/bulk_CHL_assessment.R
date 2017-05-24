

library(raadtools)
library(roc)
library(tibble)
library(dplyr)
## data frame of all L3 RRS files for MODISA
modisfiles <- ocfiles(time.resolution = "daily", product = "MODISA", varname = "RRS", type = "L3b", bz2.rm = TRUE, ext = "nc") %>% 
  as_tibble() %>% transmute(date, file = basename(fullname), fullname)
seawifsfiles <- ocfiles(time.resolution = "daily", product = "SeaWiFS", varname = "RRS", type = "L3b", bz2.rm = TRUE, ext = "nc") %>% 
  as_tibble() %>% transmute(date, file = basename(fullname), fullname)



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

sub_l3 <- function(file) {
  read_L3_file(file, compound_vars = c("Rrs_443", "Rrs_488", "Rrs_555", "Rrs_547")) %>% 
  filter(bin_num <= maxbin) %>% 
  mutate(chla_johnson = chla(., sensor = "MODISA", algo = "johnson")) %>% 
  mutate(chla_nasa = chla(., sensor = "MODISA", algo = "oceancolor")) %>% 
  dplyr::select(bin_num, chla_johnson, chla_nasa) %>% filter(chla_johnson > 0)
}


read_l3_fileset <- function(files) {
  bind_rows(lapply(files, sub_l3))
}


## counts up from the south
maxbin <- init$totbin/2



## initialize the bin logic for MODISA
init <- initbin(NUMROWS = 4320)




files <- modisfiles
files$season <- aceecostats::aes_season(files$date)
segs <- unclass(factor(cumsum(c(0, abs(diff(unclass(factor(files$season))))))))

db <- src_sqlite("/mnt/sql/modis_assessment.sqlite3", create = TRUE)
usegs <- unique(segs)
for (i in seq_along(usegs)) {
  asub <- segs == usegs[i]
  d <- read_l3_fileset(modisfiles$fullname[asub])
  d <- d %>% group_by(bin_num) %>% mutate(n = n()) %>% 
    summarize(chla_johnson_min = min(chla_johnson), chla_johnson_max = max(chla_johnson), 
              chla_nasa_min = min(chla_nasa), chla_nasa_max = max(chla_nasa), n = sum(n))
  d$date <- as.Date(modisfiles$date[asub][1])
  if (i == 0) {
    copy_to(db, d, "modisa", indexes = list("bin_num", "date"), temporary = FALSE)
  } else {
    db_insert_into( con = db$con, table = "modisa", values = d)
  }
}
