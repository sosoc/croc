library(raadtools)
library(roc)
library(tibble)
library(dplyr)
## data frame of all L3 RRS files for MODISA


files <- ocfiles(time.resolution = "daily", product = "MODISA", varname = "RRS", type = "L3b", bz2.rm = TRUE, ext = "nc") %>% 
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

## initialize the bin logic for MODISA
init <- initbin(NUMROWS = 4320)
## counts up from the south
maxbin <- init$totbin/2

library(dplyr)
library(DBI)
db <- src_sqlite("/mnt/sql/modisa_rj_so_chl_l3.sqlite3", create = TRUE)
icount <- 0
for (ifile in seq_len(nrow(files))) {
  xx <- try(read_L3_file(files$fullname[ifile], compound_vars = c("Rrs_443", "Rrs_488", "Rrs_555")))
  if (inherits(xx, "try-error")) next; 
  d <- xx %>% 
    filter(bin_num <= maxbin) %>% mutate(chl = chla(., sensor = "MODISA", algo = "johnson")) %>% 
    dplyr::select(bin_num, chl) %>% filter(chl > 0) %>% mutate(date = as.integer(as.Date(files$date[ifile])))
  if (nrow(d) < 1) next; 
  icount <- icount + 1
  if (icount == 1) {
    copy_to(db, d, name = "modisa", indexes = list("date", "bin_num"), temporary = FALSE)
  } else {
    db_insert_into( con = db$con, table = "modisa", values = d)
  }
  if (ifile %% 100 == 0) print(ifile)
}

