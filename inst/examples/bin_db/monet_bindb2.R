library(raadtools)
files <- chla_johnsonfiles()
library(DBI)
dbdir <- "/mnt/sql/md_modis"
#dir.create(dbdir)
con <- dbConnect(MonetDBLite::MonetDBLite(), dbdir)


library(dplyr)
for (i in seq_len(nrow(files))) {
  d <- readRDS(files$fullname[i]) %>% mutate(day = as.integer(as.Date(date))) %>% 
    dplyr::select(-date)
  dbWriteTable(con, "bins", d, append = i > 1)
  if (i %% 20 == 0) print(i)
}


