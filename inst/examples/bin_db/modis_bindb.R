library(raadtools)
files <- chla_johnsonfiles()
library(dplyr)

f <- "/mnt/sql/test_index.sqlite3"
#unlink(f)
db <- src_sqlite(f, create = TRUE)

for (i in seq_len(nrow(files))) {
  d <- readRDS(files$fullname[i]) %>% mutate(date = as.integer(as.Date(date))) 
  if (i == 1 ) {
    d <- copy_to(db, d, name = "atable", indexes = list("date", "bin_num"))
  } else {
    d <- db_insert_into(db$con, values = d, table = "atable")
  }
  print(i)
  if (i %% 100 == 0) {
    rm(db)

    print(Sys.time())
    db <- src_sqlite(f)
  }
}
