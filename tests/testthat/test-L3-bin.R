context("L3-bin")


afile <- system.file("extdata", "ocfiles", "S2008001.L3b_DAY_RRS.nc", package = "roc")
test_that("read L3 binlist works", {
  bins <- read_binlist(afile)
  bins %>% 
    expect_s3_class("tbl_df") %>% 
    expect_length(5L) %>% 
    expect_named(c("bin_num", "nobs", "nscenes", "weights", "time_rec"))
  expect_equal(nrow(bins), 2L)
  expect_equal(sum(bins$nscenes),  2)
  
})


compound_names <- c("Rrs_412_sum", "Rrs_412_sum_squared", "Rrs_443_sum", "Rrs_443_sum_squared", 
                    "Rrs_490_sum", "Rrs_490_sum_squared", "Rrs_510_sum", "Rrs_510_sum_squared", 
                    "Rrs_555_sum", "Rrs_555_sum_squared", "Rrs_670_sum", "Rrs_670_sum_squared", 
                    "angstrom_sum", "angstrom_sum_squared", "aot_865_sum", "aot_865_sum_squared"
)
test_that("read L3bin data works", {
  bindat <- read_compound(afile)
  
  bindat %>% expect_s3_class("tbl_df") %>% 
    expect_length(16L) %>% 
    expect_named(compound_names)
  expect_equal(nrow(bindat),2)
  expect_equivalent(sum(bindat$aot_865_sum_squared),  0.03092645)
})
