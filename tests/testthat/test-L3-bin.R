context("L3-bin")

rrsfiles <- raadtools::ocfiles(product = "SeaWiFS")
afile <- rrsfiles$fullname[200]
test_that("read L3bin works", {
  bins <- read_binlist(afile)
  bins %>% 
    expect_s3_class("tbl_df") %>% 
    expect_length(5L) %>% 
    expect_named(c("bin_num", "nobs", "nscenes", "weights", "time_rec"))
})
