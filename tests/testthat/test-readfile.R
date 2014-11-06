context("Basic read")

ftest1 <- system.file("extdata", "ocfiles", "S2008001.L3b_DAY_CHL.main", package = "roc")
ftest2 <- system.file("extdata", "ocfiles", "S2010006.L3b_DAY_RRS.main", package = "roc")
test_that("file read is successful", {
  expect_that(file.exists(ftest1), is_true()) 
  expect_that(readL3(ftest1),is_a("list"))
  expect_that(names(readL3(ftest2))[21L], equals("Rrs_670_ssq"))
 })
 
