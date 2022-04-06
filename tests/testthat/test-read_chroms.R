library(testthat)
library(chromConverter)

elementwise.all.equal <- Vectorize(function(x, y, ...) {isTRUE(all.equal(x, y, ...))})

test_that("chemstation uv importer works", {
  path<-"testdata/DAD1.CSV"
  x <- read.csv(path, fileEncoding = "utf-16")
  path1 <- "testdata/DAD1.uv"
  x1 <- uv_converter("testdata/DAD1.uv")
  l <- length(which(!elementwise.all.equal(x[,2], x1[,"220.0"])))
  expect_equal(l,0)})
