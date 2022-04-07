library(testthat)

test_that("chemstation uv importer works", {
  skip_if_missing_dependecies()
  path<-"testdata/DAD1.CSV"
  x <- read.csv(path, fileEncoding = "utf-16")
  path1 <- "testdata/DAD1.uv"
  x1 <- uv_converter("testdata/DAD1.uv")
  l <- length(which(!elementwise.all.equal(x[,2], x1[,"220.0"])))
  expect_equal(l,0)})

test_that("read_chroms works", {
  skip_if_missing_dependecies()
  path<-"testdata/DAD1.CSV"
  x <- read.csv(path, fileEncoding = "utf-16")
  paths <- rep("testdata/DAD1.uv",2)
  x1 <- read_chroms(paths, format.in = "chemstation.uv",find_files = FALSE)
  l <- length(which(!elementwise.all.equal(x[,2], x1[[1]][,"220.0"])))
  expect_equal(l,0)
  expect_equal(length(x1), length(paths))
  })

