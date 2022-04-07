library(testthat)

# helper function to test equality
elementwise.all.equal <- Vectorize(function(x, y, ...) {isTRUE(all.equal(x, y, ...))})

# helper function to skip tests if we don't have the right python dependencies
skip_if_missing_dependecies <- function() {
  reqs <- c("scipy","numpy", "aston", "pandas")
  have_reqs <- sapply(reqs, py_module_available)
  if (mean(have_reqs) < 1)
    skip(paste("required packages", reqs[!have_reqs],
               "not available for testing"))
}

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

