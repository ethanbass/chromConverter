library(testthat)

path_csv <- "testdata/DAD1.CSV"
path_uv <- "testdata/dad1.uv"

x <- read_chroms(path_csv, format_in = "chemstation_csv")

test_that("read_chroms works", {
  skip_if_missing_dependecies()
  paths <- rep(path_uv,2)
  x1 <- read_chroms(paths, format_in = "chemstation_uv", parser = "aston",
                    find_files = FALSE,
                    read_metadata = FALSE)
  expect_equal(as.numeric(x[[1]][,1]), as.numeric(x1[[1]][,"220.0"]))
  expect_equal(as.numeric(rownames(x[[1]])), as.numeric(rownames(x1[[1]])))
  expect_equal(length(x1), length(paths))
  expect_equal(class(x1[[1]])[1], "matrix")
  })

test_that("entab parser works", {
  skip_if_not_installed("entab")
  file <- "testdata/DAD1.uv"
  x1 <- read_chroms(file, format_in = "chemstation_uv", parser = "entab",
                    find_files = FALSE,
                    read_metadata = FALSE)
  expect_equal(as.numeric(x[[1]][,1]), as.numeric(x1[[1]][,"220"]))
  expect_equal(as.numeric(rownames(x[[1]])), as.numeric(rownames(x1[[1]])))
  expect_equal(class(x1[[1]])[1], "matrix")
})


test_that("shimadzu parser works", {
  file <- "testdata/ladder.txt"
  x <- read_chroms(file, format_in = "shimadzu_fid", find_files = FALSE)
  expect_equal(class(x[[1]]), "matrix")
  expect_equal(attributes(x[[1]])$instrument, "GC-2014")
})


# test_that("thermoraw parser works",{
#   skip_if_not(configure_thermo_parser(check=T))
#   file <- "testdata/ladder.txt"
#   x <- read_chroms(file, format_in = "shimadzu_fid", find_files = FALSE)
#   expect_equal(class(x[[1]]), "matrix")
#   expect_equal(attributes(x[[1]])$instrument, "GC-2014")
# })
