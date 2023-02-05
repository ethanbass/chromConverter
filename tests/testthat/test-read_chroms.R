library(testthat)

path_csv <- "testdata/DAD1.CSV"
path_uv <- "testdata/dad1.uv"

x <- read_chroms(path_csv, format_in = "chemstation_csv")

test_that("aston parser works", {
  skip_if_missing_dependecies()
  paths <- rep(path_uv,2)
  x1 <- read_chroms(paths, format_in = "chemstation_uv", parser = "aston",
                    find_files = FALSE,
                    read_metadata = TRUE)
  expect_equal(as.numeric(x[[1]][,1]), as.numeric(x1[[1]][,"220.0"]))
  expect_equal(as.numeric(rownames(x[[1]])), as.numeric(rownames(x1[[1]])))
  expect_equal(length(x1), length(paths))
  expect_equal(class(x1[[1]])[1], "matrix")
  expect_equal(attr(x1[[1]], "data_format"), "wide")
  })

test_that("entab parser works", {
  skip_if_not_installed("entab")
  file <- "testdata/DAD1.uv"
  x1 <- read_chroms(file, format_in = "chemstation_uv", parser = "entab",
                    find_files = FALSE,
                    read_metadata = TRUE)
  expect_equal(as.numeric(x[[1]][,1]), as.numeric(x1[[1]][,"220"]))
  expect_equal(as.numeric(rownames(x[[1]])), as.numeric(rownames(x1[[1]])))
  expect_equal(class(x1[[1]])[1], "matrix")
  expect_equal(attr(x1[[1]], "parser"), "entab")
  expect_equal(attr(x1[[1]], "data_format"), "wide")
})


# test_that("rainbow parser works", {
#   skip_if_missing_dependecies()
#   file <- "testdata/DAD1.uv"
#   x1 <- read_chroms(file, format_in = "chemstation_uv", parser = "rainbow",
#                     find_files = FALSE,
#                     read_metadata = TRUE)
#   expect_equal(as.numeric(x[[1]][,1]), as.numeric(x1[[1]][,"220"]))
#   expect_equal(as.numeric(rownames(x[[1]])), as.numeric(rownames(x1[[1]])))
#   expect_equal(class(x1[[1]])[1], "matrix")
#   expect_equal(attr(x1[[1]], "parser"), "rainbow")
#   expect_equal(attr(x1[[1]], "data_format"), "wide")
# })

test_that("shimadzu parser works", {
  file <- "testdata/ladder.txt"
  x <- read_chroms(file, format_in = "shimadzu_fid", find_files = FALSE)
  expect_equal(class(x[[1]])[1], "matrix")
  expect_equal(attributes(x[[1]])$instrument, "GC-2014")
})

test_that("check_path works on unix/linux", {
  skip_on_os("windows")
  expect_equal(check_path("~/Downloads"), "~/Downloads/")
  expect_equal(check_path("Downloads"), "/Downloads/")
  expect_equal(check_path("~/Downloads/"), "~/Downloads/")
  expect_equal(check_path("/Users/foo/"), "/Users/foo/")
  expect_equal(check_path("Users/foo/"), "/Users/foo/")
  expect_equal(check_path("/Users/foo"), "/Users/foo/")
  expect_equal(check_path("Users/foo"), "/Users/foo/")
})

# test_that("thermoraw parser works",{
#   skip_if_not(configure_thermo_parser(check = TRUE))
#   file <- "/Users/ethanbass/Downloads/chrom_files/small.RAW"
#   x <- read_chroms(file, format_in = "thermoraw", find_files = FALSE)
#   expect_equal(class(x[[1]])[1], "matrix")
#   expect_equal(attributes(x[[1]])$instrument, "GC-2014")
# })

