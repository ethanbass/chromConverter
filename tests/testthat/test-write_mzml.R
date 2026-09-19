# Structural checks on the mzML writer. These use the bundled 'ChemStation'
# `.uv` fixture so they run without `chromConverterExtraTests`.

write_test_mzml <- function(dir, format_out = "matrix", ...){
  x <- read_chroms(test_path("testdata/dad1.uv"), format_in = "chemstation_uv",
                   parser = "chromconverter", format_out = format_out,
                   progress_bar = FALSE)[[1]]
  write_mzml(x, path_out = dir, force = TRUE, show_progress = FALSE, ...)
}

# the raw bytes of the file, which is what the offsets and checksum index
read_bytes <- function(path){
  readBin(path, "raw", n = file.size(path))
}

# position (0-indexed) of every occurrence of `pattern` in the file
locate <- function(bytes, pattern){
  grepRaw(pattern, bytes, all = TRUE, fixed = TRUE) - 1L
}

test_that("write_mzml records a correct fileChecksum", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  path <- write_test_mzml(tmp)
  bytes <- read_bytes(path)
  txt <- rawToChar(bytes)

  checksum <- sub(".*<fileChecksum>([0-9a-f]+)</fileChecksum>.*", "\\1", txt)
  expect_match(checksum, "^[0-9a-f]{40}$")

  # the mzML checksum covers the file through the opening `fileChecksum` tag
  end <- locate(bytes, "<fileChecksum>") + nchar("<fileChecksum>")
  expect_equal(checksum,
               digest::digest(bytes[seq_len(end)], algo = "sha1",
                              serialize = FALSE))
})

# Checks that every offset in every <index> lands exactly on the element it
# names and carries that element's id. `expect_identical` rather than `%in%`:
# the offsets must match the element starts one-for-one and in order, so a
# reordering or a miscount fails too. The id check matters separately -- a
# right byte carrying the wrong `idRef` would send a consumer to the wrong scan.
expect_index_offsets_exact <- function(path, n_spectra, n_chromatograms = 0L){
  bytes <- read_bytes(path)
  txt <- rawToChar(bytes)

  # <indexListOffset> must land on the <indexList> element
  index_list_offset <- as.numeric(
    sub(".*<indexListOffset>([0-9]+)</indexListOffset>.*", "\\1", txt))
  expect_equal(index_list_offset, locate(bytes, "<indexList "))

  check_section <- function(name, n_expected){
    block <- regmatches(txt, regexpr(
      sprintf('(?s)<index name="%s">.*?</index>', name), txt, perl = TRUE))
    if (n_expected == 0){
      expect_length(block, 0)
      return(invisible(NULL))
    }
    expect_length(block, 1)
    # `\\K` rather than a lookbehind: the prefix is variable-length, which PCRE
    # only accepts behind `(?<=)` from 10.43 on, so a lookbehind fails on older
    # systems than the one this was written on
    offsets <- as.numeric(regmatches(block, gregexpr(
      '<offset idRef="[^"]{1,80}">\\K[0-9]+', block, perl = TRUE))[[1]])
    idrefs <- regmatches(block, gregexpr(
      '(?<=<offset idRef=")[^"]{1,80}', block, perl = TRUE))[[1]]

    expect_equal(length(offsets), n_expected)
    expect_identical(offsets,
                     as.numeric(locate(bytes, sprintf("<%s ", name))))
    expect_identical(idrefs, regmatches(txt, gregexpr(
      sprintf('(?<=<%s id=")[^"]{1,80}', name), txt, perl = TRUE))[[1]])
  }

  check_section("spectrum", n_spectra)
  check_section("chromatogram", n_chromatograms)
}

test_that("write_mzml index offsets point at the elements they name", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  expect_index_offsets_exact(write_test_mzml(tmp), n_spectra = 1944)
})

test_that("write_mzml index offsets are exact without compression", {
  # compression changes the length of every binary array, and so every byte
  # position the index has to account for
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  expect_index_offsets_exact(write_test_mzml(tmp, compress = FALSE),
                             n_spectra = 1944)
})

test_that("write_mzml index offsets are exact for the chromatogram index", {
  skip_on_cran()
  skip_if_not_installed("chromConverterExtraTests")
  path_sms <- system.file("STRD15.SMS", package = "chromConverterExtraTests")
  skip_if_not(file.exists(path_sms))

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  x <- read_chroms(path_sms, format_in = "varian_sms", data_format = "long",
                   progress_bar = FALSE)[[1]]
  expect_named(x, c("MS1", "TIC", "BPC"))

  path <- write_mzml(x, path_out = tmp, force = TRUE, show_progress = FALSE)
  expect_index_offsets_exact(path, n_spectra = 3432, n_chromatograms = 2)
})

test_that("write_mzml honors `compress`", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  compressed <- write_test_mzml(tmp, compress = TRUE)
  txt <- rawToChar(read_bytes(compressed))
  expect_true(grepl("zlib compression", txt, fixed = TRUE))
  expect_false(grepl("no compression", txt, fixed = TRUE))

  unlink(compressed)
  uncompressed <- write_test_mzml(tmp, compress = FALSE)
  txt <- rawToChar(read_bytes(uncompressed))
  expect_true(grepl("no compression", txt, fixed = TRUE))
  expect_false(grepl("zlib compression", txt, fixed = TRUE))

  # an uncompressed 64-bit array is 8 bytes per value
  b64 <- regmatches(txt, regexpr("(?<=<binary>)[^<]+", txt, perl = TRUE))
  expect_equal(length(base64enc::base64decode(b64)) %% 8, 0)
})

test_that("write_mzml counts scans correctly for every input class", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  counts <- vapply(c("matrix", "data.frame", "data.table"), function(fmt){
    path <- write_test_mzml(tmp, format_out = fmt)
    txt <- rawToChar(read_bytes(path))
    unlink(path)
    as.numeric(sub('.*<spectrumList count="([0-9]+)".*', "\\1", txt))
  }, FUN.VALUE = numeric(1))

  # 1944 time points in dad1.uv, regardless of how the data was read
  expect_equal(unname(counts), rep(1944, 3))
})

test_that("write_mzml says what is wrong when it cannot identify the data", {
  local_reproducible_output()
  # a bare chromatogram is named for its `detector`, but not every parser
  # records one: 'Shimadzu' ASCII files give a `detector_model` instead, and
  # 'ChemStation' `.ch` files report `NA`
  x <- matrix(1:2, nrow = 2, dimnames = list(c("1", "2"), "intensity"))
  expect_error(write_mzml(x, path_out = tempdir()),
               "`detector` attribute is missing")

  # `NA` is how a parser reports that it looked and found nothing, so it is
  # described as missing rather than as a detector called "NA"
  attr(x, "detector") <- NA_character_
  expect_error(write_mzml(x, path_out = tempdir()),
               "`detector` attribute is missing")

  # a detector the writer has no mzML stream for is named in the message
  attr(x, "detector") <- "FID"
  expect_error(write_mzml(x, path_out = tempdir()), "FID")

  # a recognized detector gets past this check (and is stopped by the
  # dimensionality check below instead)
  msg <- function(expr) tryCatch({suppressWarnings(expr); ""},
                                 error = conditionMessage)
  attr(x, "detector") <- "UV"
  expect_false(grepl("`detector` attribute",
                     msg(write_mzml(x, path_out = tempdir()))))
})

test_that("write_mzml skips a one-dimensional chromatogram", {
  local_reproducible_output()
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  # written as spectra, a single trace becomes one single-point scan per
  # retention time, so it is sent to `write_andi_chrom` instead
  x <- matrix(1:2, nrow = 2, dimnames = list(c("1", "2"), "intensity"))
  attr(x, "detector") <- "UV"
  # with nothing else to write, skipping would leave an empty file
  expect_error(write_mzml(x, path_out = tmp), "write_andi_chrom",
               fixed = TRUE)
  # a named list must not slip past the check
  expect_error(write_mzml(list(DAD = x), path_out = tmp),
               "write_andi_chrom", fixed = TRUE)
  # long format says the same thing by having no wavelength column
  y <- data.frame(rt = c(1, 2), intensity = c(3, 4))
  attr(y, "data_format") <- "long"
  attr(y, "detector") <- "UV"
  expect_error(write_mzml(y, path_out = tmp), "write_andi_chrom",
               fixed = TRUE)

  # alongside another stream it is dropped with a warning, rather than
  # taking the streams that can be written down with it
  tic <- data.frame(rt = c(1, 2), intensity = c(5, 6))
  attr(tic, "data_format") <- "long"
  expect_warning(f <- write_mzml(list(TIC = tic, DAD = x),
                                 path_out = tmp, sample_name = "skip",
                                 force = TRUE, show_progress = FALSE),
                 "Skipping the DAD data")
  expect_true(file.exists(f))
  txt <- paste(readLines(f, warn = FALSE), collapse = "")
  expect_match(txt, "chromatogramList")
  # the header takes its metadata from a stream that survived into `what`. A
  # bare matrix has no `sample_name`, and reading it from the dropped DAD data
  # used to collapse the `sprintf` calls to `character(0)`, taking the `<mzML>`
  # element itself out of the file.
  expect_match(txt, "<mzML ")
  expect_no_error(xml2::read_xml(f))
})

test_that("write_mzml counts and indexes spectra correctly", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  long <- function(df){
    attr(df, "data_format") <- "long"
    df
  }
  ms1 <- long(data.frame(rt = rep(c(3, 4, 5), each = 2),
                         mz = rep(c(10, 20), 3), intensity = 1:6))

  # the TIC leads the MS1 by two points, so `write_spectra` pads the spectra
  # out to it and the header count has to be padded the same way
  f <- write_mzml(list(MS1 = ms1, TIC = long(data.frame(rt = 1:5,
                                                        intensity = c(0, 0, 7, 8, 9)))),
                  path_out = tmp, sample_name = "pad", force = TRUE,
                  show_progress = FALSE)
  txt <- paste(readLines(f, warn = FALSE), collapse = "")
  expect_equal(sub(".*<spectrumList count=.([0-9]+).*", "\\1", txt), "5")
  expect_length(gregexpr("<spectrum ", txt, fixed = TRUE)[[1]], 5)

  # `index` is the position in the spectrumList, so the DAD spectra carry on
  # from the MS1 spectra whether or not the file is indexed
  dad <- long(data.frame(rt = c(3, 3, 4, 4), lambda = c(200, 210, 200, 210),
                         intensity = 1:4))
  f <- write_mzml(list(MS1 = ms1[1:4, ], DAD = dad), path_out = tmp,
                  sample_name = "ix", force = TRUE, show_progress = FALSE,
                  indexed = FALSE)
  txt <- paste(readLines(f, warn = FALSE), collapse = "")
  expect_equal(regmatches(txt, gregexpr('index="[0-9]+"', txt))[[1]],
               sprintf('index="%d"', 0:3))
})

test_that("group_scans gathers retention times that are not contiguous", {
  dt <- data.table::as.data.table(data.frame(rt = c(1, 2, 1, 2),
                                             mz = c(10, 20, 11, 21),
                                             intensity = 1:4))
  attr(dt, "data_format") <- "long"
  scans <- group_scans(dt)
  expect_equal(scans$rts, c(1, 2))
  expect_equal(scans$get_scan(1)$mz, c(10, 11))
  expect_equal(scans$get_scan(2)$mz, c(20, 21))
})
