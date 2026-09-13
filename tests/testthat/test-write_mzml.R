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
    offsets <- as.numeric(regmatches(block, gregexpr(
      '(?<=<offset idRef="[^"]{1,80}">)[0-9]+', block, perl = TRUE))[[1]])
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
