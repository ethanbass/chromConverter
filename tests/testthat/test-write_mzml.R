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
  expect_equal(scans$keys, c(1, 2))
  expect_equal(scans$get_scan(1)$mz, c(10, 11))
  expect_equal(scans$get_scan(2)$mz, c(20, 21))

  # grouping on `scan` keeps two levels apart where they share a time
  dt$scan <- c(1L, 2L, 1L, 2L)
  expect_equal(group_scans(dt, by = "scan")$get_scan(2)$mz, c(20, 21))
})

# An interleaved DDA run: scans 1 and 4 are MS1, 2, 3 and 5 are the product-ion
# spectra taken from them. The two levels arrive as separate tables, as
# `read_shimadzu_lcd` returns them, each with its own `scan_info`.
dda_fixture <- function(){
  long <- function(df, info){
    attr(df, "data_format") <- "long"
    attr(df, "scan_info") <- info
    df
  }
  list(MS1 = long(data.frame(scan = rep(c(1L, 4L), each = 2),
                             rt = rep(c(0.1, 0.4), each = 2),
                             mz = c(100, 200, 110, 210),
                             intensity = c(1, 2, 3, 4)),
                  data.frame(scan = c(1L, 4L), rt = c(0.1, 0.4), ms_level = 1L,
                             polarity = "positive", precursor_mz = NA_real_)),
       MS2 = long(data.frame(scan = c(2L, 2L, 3L, 5L),
                             rt = c(0.2, 0.2, 0.3, 0.5),
                             precursor_mz = c(200, 200, 100, NA),
                             mz = c(50, 60, 40, 55), intensity = c(5, 6, 7, 8)),
                  data.frame(scan = c(2L, 3L, 5L), rt = c(0.2, 0.3, 0.5),
                             ms_level = 2L, polarity = "negative",
                             precursor_mz = c(200, 100, NA))))
}

write_dda <- function(dir, ...){
  write_mzml(dda_fixture(), path_out = dir, sample_name = "dda", force = TRUE,
             show_progress = FALSE, ...)
}

test_that("write_mzml interleaves MS1 and MS2 in scan order", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  f <- write_dda(tmp)
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")

  expect_equal(sub(".*<spectrumList count=.([0-9]+).*", "\\1",
                   gsub("\n", " ", txt)), "5")
  # a spectrum is named for its scan, not for its place in the list, so the
  # names stay unique across the two levels
  expect_equal(regmatches(txt, gregexpr('(?<=<spectrum id=")[^"]+', txt,
                                        perl = TRUE))[[1]],
               sprintf("scan=%d", 1:5))
  expect_equal(regmatches(txt, gregexpr('index="[0-9]+"', txt))[[1]],
               sprintf('index="%d"', 0:4))
  expect_equal(regmatches(txt, gregexpr('(?<=name="ms level" value=")[0-9]+',
                                        txt, perl = TRUE))[[1]],
               c("1", "2", "2", "1", "2"))
  # scan start times run forwards through the whole list
  rts <- as.numeric(regmatches(txt, gregexpr(
    '(?<=name="scan start time" value=")[0-9.]+', txt, perl = TRUE))[[1]])
  expect_false(is.unsorted(rts))
  expect_no_error(xml2::read_xml(f))
})

test_that("write_mzml writes the precursor of each MS2 spectrum", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  txt <- paste(readLines(write_dda(tmp), warn = FALSE), collapse = "\n")

  # scan 5 has no precursor recorded, so it gets no `precursorList` rather
  # than one naming `NA`
  expect_length(gregexpr("<precursorList ", txt, fixed = TRUE)[[1]], 2)
  expect_equal(regmatches(txt, gregexpr(
    '(?<=name="selected ion m/z" value=")[0-9.]+', txt, perl = TRUE))[[1]],
    c("200", "100"))
  # every `spectrumRef` names the MS1 spectrum that precedes the scan, and is
  # a spectrum that exists in the file
  refs <- regmatches(txt, gregexpr('(?<=<precursor spectrumRef=")[^"]+', txt,
                                   perl = TRUE))[[1]]
  expect_equal(refs, c("scan=1", "scan=1"))
  expect_true(all(refs %in% regmatches(txt, gregexpr('(?<=<spectrum id=")[^"]+',
                                                     txt, perl = TRUE))[[1]]))
  # the schema requires an `activation` whether or not anything is known
  expect_length(gregexpr("<activation>", txt, fixed = TRUE)[[1]], 2)
  # and `fileContent` says the file holds both levels
  expect_match(txt, 'accession="MS:1000580" name="MSn spectrum"')
})

test_that("write_mzml writes polarity and the spectrum representation", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  txt <- paste(readLines(write_dda(tmp), warn = FALSE), collapse = "\n")
  expect_length(gregexpr('name="positive scan"', txt)[[1]], 2)
  expect_length(gregexpr('name="negative scan"', txt)[[1]], 3)
  expect_length(gregexpr('name="centroid spectrum"', txt)[[1]], 5)

  txt <- paste(readLines(write_dda(tmp, centroided = FALSE), warn = FALSE),
               collapse = "\n")
  expect_length(gregexpr('name="profile spectrum"', txt)[[1]], 5)
  expect_false(grepl('name="centroid spectrum"', txt))
})

test_that("write_mzml indexes interleaved spectra correctly", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  # the precursor block changes the length of an MS2 spectrum, so the offsets
  # are the check that the byte counter still tracks what was written
  expect_index_offsets_exact(write_dda(tmp), n_spectra = 5)
  expect_index_offsets_exact(write_dda(tmp, compress = FALSE), n_spectra = 5)
})

test_that("write_mzml refuses MS levels that share scan numbers", {
  local_reproducible_output()
  x <- dda_fixture()
  x$MS2$scan[x$MS2$scan == 2] <- 1L
  attr(x$MS2, "scan_info")$scan[1] <- 1L
  expect_error(write_mzml(x, path_out = tempdir(), sample_name = "dup",
                          force = TRUE, show_progress = FALSE),
               "share scan numbers")
})

test_that("write_mzml writes spectra named as 'RaMS' names them", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  # what `read_mzml` hands back: no `scan`, and the precursor and the fragment
  # under names no other reader in the package uses
  long <- function(df){
    df <- data.table::as.data.table(df)
    attr(df, "data_format") <- "long"
    df
  }
  ms1 <- long(data.frame(rt = c(0.1, 0.1, 0.4), mz = c(100, 200, 110),
                         intensity = c(1, 2, 3)))
  ms2 <- long(data.frame(rt = c(0.2, 0.2, 0.3), premz = c(200, 200, 100),
                         fragmz = c(50, 60, 40), intensity = c(5, 6, 7),
                         voltage = NA_integer_))
  nms <- names(ms2)

  f <- write_mzml(list(MS1 = ms1, MS2 = ms2), path_out = tmp,
                  sample_name = "rams", force = TRUE, show_progress = FALSE)
  # renaming must not reach back into the caller's table, which `setnames`
  # would do
  expect_identical(names(ms2), nms)

  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_equal(regmatches(txt, gregexpr(
    '(?<=name="selected ion m/z" value=")[0-9.]+', txt, perl = TRUE))[[1]],
    c("200", "100"))
  # with no `scan` to merge on, the levels interleave by retention time
  expect_equal(regmatches(txt, gregexpr('(?<=name="ms level" value=")[0-9]+',
                                        txt, perl = TRUE))[[1]],
               c("1", "2", "2", "1"))
  expect_equal(regmatches(txt, gregexpr('(?<=defaultArrayLength=")[0-9]+', txt,
                                        perl = TRUE))[[1]],
               c("2", "2", "1", "1"))
  expect_no_error(xml2::read_xml(f))
})

test_that("write_mzml refuses a table it cannot build a spectrum from", {
  local_reproducible_output()
  # a missing column used to encode as an empty binary array, leaving the
  # spectrum declaring a `defaultArrayLength` that neither array matched
  x <- data.frame(rt = c(1, 2), intensity = c(3, 4))
  attr(x, "data_format") <- "long"
  attr(x, "detector") <- "MS"
  expect_error(write_mzml(x, path_out = tempdir(), sample_name = "no_mz",
                          force = TRUE, show_progress = FALSE),
               "has no 'mz' column")

  x$mz <- c(100, 200)
  x$intensity <- NULL
  expect_error(write_mzml(x, path_out = tempdir(), sample_name = "no_int",
                          force = TRUE, show_progress = FALSE),
               "has no 'intensity' column")
})

# The metadata the writer takes from the chromatogram: an MS1 stream carrying
# the fields chromConverter records, written without external files.

make_ms1 <- function(n_scan = 3, n_pt = 4){
  ms <- data.frame(rt = rep(seq_len(n_scan) * 0.1, each = n_pt),
                   mz = rep(seq_len(n_pt) + 99, times = n_scan),
                   intensity = seq_len(n_scan * n_pt))
  attr(ms, "data_format") <- "long"
  attr(ms, "time_unit") <- "Minutes"
  attr(ms, "detector") <- "MS"
  ms
}

write_ms1_mzml <- function(dir, ms, name = "meta"){
  write_mzml(list(MS1 = ms), path_out = dir, sample_name = name, force = TRUE,
             show_progress = FALSE)
}

test_that("write_mzml records the metadata the chromatogram carries", {
  skip_on_cran()

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  ms <- make_ms1()
  attr(ms, "polarity") <- "negative"
  attr(ms, "instrument") <- "LCMS8040"
  attr(ms, "detector_model") <- "LCMS-3030"
  attr(ms, "software") <- "LabSolutions"
  attr(ms, "software_version") <- "5.65"
  attr(ms, "operator") <- "A. Chemist"
  attr(ms, "sample_id") <- "JOU 812"
  attr(ms, "source_file_format") <- "andi_ms"

  txt <- paste(readLines(write_ms1_mzml(tmp, ms)), collapse = "\n")

  # one polarity cvParam per spectrum
  expect_equal(lengths(regmatches(txt, gregexpr("MS:1000129", txt)))[[1]], 3)
  expect_match(txt, 'accession="MS:1000031" name="instrument model" value="LCMS-3030"')
  expect_match(txt, 'accession="MS:1001455" name="acquisition software" value="LabSolutions"')
  expect_match(txt, '<software id="acquisition" version="5.65">', fixed = TRUE)
  expect_match(txt, '<softwareList count="2">', fixed = TRUE)
  expect_match(txt, 'accession="MS:1000586" name="contact name" value="A. Chemist"')
  expect_match(txt, 'accession="MS:1002441" name="Andi-MS format"')
  # `id` is an `xs:ID`, so the space cannot be carried into it
  expect_match(txt, '<sample id="sJOU_812" name="meta">', fixed = TRUE)
})

test_that("write_mzml falls back where the chromatogram records nothing", {
  skip_on_cran()

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  txt <- paste(readLines(write_ms1_mzml(tmp, make_ms1())), collapse = "\n")

  expect_false(grepl("MS:1000129|MS:1000130", txt))
  expect_false(grepl("MS:1000586", txt))
  expect_match(txt, '<cvParam cvRef="MS" accession="MS:1000031" name="instrument model"/>',
               fixed = TRUE)
  expect_match(txt, '<softwareList count="1">', fixed = TRUE)
  # a format with no term of its own is described by the parent term
  expect_match(txt, 'accession="MS:1000560" name="mass spectrometer file format"')
  # and a missing `sample_id` cannot become the string "sNA"
  expect_match(txt, '<sample id="s1"', fixed = TRUE)
  expect_false(grepl('id="sNA"', txt, fixed = TRUE))
})

test_that("write_mzml writes no polarity for a run that switched polarity", {
  skip_on_cran()

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  ms <- make_ms1()
  attr(ms, "polarity") <- "positive"
  attr(ms, "scan_info") <- data.frame(scan = 1:3, rt = (1:3) * 0.1,
                                      ms_level = 1L,
                                      polarity = c("positive", "negative",
                                                   "positive"))
  txt <- paste(readLines(write_ms1_mzml(tmp, ms)), collapse = "\n")
  expect_false(grepl("MS:1000129|MS:1000130", txt))
})

test_that("write_mzml skips a stream it was asked for but does not have", {
  skip_on_cran()

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  expect_warning(
    expect_warning(
      path <- write_mzml(list(MS1 = make_ms1(), MS2 = make_ms1(n_scan = 0)),
                         what = c("MS1", "MS2", "TIC"), path_out = tmp,
                         sample_name = "gaps", force = TRUE,
                         show_progress = FALSE),
      "MS2 data not found"),
    "TIC data not found")

  txt <- paste(readLines(path), collapse = "\n")
  # the header may not promise a stream the file does not go on to hold
  expect_false(grepl("MSn spectrum", txt, fixed = TRUE))
  expect_false(grepl("<chromatogramList", txt, fixed = TRUE))
  expect_s3_class(xml2::read_xml(path), "xml_document")
})

test_that("write_mzml leaves out a scan start time it does not have", {
  skip_on_cran()

  make_level <- function(scans, level){
    n_pt <- 4
    x <- data.frame(rt = rep(scans * 0.1, each = n_pt),
                    mz = rep(seq_len(n_pt) + 99, times = length(scans)),
                    intensity = seq_len(length(scans) * n_pt),
                    scan = rep(scans, each = n_pt))
    attr(x, "data_format") <- "long"
    attr(x, "time_unit") <- "Minutes"
    attr(x, "ms_level") <- level
    x
  }

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  ms1 <- make_level(1:3, 1L)
  attr(ms1, "scan_info") <- data.frame(scan = 1:3, rt = c(0.1, NA, 0.3))
  ms2 <- make_level(4L, 2L)
  attr(ms2, "scan_info") <- data.frame(scan = 4L, rt = 0.4,
                                       precursor_mz = 100)

  path <- write_mzml(list(MS1 = ms1, MS2 = ms2), path_out = tmp,
                     sample_name = "narts", force = TRUE,
                     show_progress = FALSE)
  txt <- paste(readLines(path), collapse = "\n")
  # "NA" is not an `xs:double`, so the term is dropped rather than written
  expect_false(grepl('value="NA"', txt, fixed = TRUE))
  expect_equal(lengths(regmatches(txt, gregexpr("scan start time", txt)))[[1]],
               3)
  expect_s3_class(xml2::read_xml(path), "xml_document")
})

test_that("write_mzml escapes the sample name, matches the source format
          case-insensitively and normalizes polarity", {
  skip_if_not_installed("xml2")

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  x <- data.frame(rt = rep(c(0.1, 0.2), each = 2), mz = c(100, 101, 100, 101),
                  intensity = 1:4)
  attr(x, "data_format") <- "long"
  attr(x, "time_unit") <- "Minutes"
  attr(x, "source_file_format") <- "mzML"
  attr(x, "polarity") <- "Positive Polarity"

  path <- write_mzml(list(MS1 = x), path_out = tmp,
                     sample_name = 'Std "A" & B', force = TRUE,
                     show_progress = FALSE)
  expect_s3_class(xml2::read_xml(path), "xml_document")
  txt <- paste(readLines(path), collapse = "\n")
  expect_true(grepl("MS:1000584", txt, fixed = TRUE))
  expect_true(grepl("MS:1000130", txt, fixed = TRUE))
})
