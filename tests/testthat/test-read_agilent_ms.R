write_msscan_fixture <- function(dir, spectra, rt, ms_level = rep(1L, length(spectra)),
                                 format_id = 1L, sizes = c(4, 4), profile = NA,
                                 stored_step = NULL, scan_type = 1L, precursor = 0,
                                 polarity = 0L, collision_energy = 0){
  scan_type <- rep_len(scan_type, length(spectra))
  polarity <- rep_len(polarity, length(spectra))
  collision_energy <- rep_len(collision_energy, length(spectra))
  precursor <- rep_len(precursor, length(spectra))
  profile <- rep_len(profile, length(spectra))
  format_id <- ifelse(is.na(profile), format_id, 2L)
  xsd <- file.path(dir, "MSScan.xsd")
  writeLines(c(
    '<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">',
    '<xs:complexType name="ScanRecordType"><xs:sequence>',
    '<xs:element name="ScanID" type="xs:int"/>',
    '<xs:element name="ScanTime" type="xs:double"/>',
    '<xs:element name="MSLevel" type="xs:int"/>',
    '<xs:element name="ScanType" type="xs:int"/>',
    '<xs:element name="IonPolarity" type="xs:int"/>',
    '<xs:element name="TIC" type="xs:double"/>',
    '<!-- <xs:element name="AbundanceLimit" type="xs:double"/> -->',
    '<xs:element name="BasePeakMZ" type="xs:double"/>',
    '<xs:element name="BasePeakValue" type="xs:double"/>',
    '<xs:element name="MzOfInterest" type="xs:double"/>',
    '<xs:element name="CollisionEnergy" type="xs:double"/>',
    '<xs:element name="SamplingPeriod" type="xs:double"/>',
    '<xs:element name="SpectrumParamValues" type="SpectrumParamsType"/>',
    '</xs:sequence></xs:complexType>',
    '<xs:complexType name="SpectrumParamsType"><xs:sequence>',
    '<xs:element name="SpectrumFormatID" type="xs:int"/>',
    '<xs:element name="SpectrumOffset" type="xs:long"/>',
    '<xs:element name="ByteCount" type="xs:int"/>',
    '<xs:element name="PointCount" type="xs:int"/>',
    '</xs:sequence></xs:complexType>',
    '</xs:schema>'), xsd)
  peak <- rawConnection(raw(0), "wb")
  writeBin(raw(68), peak)
  prof <- rawConnection(raw(0), "wb")
  writeBin(raw(68), prof)
  scan <- rawConnection(raw(0), "wb")
  writeBin(raw(88), scan)
  writeBin(92L, scan, size = 4, endian = "little")
  for (i in seq_along(spectra)){
    s <- spectra[[i]]
    step <- if (is.na(profile[i])) 0 else if (length(s$mz) > 1) s$mz[2] - s$mz[1] else 0.1
    if (length(s$mz) == 0){
      offset <- 0
      n_bytes <- 0
    } else if (!is.na(profile[i])){
      offset <- length(rawConnectionValue(prof))
      writeBin(c(s$mz[1], if (is.null(stored_step)) step else stored_step), prof,
               size = profile[i] / 2, endian = "little")
      writeBin(if (profile[i] == 8) as.integer(s$intensity) else s$intensity, prof,
               size = 4, endian = "little")
      n_bytes <- profile[i] + 4 * length(s$mz)
    } else {
      offset <- length(rawConnectionValue(peak))
      writeBin(s$mz, peak, size = sizes[1], endian = "little")
      writeBin(if (sizes[2] < 0) as.integer(s$intensity) else s$intensity, peak,
               size = abs(sizes[2]), endian = "little")
      n_bytes <- sum(abs(sizes)) * length(s$mz)
    }
    writeBin(i, scan, size = 4, endian = "little")
    writeBin(rt[i], scan, size = 8, endian = "little")
    writeBin(c(ms_level[i], scan_type[i], polarity[i]), scan, size = 4,
             endian = "little")
    writeBin(c(sum(s$intensity), c(s$mz[which.max(s$intensity)], 0)[1],
               max(s$intensity, 0), precursor[i], collision_energy[i], step),
             scan, size = 8, endian = "little")
    writeBin(as.integer(c(format_id[i], offset, 0, n_bytes, length(s$mz))), scan,
             size = 4, endian = "little")
  }
  files <- file.path(dir, c("MSScan.bin", "MSPeak.bin", "MSProfile.bin"))
  writeBin(rawConnectionValue(scan), files[1])
  if (anyNA(profile)) writeBin(rawConnectionValue(peak), files[2])
  if (!all(is.na(profile))) writeBin(rawConnectionValue(prof), files[3])
  close(scan)
  close(peak)
  close(prof)
  list(scan = files[1], peak = files[2], xsd = xsd)
}

spectra <- list(list(mz = c(41.1, 43, 57), intensity = c(150, 2214, 922)),
                list(mz = c(77, 105.5), intensity = c(486784, 4245)))

test_that("read_agilent_msdata decodes MSScan.bin and MSPeak.bin", {
  dir <- withr::local_tempdir()
  fx <- write_msscan_fixture(dir, spectra, rt = c(4.0973, 4.10745))
  x <- read_agilent_msdata(fx$scan, fx$peak, fx$xsd, what = c("MS1", "TIC", "BPC"),
                           format_out = "data.frame", data_format = "long")
  expect_equal(x$MS1$rt, rep(c(4.0973, 4.10745), c(3, 2)))
  expect_equal(x$MS1$mz, c(41.1, 43, 57, 77, 105.5), tolerance = 1e-6)
  expect_equal(x$MS1$intensity, c(150, 2214, 922, 486784, 4245))
  expect_equal(x$TIC$intensity, c(3286, 491029))
  expect_equal(x$BPC$mz, c(43, 77))
  expect_equal(x$BPC$intensity, c(2214, 486784))

  x <- read_agilent_msdata(fx$scan, fx$peak, fx$xsd, what = c("MS1", "TIC"),
                           format_out = "matrix", data_format = "wide")
  expect_s3_class(x$MS1, "data.table")
  expect_equal(rownames(x$TIC), as.character(c(4.0973, 4.10745)))
})

test_that("read_agilent_msdata reads 64-bit floats and integer intensities", {
  for (fmt in list(list(id = 1L, sizes = c(8, 8)), list(id = 3L, sizes = c(4, -4)))){
    dir <- withr::local_tempdir()
    fx <- write_msscan_fixture(dir, spectra, rt = c(1, 2), format_id = fmt$id,
                               sizes = fmt$sizes)
    x <- read_agilent_msdata(fx$scan, fx$peak, fx$xsd, what = "MS1",
                             format_out = "data.frame", data_format = "long")
    expect_equal(x$MS1$mz, c(41.1, 43, 57, 77, 105.5), tolerance = 1e-6)
    expect_equal(x$MS1$intensity, c(150, 2214, 922, 486784, 4245))
  }
})

test_that("read_agilent_msdata reads both layouts of MSProfile.bin", {
  profiles <- list(list(mz = c(100, 100.1, 100.2), intensity = c(5, 1250, 0)),
                   list(mz = c(100, 100.1, 100.2), intensity = c(7, 3, 980)))
  read <- function(fx){
    read_agilent_msdata(fx$scan, fx$peak, fx$xsd, what = "MS1",
                        format_out = "data.frame", data_format = "long")
  }
  for (header in c(8, 16)){
    fx <- write_msscan_fixture(withr::local_tempdir(), profiles, rt = c(1, 2),
                               profile = header, stored_step = 0.09999847)
    x <- read(fx)
    expect_equal(x$MS1$rt, rep(c(1, 2), each = 3))
    expect_equal(x$MS1$mz, rep(c(100, 100.1, 100.2), 2), tolerance = 1e-12)
    expect_equal(x$MS1$intensity, c(5, 1250, 0, 7, 3, 980))
  }

  fx <- write_msscan_fixture(withr::local_tempdir(), c(profiles[1], spectra[1]),
                             rt = c(1, 2), profile = c(8, NA))
  x <- read(fx)
  expect_equal(x$MS1$mz, c(100, 100.1, 100.2, 41.1, 43, 57), tolerance = 1e-6)
  expect_equal(x$MS1$intensity, c(5, 1250, 0, 150, 2214, 922))

  empty <- list(list(mz = numeric(), intensity = numeric()))
  fx <- write_msscan_fixture(withr::local_tempdir(), c(empty, profiles[2]),
                             rt = c(1, 2), profile = 8)
  x <- read(fx)
  expect_equal(x$MS1$rt, c(2, 2, 2))

  fx <- write_msscan_fixture(withr::local_tempdir(), profiles, rt = c(1, 2),
                             profile = 8, stored_step = 0.2)
  expect_error(read(fx), "in a layout that cannot be read yet",
               class = "agilent_ms_format_error")

  fx <- write_msscan_fixture(withr::local_tempdir(), profiles, rt = c(1, 2),
                             format_id = 2L, sizes = c(4, 4))
  expect_error(read(fx), "in a layout that cannot be read yet",
               class = "agilent_ms_format_error")
})

test_that("read_msscan_layout follows type names with a namespace prefix", {
  dir <- withr::local_tempdir()
  fx <- write_msscan_fixture(dir, spectra, rt = c(1, 2))
  writeLines(sub('type="SpectrumParamsType"', 'type="mstns:SpectrumParamsType"',
                 readLines(fx$xsd), fixed = TRUE), fx$xsd)
  x <- read_agilent_msdata(fx$scan, fx$peak, fx$xsd, what = "MS1",
                           format_out = "data.frame", data_format = "long")
  expect_equal(x$MS1$intensity, c(150, 2214, 922, 486784, 4245))
})

test_that("read_agilent_msdata reads product ion and MRM scans as MS2", {
  read <- function(fx){
    read_agilent_msdata(fx$scan, fx$peak, fx$xsd, what = c("MS1", "MS2"),
                        format_out = "data.frame", data_format = "long")
  }
  dir <- withr::local_tempdir()
  fx <- write_msscan_fixture(dir, spectra, rt = c(1, 2), ms_level = c(1L, 2L),
                             scan_type = c(1L, 512L), precursor = c(0, 609.3),
                             format_id = 3L, sizes = c(4, -4))
  x <- read(fx)
  expect_equal(x$MS1$intensity, c(150, 2214, 922))
  expect_named(x$MS2, c("rt", "precursor_mz", "mz", "intensity"))
  expect_equal(x$MS2$precursor_mz, c(609.3, 609.3))
  expect_equal(x$MS2$intensity, c(486784, 4245))

  # 8-byte points of spectrum format 1 hold integer intensities in MRM scans
  dir <- withr::local_tempdir()
  fx <- write_msscan_fixture(dir, spectra, rt = c(1, 2), ms_level = c(2L, 2L),
                             scan_type = 256L, precursor = c(309, 283),
                             sizes = c(4, -4), polarity = c(0L, 1L),
                             collision_energy = c(15, -20))
  x <- read(fx)
  expect_named(x, "MS2")
  expect_equal(x$MS2$precursor_mz, rep(c(309, 283), c(3, 2)))
  expect_equal(x$MS2$intensity, c(150, 2214, 922, 486784, 4245))
  info <- attr(x$MS2, "scan_info")
  expect_equal(info$polarity, c("positive", "negative"))
  expect_equal(info$collision_energy, c(15, 20))
  expect_equal(attr(x, "polarity"), c("positive", "negative"))

  dir <- withr::local_tempdir()
  fx <- write_msscan_fixture(dir, spectra, rt = c(1, 2), ms_level = c(2L, 2L),
                             scan_type = 512L, precursor = 609.3)
  expect_equal(read(fx)$MS2$intensity, c(150, 2214, 922, 486784, 4245))
})

test_that("read_agilent_d keeps MS1 where the MS2 spectra cannot be read", {
  local_reproducible_output()
  path <- file.path(withr::local_tempdir(), "run.d")
  dir.create(file.path(path, "AcqData"), recursive = TRUE)
  write_msscan_fixture(file.path(path, "AcqData"), spectra, rt = c(1, 2),
                       ms_level = c(1L, 2L), scan_type = c(1L, 512L),
                       format_id = c(1L, 5L))
  expect_warning(x <- read_agilent_d(path, format_out = "data.frame"),
                 "Left out 'MS2'")
  expect_named(x, c("MS1", "TIC", "BPC"))
  expect_equal(x$MS1$intensity, c(150, 2214, 922))
  expect_error(read_agilent_d(path, what = "MS2"), "cannot be read yet")

  path <- file.path(withr::local_tempdir(), "run.d")
  dir.create(file.path(path, "AcqData"), recursive = TRUE)
  write_msscan_fixture(file.path(path, "AcqData"), spectra, rt = c(1, 2))
  expect_no_warning(x <- read_agilent_d(path, format_out = "data.frame"))
  expect_named(x, c("MS1", "TIC", "BPC"))
  expect_error(read_agilent_d(path, what = "MS2"), "no MS2 scans")
})

test_that("read_agilent_msdata refuses what it cannot read", {
  read <- function(fx, what = "MS1"){
    read_agilent_msdata(fx$scan, fx$peak, fx$xsd, what = what,
                        format_out = "data.frame", data_format = "long")
  }
  dir <- withr::local_tempdir()
  fx <- write_msscan_fixture(dir, spectra, rt = c(1, 2), ms_level = c(1L, 2L),
                             scan_type = c(1L, 2048L))
  expect_warning(x <- read(fx, c("MS1", "MS2")), "Skipped 1 scans")
  expect_named(x, "MS1")
  expect_equal(unique(x$MS1$rt), 1)
  expect_error(suppressWarnings(read(fx, "MS2")), "no MS2 scans")

  dir <- withr::local_tempdir()
  fx <- write_msscan_fixture(dir, spectra, rt = c(1, 2), format_id = 2L)
  expect_error(read(fx), "cannot be read yet", class = "agilent_ms_format_error")

  dir <- withr::local_tempdir()
  fx <- write_msscan_fixture(dir, spectra, rt = c(1, 2))
  writeBin(raw(68), fx$peak)
  expect_error(read(fx), "points beyond the end", class = "agilent_ms_format_error")
  unlink(fx$peak)
  expect_error(read(fx), "'MSPeak.bin' could not be found")

  dir <- withr::local_tempdir()
  fx <- write_msscan_fixture(dir, spectra, rt = c(2, 1))
  expect_error(read(fx, "TIC"), "does not match the record layout",
               class = "agilent_ms_format_error")

  dir <- withr::local_tempdir()
  fx <- write_msscan_fixture(dir, spectra, rt = c(1, 2))
  writeLines(sub('<xs:element name="BasePeakMZ" type="xs:double"/>', "",
                 readLines(fx$xsd), fixed = TRUE), fx$xsd)
  expect_error(read(fx, "TIC"), "without .BasePeakMZ.",
               class = "agilent_ms_format_error")
})

test_that("the TIC and BPC are read from every scan record", {
  for (fmt in list(list(ms_level = c(2L, 2L), format_id = 1L),
                   list(ms_level = c(1L, 1L), format_id = 2L))){
    dir <- withr::local_tempdir()
    fx <- write_msscan_fixture(dir, spectra, rt = c(1, 2),
                               ms_level = fmt$ms_level, format_id = fmt$format_id)
    x <- read_agilent_msdata(fx$scan, fx$peak, fx$xsd, what = c("TIC", "BPC"),
                             format_out = "data.frame", data_format = "long")
    expect_equal(x$TIC$intensity, c(3286, 491029))
    expect_equal(x$BPC$mz, c(43, 77))
    expect_error(read_agilent_msdata(fx$scan, fx$peak, fx$xsd, what = "MS1",
                                     format_out = "data.frame",
                                     data_format = "long"))
  }
})
