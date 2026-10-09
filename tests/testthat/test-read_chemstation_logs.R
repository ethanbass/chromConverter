# The fixture is cut from a real sequence log: the start of the sequence, its
# first two injections, and its last two, in which the modules timed out and
# the instrument error that followed aborted the run and the sequence. The
# `_ascii` copy is the same log without a byte order mark, as revision A
# writes it.

test_that("read_chemstation_logs reads events from a sequence log", {
  x <- read_chemstation_logs(test_path("testdata/chemstation_sequence.LOG"),
                             what = "events")

  expect_s3_class(x, "data.frame")
  expect_equal(colnames(x),
               c("sequence", "folder", "time", "injection", "data_file",
                 "problem", "event_code", "module_id", "source", "message"))
  expect_equal(unique(x$sequence), "ETHAN_01_20_21")
  expect_equal(unique(x$folder), "testdata")
  expect_equal(nrow(x), 56)

  expect_equal(x$message[1], "ETHAN_01_20_21.S started")
  expect_equal(x$source[1], "Sequence")
  expect_equal(x$message[5], "Pressure = 135.5 bar")
  expect_equal(x$message[13], "at line 2833 in file C:\\CHEM32\\CORE\\SIGNAL.MCX")

  timeout <- x[x$message == "Time-out", ]
  expect_equal(timeout$source, c("DAD        1", "PUMP       1", "ALS        1"))
  expect_equal(unique(timeout$event_code), "3e")
  expect_equal(timeout$module_id, c("1da8", "1da6", "1da7"))
  expect_equal(unique(timeout$time), as.POSIXct("2021-01-21 16:24:38", tz = "UTC"))

  expect_equal(x$message[56], "ETHAN_01_20_21.S terminated due to an error")
})

test_that("read_chemstation_logs assigns events to injections", {
  x <- read_chemstation_logs(test_path("testdata/chemstation_sequence.LOG"),
                             what = "events")

  expect_equal(unique(x$injection[3:17]), 1)
  expect_equal(unique(x$data_file[3:17]), "MEOH.D")
  expect_equal(unique(x$injection[18:32]), 2)
  expect_equal(unique(x$data_file[18:32]), "885_2.D")
  expect_equal(unique(x$data_file[x$message == "Time-out"]), "840.D")

  aborted <- x[x$message == "Method aborted", ]
  expect_equal(aborted$injection, 4)
  expect_true(is.na(aborted$data_file))

  expect_true(all(is.na(x$injection[x$source == "Sequence"])))
  expect_true(is.na(x$injection[x$message == "Loading Method AINO_DT_L12_8L.M"]))
  expect_true(is.na(x$injection[51]))
})

test_that("read_chemstation_logs leaves events between injections unassigned", {
  fixture <- test_path("testdata/chemstation_sequence_ascii.LOG")
  lines <- readLines(fixture)
  path <- withr::local_tempfile(fileext = ".LOG")
  writeLines(append(lines, lines[3:4], after = 36), path, sep = "\r\n")

  x <- read_chemstation_logs(path, what = "events")
  expect_equal(x$message[18], "Loading Method AINO_DT_L12_8L.M")
  expect_true(is.na(x$injection[18]))
  expect_equal(x$injection[c(17, 19)], 1:2)
  expect_equal(read_chemstation_logs(path)[-2], read_chemstation_logs(fixture)[-2])
})

test_that("read_chemstation_logs reads Latin-1 logs from ChemStation revision A", {
  x <- read_chemstation_logs(test_path("testdata/chemstation_sequence_ascii.LOG"),
                             what = "events")
  y <- read_chemstation_logs(test_path("testdata/chemstation_sequence.LOG"),
                             what = "events")
  expect_identical(x, y)

  f <- withr::local_tempfile(fileext = ".LOG")
  writeBin(iconv(paste0(c(
    "  69 41df 55edf634    0",
    "Sequence     SEQ.S started                                  16:40:20 09/07/15",
    "   0 1da8 55edf673  500",
    "1100 THM   1 Column temperature = 40.0 \u00b0C                   16:41:23 09/07/15"
  ), "\r\n", collapse = ""), "UTF-8", "latin1", toRaw = TRUE)[[1]], f)
  expect_equal(read_chemstation_logs(f, what = "events")$message[2],
               "Column temperature = 40.0 \u00b0C")
})

test_that("read_chemstation_logs returns a data.table on request", {
  x <- read_chemstation_logs(test_path("testdata/chemstation_sequence.LOG"),
                             what = "events", format_out = "data.table")
  expect_s3_class(x, "data.table")
  expect_equal(nrow(x), 56)
})

test_that("read_chemstation_logs rejects files that are not logs", {
  path <- system.file("extdata/alkane_ladder.txt", package = "chromConverter")
  expect_error(read_chemstation_logs(path),
               "not a recognised")
})

test_that("read_chemstation_logs reads the sequence logs in a directory", {
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE))
  for (d in c("seq1", "seq2", "seq2/1AA-0101.D")) {
    dir.create(file.path(root, d), recursive = TRUE)
  }
  fixture <- test_path("testdata/chemstation_sequence.LOG")
  file.copy(fixture, file.path(root, "seq1", "SEQ1.LOG"))
  file.copy(test_path("testdata/chemstation_sequence_ascii.LOG"),
            file.path(root, "seq2", "SEQ2.LOG"))
  file.copy(fixture, file.path(root, "seq2", "1AA-0101.D", "RUN.LOG"))
  file.copy(system.file("extdata/alkane_ladder.txt", package = "chromConverter"),
            file.path(root, "seq2", "OTHER.LOG"))

  expect_warning(x <- read_chemstation_logs(root, what = "events"), "OTHER.LOG")
  expect_equal(unique(x$folder), c("seq1", "seq2"))
  expect_equal(nrow(x), 112)

  p <- suppressWarnings(read_chemstation_logs(root, what = "problems"))
  expect_equal(p$incident, rep(1:2, each = 9))

  y <- read_chemstation_logs(c(file.path(root, "seq1"), fixture),
                             what = "events")
  expect_equal(unique(y$folder), c("seq1", "testdata"))

  expect_error(read_chemstation_logs(file.path(root, "seq2", "1AA-0101.D")),
               "No sequence logs found")

  conf <- file.path(root, "conf.d")
  dir.create(file.path(conf, "seqs"), recursive = TRUE)
  file.copy(fixture, file.path(conf, "seqs", "SEQ1.LOG"))
  expect_equal(nrow(read_chemstation_logs(conf, what = "events")), 56)
  expect_error(read_chemstation_logs(file.path(root, "seq2", "OTHER.LOG")),
               "not a recognised")
})

test_that("read_chemstation_logs marks problems", {
  path <- test_path("testdata/chemstation_sequence.LOG")
  x <- read_chemstation_logs(path, what = "events")
  expect_equal(which(x$problem), c(35:39, 51, 54:56))
  expect_false(any(x$problem[x$source == "Method" &
                               x$message == "Method completed"]))
  expect_false(any(x$problem[x$source == "CP Macro"]))

  p <- read_chemstation_logs(path, what = "problems")
  expect_equal(p$message, x$message[c(35:39, 51, 54:56)])
  expect_equal(colnames(p), append(setdiff(colnames(x), "problem"), "incident",
                                   after = 2))
  expect_equal(p$incident, rep(1L, 9))
  expect_equal(p$message[c(1, 9)],
               c("Time-out", "ETHAN_01_20_21.S terminated due to an error"))
})

test_that("read_chemstation_logs summarizes injections", {
  x <- read_chemstation_logs(test_path("testdata/chemstation_sequence.LOG"))
  expect_equal(colnames(x),
               c("sequence", "folder", "injection", "data_file", "sample",
                 "start", "minutes", "status", "pressure_start",
                 "pressure_end", "problems"))
  expect_equal(x$injection, 1:4)
  expect_equal(x$data_file, c("MEOH.D", "885_2.D", "840.D", NA))
  expect_equal(x$sample, c("Vial 1", "Vial 5", "Vial 30", "Vial 31"))
  expect_equal(x$status, c("completed", "completed", "completed", "aborted"))
  expect_equal(x$pressure_start, c(135.5, 138.1, NA, NA))
  expect_equal(x$pressure_end, c(49.4, 49.6, NA, NA))
  expect_equal(x$problems[1:2], c("", ""))
  expect_equal(x$problems[3], "Time-out; Error Method started")
  expect_match(x$problems[4], "^Instrument Error")
  expect_equal(x$start[1], as.POSIXct("2021-01-21 03:12:31", tz = "UTC"))
})

test_that("read_chemstation_logs names the sequence of a run log by its folder", {
  d <- file.path(tempfile(), "SEQ", "1AA-0101.D")
  dir.create(d, recursive = TRUE)
  on.exit(unlink(dirname(dirname(d)), recursive = TRUE))
  file.copy(test_path("testdata/chemstation_sequence.LOG"),
            file.path(d, "RUN.LOG"))
  x <- read_chemstation_logs(file.path(d, "RUN.LOG"), what = "events")
  expect_equal(unique(x$folder), "SEQ")
  expect_equal(unique(x$sequence), "ETHAN_01_20_21")
})

test_that("read_chemstation_logs drops a last event whose message is not written yet", {
  x <- readLines(test_path("testdata/chemstation_sequence_ascii.LOG"),
                 encoding = "bytes")
  x <- x[grepl("[^ \t\r]", x, useBytes = TRUE)]
  f <- withr::local_tempfile(fileext = ".LOG")
  writeLines(x[-length(x)], f, useBytes = TRUE)
  full <- read_chemstation_logs(test_path("testdata/chemstation_sequence_ascii.LOG"),
                                what = "events")
  cut <- read_chemstation_logs(f, what = "events")
  cols <- setdiff(names(full), "folder")
  expect_equal(cut[cols], full[-nrow(full), cols], ignore_attr = TRUE)
})

test_that("read_chemstation_logs accepts format_out = 'matrix'", {
  x <- read_chemstation_logs(test_path("testdata/chemstation_sequence.LOG"),
                             format_out = "matrix")
  expect_s3_class(x, "data.table")
})

test_that("read_chemstation_logs joins wrapped messages and skips runs that acquire nothing", {
  ev <- function(code, src, msg){
    c(sprintf("%5s 41e0 5ff00000    0", code),
      sprintf("%-13s%-45s  12:00:00 01/01/21", src, msg))
  }
  name <- "ABCDEFGHIJKLMNOPQRSTUVWXYZA"
  f <- withr::local_tempfile(fileext = ".LOG")
  writeLines(c(
    ev("69", "Sequence", paste0(name, ".S started")),
    ev("7da", "Method", "Loading Method A.M"),
    ev("443", "Method", "Method started:  line# 1 vial# 1 inj# 1"),
    ev("67", "Method", "Instrument running sample Vial 1"),
    ev("70", "CP Macro", "Analyzing rawdata ABCDEFGHIJKLMNOPQRSTUVWXYZ>"),
    ev("70", "CP Macro", " 1.D"),
    ev("67", "Method", "Method completed"),
    ev("7da", "Method", "Loading Method EXPORT.M"),
    ev("443", "Method", "Method started:  line# 1 vial# 1 inj# 1"),
    ev("70", "CP Macro", "Custom Data Analysis on rawdata A.D"),
    ev("67", "Method", "Method completed"),
    ev("43e", "Sequence", paste0(name, ".S stopped by use"))
  ), f, sep = "\r\n")

  e <- read_chemstation_logs(f, what = "events")
  expect_equal(nrow(e), 11)
  expect_equal(e$message[5], "Analyzing rawdata ABCDEFGHIJKLMNOPQRSTUVWXYZ 1.D")
  expect_equal(e$injection, c(NA, NA, 1, 1, 1, 1, NA, NA, NA, NA, NA))
  expect_true(e$problem[11])

  i <- read_chemstation_logs(f)
  expect_equal(nrow(i), 1)
  expect_equal(i$data_file, "ABCDEFGHIJKLMNOPQRSTUVWXYZ 1.D")
})
