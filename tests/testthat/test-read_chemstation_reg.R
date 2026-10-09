# Expected pressures come from the RUN.LOG of each injection, which the
# instrument writes independently of the register file.

test_that("read_chemstation_reg reads column temperature and pump traces", {
  x <- read_chemstation_reg(extra_test_file("chemstation_carotenoid_LCDIAG.REG"))$traces

  expect_s3_class(x, "data.frame")
  expect_equal(colnames(x), c("trace", "unit", "time", "value"))
  expect_setequal(unique(x$trace),
                  c("THM1, Temperature (Left)", "PMP1, Pressure", "PMP1, Flow",
                    paste0("PMP1, Solvent ", LETTERS[1:4])))

  p <- x[x$trace == "PMP1, Pressure", ]
  expect_equal(unique(p$unit), "bar")
  expect_equal(nrow(p), 9000)
  expect_equal(range(p$time), c(0, 44.995))
  # RUN.LOG: "Pressure = 21.7 bar" at the end of the run
  expect_lt(abs(p$value[nrow(p)] - 21.7), 0.5)

  expect_true(all(x$value[x$trace == "PMP1, Flow"] == 0.4))
  temp <- x[x$trace == "THM1, Temperature (Left)", ]
  expect_equal(unique(temp$unit), "\u00b0C")
  expect_equal(nrow(temp), 2700)
  expect_true(all(abs(temp$value - 30) < 0.1))
  solvents <- split(x$value[grepl("Solvent", x$trace)],
                    x$trace[grepl("Solvent", x$trace)])
  expect_equal(Reduce(`+`, solvents), rep(100, 9000), tolerance = 0.01)
})

test_that("read_chemstation_reg reads start/stop conditions", {
  x <- read_chemstation_reg(extra_test_file("chemstation_carotenoid_LCDIAG.REG"))$conditions
  expect_equal(colnames(x), c("object", "key", "value"))
  pmp <- x[x$object == "PMP1, Start/Stop Conditions", ]
  # RUN.LOG: 41.2 bar at the start and 21.7 bar at the end
  expect_lt(abs(as.numeric(pmp$value[pmp$key == "StartPressure"]) - 41.2), 0.05)
  expect_lt(abs(as.numeric(pmp$value[pmp$key == "StopPressure"]) - 21.7), 0.05)
  expect_equal(pmp$value[pmp$key == "DateTime"], "28-Jun-13, 10:59:23")
})

test_that("read_chemstation_reg reads a ChemStation B.04 LCDIAG.REG", {
  x <- read_chemstation_reg(extra_test_file("chemstation_B0402_LCDIAG.REG"))
  expect_named(x, c("traces", "conditions", "tables"))

  # this revision writes "PMP1 , Pressure"; the space is dropped
  expect_setequal(unique(x$traces$trace),
                  c("PMP1, Pressure", "PMP1, Flow", paste0("PMP1, Solvent ",
                                                           LETTERS[1:4])))
  p <- x$traces[x$traces$trace == "PMP1, Pressure", ]
  expect_equal(range(p$time), c(0, 13))
  # RUN.LOG: "Pressure = 66.6 bar" at the end of the run
  expect_lt(abs(p$value[nrow(p)] - 66.6), 0.5)

  pmp <- x$conditions[x$conditions$object == "PMP1, Start/Stop Conditions", ]
  expect_lt(abs(as.numeric(pmp$value[pmp$key == "StartPressure"]) - 154.1), 0.05)
  expect_lt(abs(as.numeric(pmp$value[pmp$key == "StopPressure"]) - 66.6), 0.05)
})

test_that("read_chemstation_reg rejects files that are not register files", {
  path <- system.file("extdata/alkane_ladder.txt", package = "chromConverter")
  expect_error(read_chemstation_reg(path),
               "not a 'ChemStation' register file")
})

test_that("read_agilent_d returns LCDIAG.REG traces with what = 'instrument'", {
  d <- file.path(tempfile(), "sample.D")
  dir.create(d, recursive = TRUE)
  on.exit(unlink(dirname(d), recursive = TRUE))
  file.copy(extra_test_file("chemstation_carotenoid_LCDIAG.REG"),
            file.path(d, "LCDIAG.REG"))

  x <- read_agilent_d(d, what = "instrument")
  expect_named(x, c("THM1, Temperature (Left)", "PMP1, Pressure", "PMP1, Flow",
                    paste0("PMP1, Solvent ", LETTERS[1:4])))
  p <- x[["PMP1, Pressure"]]
  expect_true(is.matrix(p))
  expect_equal(dim(p), c(9000L, 1L))
  expect_equal(attr(p, "detector_y_unit"), "bar")
  expect_equal(attr(p, "run_datetime"),
               as.POSIXct("2013-06-28 10:59:23", tz = "UTC"))

  y <- read_agilent_d(d, what = "instrument", format_out = "data.frame",
                      data_format = "long")
  expect_equal(colnames(y[["PMP1, Flow"]]), c("rt", "intensity"))

  expect_error(read_agilent_d(test_path("testdata/RUTIN2.D"),
                              what = "instrument"), "No files found")
})

test_that("reg_trace rejects traces whose times and values differ in length", {
  expect_error(reg_trace("a", list(values = 1:2), list(unit = "", values = 1:3)),
               "3 values but 2 times")
  expect_error(reg_trace("a", NULL, list(unit = "", values = 1:3)),
               "3 values but 0 times")
})

test_that("read_chemstation_reg drops the keys of objects it could not parse", {
  local_mocked_bindings(reg_obj = function(a){
    a$kv <- list(list(key = "DateTime", value = "x"))
    stop("boom")
  })
  expect_warning(x <- read_chemstation_reg(
    extra_test_file("chemstation_carotenoid_LCDIAG.REG")), "boom")
  expect_equal(nrow(x$conditions), 0)
})

test_that("read_chemstation_reg keeps traces with the same title apart", {
  tr <- data.frame(trace = "PMP1, Pressure", unit = "bar", time = 1:2,
                   value = 1:2)
  local_mocked_bindings(reg_read_mfc = function(b, offs){
    list(traces = list(tr, tr), conditions = list(), failed = NULL)
  })
  x <- read_chemstation_reg(extra_test_file("chemstation_carotenoid_LCDIAG.REG"))
  expect_equal(unique(x$traces$trace), c("PMP1, Pressure", "PMP1, Pressure.1"))
})
