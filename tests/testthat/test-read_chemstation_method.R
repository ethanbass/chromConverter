# Expected settings come from the ACQ.TXT printout 'ChemStation' wrote for the
# run the orange method was copied from.

test_that("read_chemstation_method reads a binary pump method", {
  path <- extra_test_file("chemstation_orange_ACQ.M")
  x <- read_chemstation_method(path)
  expect_named(x, c("pump", "dad", "autosampler", "column"))

  expect_equal(x$pump$flow_mL_min, 0.75)
  expect_equal(x$pump$stop_time_min, 5)
  expect_equal(x$pump$post_time_min, 0.5)
  expect_equal(x$pump$pressure_high_bar, 400)
  expect_equal(x$pump$solvents$channel, c("A", "B"))
  expect_equal(x$pump$solvents$percentage, c(90, 10))
  expect_equal(x$pump$gradient,
               data.frame(time_min = c(0, 4, 4.99, 5),
                          pct_A = c(90, 1, 1, 90),
                          pct_B = c(10, 99, 99, 10),
                          flow_mL_min = 0.75))

  expect_equal(x$dad$signals,
               data.frame(id = "A", wavelength_nm = 210, bandwidth_nm = 8,
                          reference_nm = 360, reference_bandwidth_nm = 100))
  expect_equal(c(x$dad$spectra_from_nm, x$dad$spectra_to_nm), c(190, 400))
  # "Right temperature: Same as left"
  expect_equal(x$column$temp_controls$temperature_C, c(45, 45))
  # "Enable analysis: When Temp. is within setpoint +/- 0.8°C"
  expect_equal(x$column$temp_controls$not_ready_limit_C, c(0.8, 0.8))
  # "Posttime: Off"
  expect_equal(x$column$post_time_min, NA_real_)
  # the wellplate sampler's settings are in `LWLS1.REG`
  expect_equal(x$autosampler$injection_volume_uL, 2)
  expect_equal(x$autosampler$draw_speed_uL_min, 200)
  expect_equal(x$autosampler$eject_speed_uL_min, 200)
})

test_that("read_chemstation_method reads a quaternary pump method", {
  path <- extra_test_file("chemstation_carotenoid_RUN.M")
  x <- read_chemstation_method(path)
  expect_named(x, c("pump", "dad", "autosampler", "column"))
  expect_equal(x$pump$solvents$solvent,
               c("Hexane/1%IPA", "IPA", "MeOH:DCM", "MeOH:H2O"))
  expect_equal(x$pump$gradient,
               data.frame(time_min = c(0, 1, 16), pct_A = 0, pct_B = 0,
                          pct_C = c(0, 0, 100), pct_D = c(100, 100, 0)))
  expect_equal(nrow(x$dad$signals), 0)
  expect_equal(x$autosampler$injection_volume_uL, 5)
})

test_that("read_chemstation_method reads a revision A method", {
  # the gradient matches the solvent traces in the run's LCDIAG.REG
  path <- extra_test_file("chemstation_mwd_RUN.M")
  x <- read_chemstation_method(path, what = c("pump", "column"))
  expect_equal(x$pump$gradient,
               data.frame(time_min = c(0, 10), pct_A = 0, pct_B = 0,
                          pct_C = c(100, 0), pct_D = c(0, 100)))
  expect_equal(x$pump$solvents$solvent[3:4], c("EtOAc", "80:20 ACN:H2O"))
  expect_equal(x$column$temp_controls$temperature_C, c(40, NA))
})

test_that("read_chemstation_method returns the gradient in long format", {
  path <- extra_test_file("chemstation_orange_ACQ.M")
  x <- suppressWarnings(read_chemstation_method(path, what = "pump",
                                                gradient_format = "long"))
  g <- x$pump$gradient
  expect_named(g, c("time_min", "channel", "percent"))
  expect_equal(unique(g$channel), c("A", "B", "flow"))
  expect_equal(g$percent[g$channel == "B"], c(10, 99, 99, 10))
})

test_that("read_chemstation_method finds the method in a `.D` directory", {
  d <- file.path(withr::local_tempdir(), "RUN.D")
  dir.create(file.path(d, "ACQ.M"), recursive = TRUE)
  file.copy(list.files(extra_test_file("chemstation_orange_ACQ.M"),
                       full.names = TRUE), file.path(d, "ACQ.M"))
  x <- read_chemstation_method(d, what = "pump")
  expect_equal(x$pump$flow_mL_min, 0.75)

  empty <- file.path(withr::local_tempdir(), "EMPTY.D")
  dir.create(empty)
  expect_error(read_chemstation_method(empty), "no `ACQ.M` or `RUN.M`")
  pump_only <- file.path(withr::local_tempdir(), "PUMP.M")
  dir.create(pump_only)
  file.copy(file.path(extra_test_file("chemstation_orange_ACQ.M"), "LPMP1.REG"),
            pump_only)
  expect_error(read_chemstation_method(pump_only, what = "sampler"),
               "none of the requested modules")
  expect_warning(read_chemstation_method(pump_only, what = c("pump", "dad")),
                 "No method settings for `dad`")
})

test_that("read_chemstation_method reads RapidControl XML methods", {
  path <- extra_test_file("chemstation_rapidcontrol_RUN.M")
  x <- read_chemstation_method(path)
  expect_named(x, c("pump", "dad", "autosampler", "column"))
  expect_equal(x$pump$solvents,
               data.frame(channel = c("A", "B"), percentage = c(95, 5),
                          solvent = c("H20", "ACN")))
  expect_equal(x$pump$gradient,
               data.frame(time_min = c(0, 2, 2.5, 2.6), pct_A = c(95, 0, 0, 95),
                          pct_B = c(5, 100, 100, 5), flow_mL_min = 0.5))
  expect_equal(x$dad$signals,
               data.frame(id = c("A", "B", "C"),
                          wavelength_nm = c(210, 230, 250), bandwidth_nm = 4,
                          reference_nm = 425, reference_bandwidth_nm = 50))
  expect_equal(x$column$temp_controls$temperature_C, c(25, 25))
})

test_that("register and RapidControl methods return the same fields", {
  reg <- read_chemstation_method(extra_test_file("chemstation_orange_ACQ.M"))
  rapid <- read_chemstation_method(
    extra_test_file("chemstation_rapidcontrol_RUN.M"))
  for (module in names(reg)){
    expect_named(rapid[[module]], names(reg[[module]]))
  }
  expect_named(rapid$dad$signals, names(reg$dad$signals))
  expect_named(rapid$column$temp_controls, names(reg$column$temp_controls))
})

test_that("read_chemstation_method warns about a data analysis method", {
  d <- file.path(withr::local_tempdir(), "DA.M")
  dir.create(d)
  file.copy(list.files(extra_test_file("chemstation_orange_ACQ.M"),
                       full.names = TRUE), d)
  expect_warning(read_chemstation_method(d, what = "pump"), "data analysis")
})

test_that("read_chemstation_reg resolves repeated names and reads tables", {
  path <- file.path(extra_test_file("chemstation_orange_ACQ.M"), "LPMP1.REG")
  x <- read_chemstation_reg(path)
  expect_false(any(grepl("^#@", x$conditions$key)))
  expect_true(all(paste0("CONTACT_", 1:4) %in% x$conditions$key))
  expect_named(x$tables$TIMETABLE,
               c("time", "solv_B", "solv_C", "solv_D", "flow", "pressure"))
  expect_true(all(is.na(x$tables$TIMETABLE$pressure)))
})
