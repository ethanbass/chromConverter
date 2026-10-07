test_that("asm_peak_table handles plain values, quantities and missing fields", {
  peaks <- list(
    list(identifier = "a", `retention time` = list(unit = "s", value = 10),
         `peak start` = list(unit = "s", value = 9),
         `peak area` = list(unit = "pA.s", value = 100),
         `peak height` = list(unit = "pA", value = 5),
         `relative peak area` = list(unit = "%", value = "NaN"),
         `written name` = "Hexane"),
    list(identifier = "b", `retention time` = list(unit = "s", value = 20),
         `peak area` = list(value = 200),
         `peak height` = list(unit = "pA", value = 7))
  )
  x <- asm_peak_table(peaks)
  expect_equal(names(x), c("rt", "start", "end", "area", "height"))
  expect_equal(x$rt, c(10, 20))
  expect_equal(x$start, c(9, NA))
  expect_equal(x$end, c(NA_real_, NA_real_))
  expect_equal(x$area, c(100, 200))
  expect_equal(attr(x, "units"), list(time = "s", y = "pA"))

  y <- asm_peak_table(peaks, peaktable_format = "original")
  expect_equal(y$identifier, c("a", "b"))
  expect_equal(y[["written name"]], c("Hexane", NA))
  expect_true(is.nan(y[["relative peak area"]][1]))
})

test_that("asm_peak_table uses retention volume when there is no retention time", {
  peaks <- list(list(`retention volume` = list(unit = "mL", value = 9.13),
                     `peak height` = list(unit = "S/m", value = 0.03)))
  x <- asm_peak_table(peaks)
  expect_equal(x$rt, 9.13)
  expect_equal(attr(x, "units"), list(time = "mL", y = "S/m"))
})

test_that("find_asm_peak_lists finds peak lists at any depth outside data cubes", {
  pl <- list(peak = list(list(identifier = "a")))
  md <- list(
    `chromatogram data cube` = list(`peak list` = pl),
    `processed data aggregate document` = list(
      `processed data document` = list(list(`peak list` = pl)))
  )
  expect_length(find_asm_peak_lists(md), 1)
  expect_length(find_asm_peak_lists(list(`peak list` = pl)), 1)
  expect_length(find_asm_peak_lists(list(`sample document` = list())), 0)
})

test_that("read_asm_ms_data reads flat and per-scan points", {
  cube <- function(dims, points){
    list(data = list(dimensions = list(as.list(dims), list(length = 3)),
                     points = points))
  }
  flat <- cube(c(1, 2), list(list(1, 50, 10), list(1, 51, 11), list(2, 50, 12)))
  expect_equal(read_asm_ms_data(flat),
               data.frame(rt = c(1, 1, 2), mz = c(50, 51, 50),
                          intensity = c(10, 11, 12)))

  scans <- cube(c(1, 2, 3),
                list(list(list(1000, 50, 10), list(1000, 51, NULL)), list(),
                     list(list(3000, 50, 12))))
  x <- read_asm_ms_data(scans)
  expect_equal(x$rt, c(1, 1, 3))
  expect_equal(x$intensity, c(10, NA, 12))

  expect_null(read_asm_ms_data(cube(1, list(list(0)))))
})

test_that("find_asm_cube only takes chromatograms and mass spectra over time", {
  dim <- function(concept) list(concept = concept, unit = "u")
  cube <- function(...) list(`cube-structure` = list(dimensions = list(...)))
  md <- list(`mass spectrum data cube` = cube(dim("m/z")),
             `chromatogram data cube` = cube(dim("retention time")),
             `ms cube data cube` = cube(dim("retention time"), dim("m/z")))
  expect_equal(find_asm_cube(md, 1), "chromatogram data cube")
  expect_equal(find_asm_cube(md, 2), "ms cube data cube")
  expect_true(is.na(find_asm_cube(md["mass spectrum data cube"], 1)))
  dad <- list(`dad data cube` = cube(dim("retention time"), dim("wavelength")))
  expect_true(is.na(find_asm_cube(dad, 2)))
})

test_that("get_asm_measurements groups measurements by injection", {
  md <- function(id) list(`injection document` = list(`injection identifier` = id))
  per_document <- list(
    list(analyst = "a", `measurement aggregate document` =
           list(`measurement document` = list(md("1"), md("1")))),
    list(analyst = "a", `measurement aggregate document` =
           list(`measurement document` = list(md("2")))))
  meas <- get_asm_measurements(per_document)
  expect_equal(vapply(meas, `[[`, character(1), "injection"), c("1", "1", "2"))
  expect_equal(meas[[1]]$doc, list(analyst = "a"))

  in_document <- list(list(
    `injection document` = list(`injection identifier` = "x"),
    `measurement aggregate document` =
      list(`measurement document` = list(list(), list()))))
  meas <- get_asm_measurements(in_document)
  expect_equal(vapply(meas, `[[`, character(1), "injection"), c("x", "x"))
})

test_that("name_asm_samples names injections by sample and keeps names unique", {
  m <- function(written, id, injection){
    smp <- list(`written name` = written, `sample identifier` = id)
    list(list(md = list(`sample document` = smp[!vapply(smp, is.null, NA)]),
              doc = list(), injection = injection))
  }
  groups <- list(m("Blank", "B1", "1"), m(NULL, "Std", "2"), m(NULL, "Std", "3"),
                 m(NULL, NULL, "4"))
  expect_equal(name_asm_samples(groups), c("Blank", "Std", "Std_1", "4"))
})

test_that("flatten_asm flattens nested documents and value/unit pairs", {
  x <- list(`written name` = "S1",
            `injection volume setting` = list(value = 5, unit = "uL"),
            `custom information document` = list(vial = "1"),
            `pressure data cube` = list(data = 1))
  expect_equal(flatten_asm(x),
               list(`written name` = "S1", `injection volume setting` = 5,
                    `injection volume setting unit` = "uL",
                    `custom information document.vial` = "1"))
  expect_equal(flatten_asm(NULL), list())
})

test_that("splice_samples replaces multi-sample files with their samples", {
  seq <- structure(list(s1 = 1, s2 = 2), class = "chrom_list")
  expect_equal(splice_samples(list(seq, 3), c("file1", "file2")),
               list(s1 = 1, s2 = 2, file2 = 3))
  expect_equal(splice_samples(list(3, 4), c("a", "b")), list(a = 3, b = 4))
})

test_that("asm_dimension_values expands functions", {
  expect_equal(asm_dimension_values(c(1, 2, 4)), c(1, 2, 4))
  expect_equal(asm_dimension_values(list(start = 0, incr = 0.5, length = 3)),
               c(0, 0.5, 1))
  expect_equal(asm_dimension_values(list(length = 3)), c(1, 2, 3))
})

test_that("find_asm_traces finds diagnostic and device control traces", {
  cube <- function(label, concept = "retention volume", values = 1:2){
    list(label = label,
         `cube-structure` = list(dimensions = list(list(concept = concept)),
                                 measures = list(list(concept = "x"))),
         data = list(dimensions = list(as.list(values)),
                     measures = list(as.list(values))))
  }
  diagnostic <- list(`diagnostic trace document` = list(
    list(description = "pH", `pH data cube` = cube("pH data cube"))))
  md <- list(`diagnostic trace aggregate document` = diagnostic,
             `device control aggregate document` = list(
               `device control document` = list(list(
                 `system pressure data cube` = cube("System pressure"),
                 `spectrum data cube` = cube("spectrum", concept = "wavelength"),
                 `flow data cube` = cube("flow"),
                 `other flow data cube` = cube("flow", values = 3:4)))))
  doc_level <- list(`diagnostic trace aggregate document` = list(
    `diagnostic trace document` = list(
      list(description = "temperature", `data cube` = cube("t")))))
  meas <- list(list(md = md, doc = doc_level), list(md = md, doc = list()))
  traces <- find_asm_traces(meas)
  expect_equal(names(traces), c("pH", "temperature", "System pressure", "flow",
                                "flow 1"))
})

test_that("minify_asm strips whitespace without changing the contents", {
  path <- withr::local_tempfile(fileext = ".json")
  writeLines(c('{', '  "written name": "S 1 µg",', '  "points": [', '    [', '      3502.856,',
               '      30.0', '    ]', '  ]', '}'), path, useBytes = TRUE)
  out <- withr::local_tempfile(fileext = ".json")
  minify_asm(path, out)
  expect_equal(readLines(out, encoding = "UTF-8"),
               '{"written name":"S 1 µg","points":[[3502.856,30.0]]}')
  expect_equal(jsonlite::fromJSON(out), jsonlite::fromJSON(path))
})
