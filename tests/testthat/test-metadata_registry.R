# The field maps in R/metadata_*.R are reached through `.metadata_maps`. These
# tests pin that table against the functions it names and against the
# vocabulary, so a new format cannot quietly invent a field name -- which is how
# the vocabulary drifted before it was written down in one place.

test_that("every registry entry names a live field map", {
  reg <- .metadata_maps
  ns <- asNamespace("chromConverter")
  expect_gt(length(reg), 0)
  expect_false(anyDuplicated(names(reg)) > 0)
  for (tag in names(reg)){
    expect_true(is.character(reg[[tag]]),
                label = sprintf("%s names a function", tag))
    expect_true(exists(reg[[tag]], envir = ns, mode = "function"),
                label = sprintf("%s map %s exists", tag, reg[[tag]]))
    fn <- metadata_map(tag)
    expect_named(formals(fn), c("meta", "ctx"))
  }
})

test_that("every metadata tag a reader can produce has a field map", {
  # the tags the readers pass as `format_in`, harvested from their calls to
  # `check_metadata_format`
  tags <- unlist(lapply(list.files(test_path("../../R"), pattern = "\\.R$",
                                   full.names = TRUE), function(f){
    hits <- grep("check_metadata_format\\(metadata_format, \"", readLines(f),
                 value = TRUE)
    sub('.*check_metadata_format\\(metadata_format, "([^"]+)".*', "\\1", hits)
  }))
  skip_if(length(tags) == 0, "could not locate the package sources")
  tags <- setdiff(unique(tags), "raw")   # `raw` is handled before dispatch
  expect_true(all(tags %in% names(.metadata_maps)),
              info = paste("tags without a map:",
                           paste(setdiff(tags, names(.metadata_maps)),
                                 collapse = ", ")))
})

test_that("an unknown format returns the data with a warning", {
  x <- matrix(1:4, 2)
  expect_null(metadata_map("nonsense"))
  expect_warning(
    r <- attach_metadata(x, meta = list(),
           format_in = "nonsense", format_out = "matrix", data_format = "wide",
           parser = "chromconverter",
           source_file = test_path("testdata/dad1.uv")),
    "could not be interpreted")
  expect_equal(dim(r), c(2L, 2L))
  expect_match(attr(r, "source_sha1"), "^[0-9a-f]{40}$")
  expect_equal(attr(r, "parser"), "chromconverter")
})

test_that("finalize_metadata stamps provenance and honors the caller", {
  ctx <- list(source_file = test_path("testdata/dad1.uv"),
              source_file_format = "chemstation_31", format_out = "matrix",
              data_format = "wide", parser = "entab", scale = FALSE)
  r <- finalize_metadata(matrix(1:4, 2),
                                          list(instrument = "LC"), ctx)
  expect_equal(attr(r, "instrument"), "LC")
  expect_equal(attr(r, "source_file_format"), "chemstation_31")
  expect_equal(attr(r, "format_out"), "matrix")
  # `scale` was accepted by `attach_metadata` and dropped by 13 of the 20 field
  # maps; it is now recorded whenever the caller passes one
  expect_false(attr(r, "scaled"))

  # the caller knows which parser read the file, so its value wins over a
  # field map's default
  r2 <- finalize_metadata(matrix(1:4, 2),
          list(parser = "chromconverter"), ctx)
  expect_equal(attr(r2, "parser"), "entab")
  # and a map's default applies when the reader passes none
  r3 <- finalize_metadata(matrix(1:4, 2),
          list(parser = "chromconverter"), utils::modifyList(ctx, list(parser = NULL)))
  expect_equal(attr(r3, "parser"), "chromconverter")

  # a `NULL` from a field map means "not recorded", not "delete what is there"
  r4 <- finalize_metadata(structure(matrix(1:4, 2), keep = "yes"),
          list(instrument = NULL), ctx)
  expect_equal(attr(r4, "keep"), "yes")
  expect_null(attr(r4, "instrument"))
})

test_that("validate_metadata_names flags a name outside the vocabulary", {
  old <- options(chromConverter.validate_metadata = TRUE)
  on.exit(options(old))
  expect_warning(validate_metadata_names(list(nonsense = 1)),
                 "Unrecognized metadata field")
  # canonical fields and the documented format-specific ones are both accepted
  expect_silent(validate_metadata_names(
    list(instrument = 1, n_scans = 2, ms_params = list(), metadata = list())))
})

test_that("a second pass of metadata does not overwrite the first with `NA`", {
  # `read_thermoraw` reads the mzML it exports and then annotates it from the
  # `-metadata.txt` file written with it, so `meta_thermoraw` is applied on top
  # of `meta_mzml`. Twelve
  # fields are set by both, and the text file does not carry all of them.
  ctx <- list(source_file = test_path("testdata/dad1.uv"),
              source_file_format = "thermoraw", format_out = "matrix",
              data_format = "long", parser = NULL, scale = NULL)
  from_mzml <- structure(matrix(1:4, 2), time_range = c(0.017, 29.99),
                         detector = "MS", time_unit = "Minutes")
  sidecar <- list(`RAW file path` = "/tmp/small.RAW",
                  `Creation date` = "07/20/2005 14:44:22")

  r <- finalize_metadata(
    from_mzml, meta_thermoraw(sidecar, ctx), ctx)

  # absent from the text file, so the mzML values stand
  expect_equal(attr(r, "time_range"), c(0.017, 29.99))
  expect_equal(attr(r, "detector"), "MS")
  # present in the text file, so they win
  expect_equal(attr(r, "sample_name"), "small")
  expect_equal(attr(r, "run_datetime"),
               as.POSIXct("2005-07-20 14:44:22", tz = "UTC"))

  # the field maps really do overlap, so the guarantee above is load-bearing
  overlap <- intersect(names(meta_mzml(list(), ctx)),
                       names(meta_thermoraw(sidecar, ctx)))
  expect_gt(length(overlap), 1)
})
