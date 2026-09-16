test_that("parser auto-detection does not initialize Python", {
  skip_if_not_installed("reticulate")
  skip_if(reticulate::py_available(initialize = FALSE),
          "Python is already initialized.")
  expect_false(check_parser("chemstation_ch", find = TRUE) %in%
                 c("aston", "rainbow"))
  expect_false(reticulate::py_available(initialize = FALSE))
})

test_that("auto-detection picks 'rainbow' for waters_raw without probing it", {
  skip_if_not_installed("reticulate")
  skip_if(reticulate::py_available(initialize = FALSE),
          "Python is already initialized.")
  # 'waters_raw' is the one format for which auto-detection prefers 'rainbow'
  # over the internal parser, so it is the only place the availability of the
  # Python module could have changed the outcome. It is no longer consulted: a
  # missing module is reported by `check_py_module` when the parser runs.
  expect_equal(check_parser("waters_raw", find = TRUE), "rainbow")
  expect_false(reticulate::py_available(initialize = FALSE))
})

test_that("check_py_module names the parsers that can read the format", {
  skip_if_not_installed("reticulate")
  local_mocked_bindings(init_python = function(...) TRUE)
  local_mocked_bindings(
    py_module_available = function(...) FALSE,
    py_config = function(...) list(python = "/fake/python"),
    .package = "reticulate")

  # 'waters_raw' is readable by 'chromconverter' too, and it is named
  err <- expect_error(check_py_module("rainbow", format_in = "waters_raw"))
  expect_match(conditionMessage(err), "chromconverter", fixed = TRUE)
  expect_match(conditionMessage(err), "select it with the `parser` argument",
               fixed = TRUE)
  # the distribution is 'rainbow-api'; 'rainbow' on PyPI is a different package
  expect_match(conditionMessage(err),
               "reticulate::py_install(\"rainbow-api\")", fixed = TRUE)

  # more than one alternative is pluralized
  skip_if_not_installed("entab")
  err <- expect_error(check_py_module("rainbow", format_in = "chemstation_uv"))
  expect_match(conditionMessage(err), "parsers can also read", fixed = TRUE)
  expect_match(conditionMessage(err), "select one with the `parser` argument",
               fixed = TRUE)
})

test_that("check_py_module offers no alternative when there is none", {
  skip_if_not_installed("reticulate")
  local_mocked_bindings(init_python = function(...) TRUE)
  local_mocked_bindings(
    py_module_available = function(...) FALSE,
    py_config = function(...) list(python = "/fake/python"),
    .package = "reticulate")

  # 'olefile' backs the only parser for the 'Shimadzu' OLE formats, so its
  # callers supply no `format_in` and the error advertises no other route
  err <- expect_error(check_py_module("olefile"))
  expect_false(grepl("`parser` argument", conditionMessage(err), fixed = TRUE))
  expect_match(conditionMessage(err), "configure_python_environment")

  # an unrecognized format yields no suggestion rather than an error
  expect_equal(alternative_parsers("rainbow", "not_a_format"), character())
  expect_equal(alternative_parsers("rainbow", NULL), character())
  # a non-scalar format_in is ignored rather than propagated into the registry
  expect_equal(alternative_parsers("rainbow", c("waters_raw", "agilent_d")),
               character())
})

test_that("init_python falls back on the uv cache when offline", {
  local_mocked_bindings(py_initialized = function() FALSE)
  attempts <- 0
  local_mocked_bindings(try_py_init = function(){
    attempts <<- attempts + 1
    list(ok = nzchar(Sys.getenv("UV_OFFLINE")), error = NULL)
  })
  old <- set_env_var("UV_OFFLINE", NA)
  on.exit(set_env_var("UV_OFFLINE", old), add = TRUE)
  expect_message(expect_true(init_python()),
                 "Could not reach the Python package index")
  expect_equal(attempts, 2)
  # UV_OFFLINE stays set for the rest of the session, so that later 'uv'
  # resolutions (e.g. the aston requirements) don't go back online and fail
  expect_equal(Sys.getenv("UV_OFFLINE"), "1")
})

test_that("init_python errors informatively when Python is unavailable", {
  local_mocked_bindings(py_initialized = function() FALSE)
  local_mocked_bindings(try_py_init = function(){
    list(ok = FALSE,
         error = simpleError("Installation of Python not found"))
  })
  old <- set_env_var("UV_OFFLINE", NA)
  on.exit(set_env_var("UV_OFFLINE", old), add = TRUE)
  err <- expect_error(init_python(), "Could not initialize Python")
  # reticulate's own diagnosis is passed through rather than guessed at
  expect_match(conditionMessage(err), "Installation of Python not found")
  expect_false(init_python(error = FALSE))
  # UV_OFFLINE is restored when the fallback does not help
  expect_equal(Sys.getenv("UV_OFFLINE"), "")
})

test_that("init_python does not retry when UV_OFFLINE is already set", {
  local_mocked_bindings(py_initialized = function() FALSE)
  attempts <- 0
  local_mocked_bindings(try_py_init = function(){
    attempts <<- attempts + 1
    list(ok = FALSE, error = simpleError("nope"))
  })
  old <- set_env_var("UV_OFFLINE", "1")
  on.exit(set_env_var("UV_OFFLINE", old), add = TRUE)
  expect_false(init_python(error = FALSE))
  expect_equal(attempts, 1)
})

test_that("aston requirements are not declared at load", {
  expect_false(any(grepl("Aston", get_parser_reqs("default"), ignore.case = TRUE)))
  expect_false(any(grepl("pandas", get_parser_reqs("default"))))
})

test_that("no scipy constraint is declared anywhere", {
  # `sp_converter` is the only remaining aston binding and it does not go
  # through `TraceFile`, so nothing needs `scipy < 1.14`. A pin declared after
  # another parser has already started Python would resolve an environment
  # with an older numpy while the newer numpy stayed loaded.
  for (p in c("default", "aston", "rainbow", "olefile", "all")){
    expect_false(any(grepl("scipy", get_parser_reqs(p))), label = p)
  }
})

test_that("auto-detection only falls back on aston as a last resort", {
  # aston is ranked last, so it loses to every other parser
  expect_equal(rank_parsers(c("aston", "entab")), c("entab", "aston"))
  expect_equal(rank_parsers(c("aston", "rainbow", "chromconverter")),
               c("chromconverter", "rainbow", "aston"))
  expect_equal(rank_parsers("aston"), "aston")
  # unknown parsers are dropped
  expect_equal(rank_parsers(c("aston", "nonesuch")), "aston")

  skip_if_not_installed("entab")
  expect_equal(check_parser("masshunter_dad", find = TRUE), "entab")
  expect_equal(check_parser("other", find = TRUE), "entab")
  # but aston can still be requested explicitly for the format it can read
  expect_equal(check_parser("masshunter_dad", parser = "aston", find = FALSE),
               "aston")
  # ... and no longer for the formats whose bindings were retired
  expect_error(check_parser("chemstation_uv", parser = "aston", find = FALSE),
               "Mismatched arguments")
  expect_error(check_parser("other", parser = "aston", find = FALSE),
               "Mismatched arguments")
})

test_that("uv_converter is defunct", {
  expect_error(uv_converter("nonexistent.uv"), "defunct")
  expect_error(uv_converter("nonexistent.uv"), "read_chemstation_uv")
})

test_that("parsers are matched by exact format, not by substring", {
  fmts <- list(chromconverter = c("chemstation", "chemstation_ch"),
               rainbow = "chemstation")
  expect_equal(parsers_for_format("chemstation", fmts),
               c("chromconverter", "rainbow"))
  expect_equal(parsers_for_format("chemstation_ch", fmts), "chromconverter")
  expect_equal(parsers_for_format("nope", fmts), character(0))

  # `format_in = "chemstation"` must accept the parser auto-detection picks
  auto <- check_parser("chemstation", find = TRUE)
  expect_equal(check_parser("chemstation", parser = auto, find = FALSE), auto)
})

test_that("stop_no_parser explains how to install entab", {
  fmts <- list(chromconverter = c("cdf"), entab = c("masshunter_dad"),
               aston = c("masshunter_dad"))
  err <- expect_error(stop_no_parser("masshunter_dad", fmts))
  expect_match(conditionMessage(err), "No parser is available")
  if (!requireNamespace("entab", quietly = TRUE)){
    expect_match(conditionMessage(err), "ethanbass.github.io/drat", fixed = TRUE)
  }
  expect_error(stop_no_parser("not_a_format", fmts), "is not supported")
})

test_that("the aston deprecation warning fires once per session", {
  pkg_state$aston_deprecation_warned <- NULL
  expect_warning(warn_aston_deprecated(), "deprecated")
  expect_silent(warn_aston_deprecated())
  pkg_state$aston_deprecation_warned <- NULL
})

test_that("get_parser_reqs declares the expected constraints", {
  # a lower bound, not an upper bound: v1.5.0 is the first release whose
  # `bin_width`/`display_precision` API chromConverter targets
  expect_true("rainbow-api>=1.5.0" %in% get_parser_reqs("rainbow"))
  expect_true("rainbow-api>=1.5.0" %in% get_parser_reqs("all"))
  expect_true("rainbow-api>=1.5.0" %in% get_parser_reqs("default"))
  expect_true(all(c("Aston", "pandas") %in% get_parser_reqs("aston")))
  expect_equal(get_parser_reqs("olefile"), "olefile")
})

test_that("rb_precision_args maps `precision` onto rainbow's bin grid", {
  expect_equal(rb_precision_args(1), list(bin_width = 0.1,
                                          display_precision = 1L))
  expect_equal(rb_precision_args(2), list(bin_width = 0.01,
                                          display_precision = 2L))
  # `precision = 0` maps onto rainbow's own defaults for `parse_file()`
  expect_equal(rb_precision_args(0), list(bin_width = 1,
                                          display_precision = 0L))
})
