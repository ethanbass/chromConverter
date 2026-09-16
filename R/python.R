#' Python packages required by each parser
#'
#' `"default"` is the set declared in `.onLoad`. The 'aston' requirements are
#' deliberately excluded from it and declared lazily instead (see
#' `check_aston_configuration`), so that 'Aston' and 'pandas' are only
#' provisioned for the users who call `sp_converter`.
#'
#' Note that scipy is deliberately absent. 'Aston' imports
#' `scipy.io.netcdf.NetCDFFile`, which was removed in scipy v1.14, but only
#' from its generic `TraceFile` dispatcher. `sp_converter` uses
#' `aston.tracefile.agilent_uv.AgilentDAD` directly and never touches that
#' code path, so no scipy constraint is needed. This matters because a pin is
#' resolved by 'uv' *when Python starts*: declaring `scipy<1.14` after another
#' parser has already initialized Python would resolve an environment with an
#' older numpy while the newer numpy stayed loaded in the running session.
#' @noRd
get_parser_reqs <- function(parser){
  switch(tolower(parser),
         "aston" = c("Aston", "pandas", "numpy"),
         "olefile" = "olefile",
         "rainbow" = c("numpy", "rainbow-api>=1.5.0"),
         "default" = c("numpy", "olefile", "rainbow-api>=1.5.0"),
         "all" = c("Aston", "pandas", "numpy", "olefile",
                   "rainbow-api>=1.5.0"))
}

#' Parsers other than the one requiring `module` that can read `format_in`
#'
#' Derived from the registry rather than asserted at the call site, so the
#' suggestion made by `check_py_module` cannot drift from the parsers that
#' actually support the format. The parser requiring a module is named after it
#' ('rainbow' and 'aston' both), so `module` is what gets excluded. 'entab' is
#' dropped when it isn't installed, since it could not be selected.
#' @noRd
alternative_parsers <- function(module, format_in){
  if (is.null(format_in) || length(format_in) != 1L) return(character())
  out <- setdiff(parsers_for_format(format_in, parser_formats()), module)
  if (!requireNamespace("entab", quietly = TRUE)) out <- setdiff(out, "entab")
  out
}

#' PyPI distribution providing a module
#'
#' The name a module is imported under is not always the name it is installed
#' under: `import rainbow` comes from the 'rainbow-api' distribution, and
#' 'rainbow' on PyPI is an unrelated package. Used to make the installation
#' instructions in `check_py_module` actionable.
#' @noRd
py_distribution <- function(module){
  switch(module,
         "rainbow" = "rainbow-api",
         "aston" = "Aston",
         module)
}

#' Package-local state (deprecation warnings, etc.)
#' @noRd
pkg_state <- new.env(parent = emptyenv())

#' Check whether Python has already been initialized
#'
#' Unlike most 'reticulate' predicates, this never triggers initialization of
#' the Python session.
#' @noRd
py_initialized <- function(){
  isTRUE(reticulate::py_available(initialize = FALSE))
}

#' Initialize Python, falling back on the 'uv' cache when offline
#'
#' Since 'reticulate' v1.41, the Python packages declared with `py_require`
#' are provisioned into an ephemeral 'uv' environment the first time Python is
#' initialized. By default 'uv' contacts the Python package index to resolve
#' those requirements, so initialization fails when there is no internet
#' connection, even if a suitable environment is already cached locally.
#'
#' If the first attempt fails, this function retries once with `UV_OFFLINE=1`
#' so that a cached environment can be reused. The retry is deliberately not
#' conditioned on the error text: reticulate reports a generic "Installation
#' of Python not found" condition and writes its 'uv' diagnostics straight to
#' the stderr connection, so the R-level error cannot distinguish a network
#' failure from a version conflict. The retry is cheap, and it can only
#' succeed when a cached environment genuinely satisfies the requirements (a
#' real conflict fails offline too), so the fallback is only *reported* as an
#' offline fallback when it actually works. Otherwise reticulate's own error
#' is passed through.
#'
#' @param error Logical. Whether to throw an error (`TRUE`, the default) or
#' return `FALSE` if Python cannot be initialized.
#' @return Logical. `TRUE` if Python is available.
#' @noRd
init_python <- function(error = TRUE){
  if (py_initialized()) return(TRUE)
  first <- try_py_init()
  if (first$ok) return(TRUE)
  if (!nzchar(Sys.getenv("UV_OFFLINE"))){
    old <- Sys.getenv("UV_OFFLINE", unset = NA)
    Sys.setenv(UV_OFFLINE = "1")
    if (try_py_init()$ok){
      # `UV_OFFLINE` is deliberately left set for the rest of the session.
      # 'uv' is consulted again whenever new requirements are declared (e.g.
      # by `check_aston_configuration`), and those resolutions would go back
      # online and fail for the same reason this one did.
      pkg_state$uv_offline <- TRUE
      message("Could not reach the Python package index. Falling back on the ",
              "cached Python environment. `UV_OFFLINE` has been set for the ",
              "rest of this session; call `Sys.unsetenv('UV_OFFLINE')` to try ",
              "the index again, or set `UV_OFFLINE=1` in your .Renviron to ",
              "skip this check in future sessions.")
      return(TRUE)
    }
    if (is.na(old)) Sys.unsetenv("UV_OFFLINE") else Sys.setenv(UV_OFFLINE = old)
  }
  if (error){
    stop("Could not initialize Python, which chromConverter requires for the ",
         "'rainbow', 'olefile' and 'aston' parsers. reticulate reported:\n  ",
         if (is.null(first$error)) "(no error condition)" else
           conditionMessage(first$error),
         "\n\nIf you are offline, the required packages must already be ",
         "cached locally, or you can point reticulate at an environment you ",
         "manage yourself (see `?configure_python_environment`). Call ",
         "`reticulate::py_config()` for the full diagnostics.", call. = FALSE)
  }
  FALSE
}

#' Attempt to initialize Python
#'
#' Returns the error condition alongside the outcome so that `init_python`
#' can report reticulate's own diagnosis rather than guessing at one.
#' @noRd
try_py_init <- function(){
  err <- NULL
  ok <- tryCatch({
    reticulate::py_config()
    py_initialized()
  }, error = function(e){
    err <<- e
    FALSE
  })
  list(ok = ok, error = err)
}

#' Require a Python module
#'
#' Initializes Python (if necessary) and checks that `module` can be imported,
#' throwing an informative error otherwise.
#'
#' @param module Name of the required Python module.
#' @param format_in Format being read. When supplied, and the registry lists
#' another parser that can read it, the error names that parser and points at
#' the `parser` argument of `read_chroms`. Omitted by the 'olefile' callers,
#' where the module backs the only parser for the format, so that the error
#' does not advertise an alternative that does not exist.
#' @noRd
check_py_module <- function(module, format_in = NULL){
  init_python()
  if (!reticulate::py_module_available(module)){
    alt <- alternative_parsers(module, format_in)
    alternative <- if (length(alt) == 0L) "" else
      sprintf(paste0(" The %s parser%s can also read this format; select %s ",
                     "with the `parser` argument of `read_chroms`."),
              paste0("'", alt, "'", collapse = " and "),
              if (length(alt) > 1L) "s" else "",
              if (length(alt) > 1L) "one" else "it")
    stop(sprintf(paste0("The '%s' Python module is required to read this ",
                        "format but could not be found in the active Python ",
                        "environment (%s). It can be installed with ",
                        "`reticulate::py_install(\"%s\")` or by creating a ",
                        "dedicated environment with ",
                        "`configure_python_environment()`.%s"),
                 module, reticulate::py_config()$python,
                 py_distribution(module), alternative),
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Cache of imported Python modules
#' @noRd
py_modules <- new.env(parent = emptyenv())

#' Import a Python module (cached)
#'
#' Initializes Python on first use and caches the resulting module reference in
#' the package namespace (rather than the global environment).
#' @noRd
py_import <- function(module){
  mod <- py_modules[[module]]
  if (is.null(mod)){
    init_python()
    mod <- reticulate::import(module)
    py_modules[[module]] <- mod
  }
  mod
}
