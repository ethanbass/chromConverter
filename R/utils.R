# global variables for data.table operations
utils::globalVariables(names = c('.', "rt", "mz", "intensity"))

#' Check Format Out Argument
#' @noRd
check_format_out <- function(format_out){
  match.arg(format_out, c("matrix", "data.frame", "data.table"))
}

#' Check Data Format Argument
#'
#' Make sure that \code{data_format} argument is "long" when \code{format_out} is
#' \code{"data.table"}.
#' @noRd

check_data_format <- function(data_format, format_out){
  if (format_out == "data.table"){
    data_format <- "long"
  }
  match.arg(data_format, c("wide", "long"))
}

#' Convert chromatogram format
#' @author Ethan Bass
#' @noRd
convert_chrom_format <- function(x, format_out, data_format=NULL){
  if (is.null(data_format)){
    data_format <- attr(x, "data_format")
  }
  if (inherits(x, format_out)){
    return(x)
  } else if (format_out == "matrix"){
    return(as.matrix(x))
  } else if (format_out == "data.frame"){
    return(as.data.frame(x))
  } else if (format_out == "data.table"){
    return(data.table::as.data.table(x, keep.rownames = ifelse(data_format == "wide",
                                                               yes = "rt", no = FALSE))
    )
  }
}

#' Format 2D chromatogram
#' @noRd
format_2d_chromatogram <- function(rt, int, data_format, format_out){
  data_format <- match.arg(data_format, c("wide", "long"))
  format_out <- match.arg(format_out, c("matrix", "data.frame", "data.table"))
  if (data_format == "wide" && any(duplicated(rt))){
    stop("Some row names are duplicated. Please use `long` format instead.")
  }
  if (data_format == "long"){
    dat <- data.frame(rt = rt, intensity = int)
  } else{
    dat <- data.frame(intensity = int, row.names = rt)
  }
  if (format_out == "matrix"){
    dat <- as.matrix(dat)
  } else if (format_out == "data.table"){
    dat <- data.table::as.data.table(dat, keep.rownames = ifelse(data_format == "wide",
                                                        yes = "rt", no = FALSE))
  }
  dat
}

#' Get filetype
#' @noRd
get_filetype <- function(path, out = c("format_in", "filetype")){
  out <- match.arg(out, c("format_in", "filetype"))
  f <- file(path, "rb")
  on.exit(close(f))

  magic <- readBin(f, what = "raw", n = 4)
  magic <- paste(paste0("x", as.character(magic)), collapse = "/")
  # magic
  filetype <- switch(magic,
                     "x01/x32/x00/x00" = "chemstation_ms",
                     "x02/x02/x00/x00" = "AgilentMasshunterDADHeader",
                     "x02/x32/x00/x00" = "chemstation_mwd",
                     # "x02/x33/x00/x00" = "AgilentChemstationMWD",
                     # "x03/x31/x00/x00" = "AgilentChemstationMWD2"
                     # "x01/x32/x00/x00" = "AgilentChemstationMS"
                     "x03/x02/x00/x00" = "masshunter_dad",
                     "x02/x33/x30/x00" = "chemstation_30",
                     "x02/x33/x31/x00" = "chemstation_31",
                     "x03/x31/x33/x30" = "chemstation_130", #130
                     "x03/x31/x33/x31" = "chemstation_131", #131
                     "x03/x31/x37/x39" = "chemstation_179", #179
                     "x02/x38/x31/x00" = "chemstation_81", #81
                     "x01/x38/x00/x00" = "chemstation_8", #81
                     "x03/x31/x38/x31" = "chemstation_181", #181
                     "x01/xa1/x46/x00" = "ThermoRAW",
                     "xd0/xcf/x11/xe0" = "shimadzu_ole",
                     "x1c/x00/x09/x03" = "varian_sms",
                     "x80/x00/x01/x00" = "waters_raw",
                     "x43/x44/x46/x01" = "cdf",
                     "x50/x4b/x03/x04" = "zip"
  )
  if (filetype == "zip" && fs::path_ext(path) == "dx"){
    filetype <- "agilent_dx"
  }
  if (is.null(filetype)){
    stop("File type not recognized. Please specify a filetype by providing an argument to `format_in`
          or file an issue at `https://github.com/ethanbass/chromConverter/issues`.")
  }
  if (filetype == "chemstation_131"){
    seek(f, 348)
    magic2 <- readBin(f, what="character", n = 2)
    magic2 <- paste(magic2, collapse = "")
    filetype <- switch(magic2, "OL" = "openlab_131",
                   "LC" = "chemstation_131")
  } else if (filetype == "shimadzu_ole"){
    filetype <- paste("shimadzu", tolower(fs::path_ext(path)),sep = "_")
    # fp <- read_sz_file_properties(path)
    # filetype <- switch(fp$FileProperty.dwFileType,
    #        "67108895" = "shimadzu_lcd",
    #        "67108975" = "shimadzu_gcd")
  }
  format_in <- switch(filetype,
                      "AgilentChemstationMS" = "chemstation_ms",
                      "AgilentChemstationCH" = "chemstation_ch",
                      "AgilentChemstationFID" = "chemstation_ch",
                      # "chemstation_31" = "chemstation_uv",
                      # "chemstation_131" = "chemstation_uv",
                      # "openlab_131" = "chemstation_uv",
                      "ThermoRAW" = "thermoraw",
                      "VarianSMS" = "varian_sms",
                      filetype
  )
  switch(out, "filetype" = filetype, "format_in" = format_in)
}

#' Check parser
#' @noRd
check_parser <- function(format_in, parser = NULL, find = FALSE){
  allowed_formats <- list(openchrom = c("msd", "csd", "wsd"),
                          chromconverter = c("agilent_d", "agilent_dx", "asm",
                                             "cdf", "chemstation_csv",
                                             "chemstation_ch", "chemstation_fid",
                                             "chemstation_uv", "chromeleon_uv",
                                             "chemstation_2", "chemstation_ms",
                                             "chemstation_30", "chemstation_31",
                                             "chemstation_130", "chemstation_131",
                                             "openlab_131", "chemstation_179",
                                             "chemstation_81", "chemstation_181",
                                             "mzml", "mzxml", "mdf",
                                             "shimadzu_ascii", "shimadzu_dad",
                                             "shimadzu_fid", "shimadzu_gcd",
                                             "shimadzu_qgd", "shimadzu_lcd",
                                             "varian_sms",
                                             "waters_arw", "waters_raw",
                                             "waters_chro"),
                          aston = c("chemstation_uv", "chemstation_131",
                                    "masshunter_dad", "other"),
                          entab = c("chemstation_ms", "chemstation_mwd",
                                    "chemstation_ch",
                                    "chemstation_30", "chemstation_31",
                                    "chemstation_131", "chemstation_fid",
                                    "chemstation_uv", "masshunter_dad",
                                    "thermoraw", "other"),
                          rainbow = c("chemstation", "chemstation_ms",
                                      "chemstation_ch",
                                      "chemstation_130","chemstation_131",
                                      "chemstation_fid", "chemstation_179",
                                      "chemstation_uv", "waters_raw",
                                      "agilent_d"),
                          thermoraw = c("thermoraw")
  )
  if (find){
    if (!reticulate::py_module_available("aston")){
      allowed_formats <-
        allowed_formats[-which(names(allowed_formats) == "aston")]
    }
    if (!reticulate::py_module_available("rainbow")){
      allowed_formats <-
        allowed_formats[-which(names(allowed_formats) == "rainbow")]
    }
    if (!requireNamespace("entab", quietly = TRUE)){
      allowed_formats <-
        allowed_formats[-which(names(allowed_formats) == "entab")]
    }
    possible_parsers <- names(allowed_formats)[grep(format_in, allowed_formats)]
    if (length(possible_parsers) > 1){
      if (format_in == "waters_raw"){
        possible_parsers <- c("rainbow")
      } else{
        possible_parsers <- possible_parsers[match(
          c("thermoraw", "entab", "chromconverter", "rainbow", "aston"),
          possible_parsers)]
        if (any(is.na(possible_parsers))){
          possible_parsers <- possible_parsers[-which(is.na(possible_parsers))]
        }
      }
    }
    parser <- possible_parsers[1]
  } else{
    if (!(format_in %in% allowed_formats[[tolower(parser)]])){
      stop("Mismatched arguments!", "\n\n", "The ",
           paste0(sQuote(format_in), " format can be converted using the following parsers: ",
        paste(sQuote(names(allowed_formats)[grep(format_in, allowed_formats)]),
              collapse = ", "), ". \n \n",
        "The ", sQuote(parser), " parser can take the following formats as inputs: \n",
                                    paste(sQuote(allowed_formats[[parser]]),
                                          collapse=", "), ". \n \n",
        "Please double check your arguments and try again."))
    }
  }
  if (parser == "entab" & !requireNamespace("entab", quietly = TRUE)) {
    stop("The entab R package must be installed to use entab parsers:
      install.packages('entab', repos='https://ethanbass.github.io/drat/')",
         call. = FALSE)
  }
  return(parser)
}

#' Remove unicode characters
#' @noRd
remove_unicode_chars <- function(x){
  stringr::str_replace_all(x, "\xb5", "micro")
}

#' Extract file names
#' @noRd
extract_filenames <- function(files){
  if (all(grepl("\\.[Dd]$|\\.[Dd]?[/\\\\]", files))){
    file_names <- strsplit(files, "/")
    file_names <- gsub("\\.[Dd]", "",
                       sapply(file_names, function(n){
                         ifelse(any(grepl("\\.[Dd]", n)),
                                grep("\\.[Dd]", n, value = TRUE), tail(n, 1))
                       }))
  } else {
    file_names <- sapply(strsplit(basename(files),"\\."), function(x) x[1])
  }
  file_names
}

#' Format extension
#' @noRd
format_to_extension <- function(format_in){
  switch(format_in,
         "agilent_d" = "\\.d$",
         "agilent_dx" = "\\.dx$",
         "chemstation_ms" = "\\.ms$",
         "chemstation_2" = "\\.ms$",
         "chemstation_uv" = "\\.uv$",
         "chemstation_31" = "\\.uv$",
         "chemstation_131" = "\\.uv$",
         "chemstation_ch" = "\\.ch$",
         "chemstation_fid" = "\\.ch$",
         "chemstation_179" = "\\.ch$",
         "chemstation_181" = "\\.ch$",
         "chemstation_81" = "\\.ch$",
         "chemstation_8" = "\\.ch$",
         "chemstation_30" = "\\.ch$",
         "chemstation_130" = "\\.ch$",
         "chemstation_csv" = "\\.csv$",
         "masshunter_dad" = "\\.sp$",
         "shimadzu_txt" = "\\.txt$",
         "shimadzu_fid" = "\\.txt$",
         "shimadzu_dad" = "\\.txt$",
         "shimadzu_lcd" = "\\.lcd$",
         "shimadzu_gcd" = "\\.gcd$",
         "shimadzu_qgd" = "\\.qgd",
         "chromeleon_uv" = "\\.txt$",
         "thermoraw" = "\\.raw$",
         "cdf" = "\\.cdf$",
         "mzml" = "\\.mzml$",
         "mzxml" = "\\.mzxml$",
         "varian_sms" = "\\.sms$",
         "waters_arw" = "\\.arw$",
         "waters_raw" = "\\.raw$",
         "msd" = "\\.",
         "csd" ="\\.",
         "wsd" ="\\.",
         "mdf" = "\\.mdf$",
         "other" = "\\.",
         "\\.")
}

#' Find files
#' @noRd
find_files <- function(paths, pattern){
  files <- unlist(lapply(paths, function(path){
    dirs <- ifelse(pattern %in% c("\\.raw$", "\\.d$"), TRUE, FALSE)
    files <- list.files(path = path, pattern = pattern, include.dirs = dirs,
                        full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
    if (length(files)==0){
      if (!dir.exists(path)){
        warning(paste0("The directory '", basename(path), "' does not exist."))
      } else{
        warning(paste0("No files matching the pattern '", pattern,
                       "' were found in '", basename(path), "'"))
      }
    }
    files
  }))
}

#' Set temp directory
#' @noRd
set_temp_directory <- function(){
  ans <- readline("Export directory not specified! Export files to `temp` directory (y/n)?")
  if (ans %in% c("y","Y")){
    fs::dir_create("temp")
    path_out <- fs::path(getwd(),"temp")
    path_out
  } else{
    stop("Must specify directory to export files.")
  }
}

#' Check for suggested package
#' @noRd
#' @keywords internal
check_for_pkg <- function(pkg, return_boolean = FALSE){
  pkg_exists <- requireNamespace(pkg, quietly = TRUE)
  if (return_boolean){
    return(pkg_exists)
  } else if (!pkg_exists) {
    stop(paste(
      "Package", sQuote(pkg), "must be installed to perform this action:
          try", paste0("`install.packages('", pkg, "')`.")),
      call. = FALSE
    )
  }
}

#' Choose apply function
#' @return Returns \code{\link[pbapply]{pblapply}} if \code{progress_bar == TRUE},
#' otherwise returns \code{\link{lapply}}.
#' @noRd
choose_apply_fnc <- function(progress_bar, parallel = FALSE, cl = NULL){
  if (progress_bar){
    check_for_pkg("pbapply")
    fn <- pbapply::pblapply
    if (!is.null(cl)){
      fn <- purrr::partial(fn, cl = cl)
    }
  } else{
    fn <- lapply
  }
  fn
}

#' Rename list
#' @author Ethan Bass
#' @noRd
rename_list <- function(x, new_names){
  old_names <- names(x)
  names.idx <- match(names(x), new_names)
  new_names <- names(new_names)[names.idx]
  not_found <- which(is.na(new_names))
  if (any(not_found)){
    new_names[not_found] <- old_names[not_found]
  }
  names(x) <- new_names
  x
}

#' Collapse list
#' @noRd
collapse_list <- function(x){
  while(inherits(x, "list") && length(x) == 1){
    x <- x[[1]]
  }
  x
}

#' Split vector by position
#' @note From https://stackoverflow.com/questions/16357962/r-split-numeric-vector-at-position
#' @noRd
split_at <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))

#' Configure python environment
#'
#' Configures python virtual environment or conda environment for parsers that
#' have python dependencies, according to the value of \code{what}. While this
#' should not be necessary in most cases starting with reticulate \code{v1.41.0},
#' this function can be used to create a dedicated chromConverter environment.
#'
#' @name configure_python_environment
#' @param envname The name of, or path to, a Python virtual environment.
#' @param what What kind of virtual environment to create. A python virtual
#' environment (\code{"venv"}) or a conda environment (\code{"conda"}).
#' @param python Argument to \code{reticulate::virtualenv_create}, specifying
#' the path to a Python interpreter.
#' @param ... Additional arguments to [reticulate::virtualenv_create] or
#' [reticulate::conda_create] according to the value of \code{what}.
#' @return There is no return value.
#' @section Side effects:
#' Creates and configures either  a python virtual environment or conda
#' environment (according to the value of \code{what}) with all the packages
#' required for running chromConverter.
#' @author Ethan Bass
#' @import reticulate
#' @keywords internal
#' @export
#' @md

configure_python_environment <- function(what = c("venv", "conda"),
                                         envname = "chromConverter",
                                         python = reticulate::virtualenv_starter(),
                                         ...){
  what <- match.arg(what, c("venv", "conda"))
  packages <- c("Aston", "olefile", "pandas", "rainbow-api", "scipy")
  install <- FALSE
  if (!dir.exists(reticulate::miniconda_path())){
    install <- readline(sprintf("It is recommended to install miniconda in your R library to use %s parsers. Install miniconda now? (y/n)"))
    if (install %in% c('y', "Y", "YES", "yes", "Yes")){
      reticulate::install_miniconda()
    }
  }
  check_name <- switch(what, "venv" = reticulate::virtualenv_exists,
                       "conda" = reticulate::condaenv_exists)
  exists <- check_name(envname)
  if (exists){
    stop(sprintf('The %s environment, "%s" already exists. To create a new environment,
                 please remove the existing environment first using `%s("%s")`.',
                 switch(what,"venv" = "virtual", "conda" = "conda"), envname,
                 switch(what, "venv" = "reticulate::virtualenv_remove",
                        "conda" = "reticulate::conda_remove"),
                 envname))
  }
  if (what == "venv"){
    reticulate::virtualenv_create(envname = envname, packages = packages,
                                  python = python, ...)
  } else if (what == "conda"){
    reticulate::conda_create(envname = envname,
                             packages = c("olefile", "pandas", "scipy"), ...)
    reticulate::conda_install(envname = envname,
                              packages=c("Aston", "rainbow-api"), pip = TRUE)
  }
}

#' Get required python packages for a parser
#' @noRd
get_parser_reqs <- function(parser){
  switch(tolower(parser), "aston" = c("pandas", "scipy", "numpy", "Aston"),
         "olefile" = c("olefile"),
         "rainbow" = c("numpy", "rainbow-api"),
         "all" = c("pandas","scipy","numpy","Aston","olefile","numpy","rainbow-api"))
}

#' Utility function to capitalize first letter of string
#' @noRd
simple_cap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

#' Get retention times
#'
#' Get retention times from a list of chromatograms or a \code{peak_table} object.
#'
#' If \code{data_format} attributes is not present, the data is assumed to be in
#' wide format with retention times as rownames.
#'
#' @param x A chromatogram or list of chromatograms.
#' @param idx Index of a chromatogram from which to extract retention times.
#' @return Numeric vector of retention times from the chromatogram specified by
#' \code{idx}.
#' @family utility functions
#' @noRd

get_times <- function(x, idx = 1){
  if (inherits(x, "chrom_list") | inherits(x, "list")){
    x <- x[[idx]]
  }
  data_format <- attr(x, "data_format")
  data_format <- ifelse(is.null(data_format), "wide", data_format)
  if (data_format == "long"){
    as.numeric(x[,1])
  } else{
    as.numeric(rownames(x))
  }
}
