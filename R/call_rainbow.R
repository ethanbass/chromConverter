#' Call 'rainbow' parsers
#' Parse 'Agilent' or 'Waters' files with rainbow parsers
#'
#' Uses [rainbow](https://rainbow-api.readthedocs.io) parsers to read in Agilent
#' (`.D`) and Waters (`.raw`) files. If `format_in` is `"agilent_d"` or
#' `"waters_raw"`, a directory of the appropriate format (`.D` or `.raw`) should
#' be provided to the `path` argument. If `format_in` is `"chemstation_uv"` a
#' `.uv` file should be provided. Data can be filtered by detector type using
#' the `what` argument.
#'
#' @inheritParams shared_params
#' @param path Path to file.
#' @param format_in Format of the supplied files. Either `agilent_d`,
#' `waters_raw`, or `chemstation`.
#' @param what What types of data to return (e.g. `MS`, `UV`, `CAD`, `ELSD`).
#' This argument only applies if `by == "detector"`.
#' @param by How to order the list that is returned. Either `detector` (default)
#' or `name`.
#' @param precision Number of decimals to round mz values. Defaults to `1`.
#' Ignored if `bin_width` is supplied.
#' @param bin_width Width of the m/z grid, in daltons. An alternative to
#' `precision` for grids that are not a power of ten (e.g. `0.5`). Defaults to
#' `NULL`, in which case the grid is derived from `precision` as
#' `10^-precision`.
#' @param sparse Logical. Whether to return MS data in sparse format (excluding
#' zeros). Defaults to `TRUE`. Applies only when data are requested in `long`
#' format.
#' @author Ethan Bass
#' @return Returns a (nested) list of matrices or `data.frame`s according to
#' the value of `format_out`. Data is ordered according to the value of `by`.
#' @family external parsers
#' @export

call_rainbow <- function(path,
                         format_in = c("agilent_d", "waters_raw", "masshunter",
                                       "chemstation", "chemstation_uv",
                                       "chemstation_fid", "chemstation_ms"),
                         format_out = c("matrix", "data.frame", "data.table"),
                         data_format = c("wide", "long"),
                         by = c("detector", "name"), what = NULL,
                         read_metadata = TRUE,
                         metadata_format = c("chromconverter", "raw"),
                         collapse = TRUE, precision = 1, sparse = TRUE,
                         bin_width = NULL){
  rb <- check_rb_configuration(format_in)
  by <- match.arg(by, c("detector", "name"))
  format_out <- check_format_out(format_out)
  data_format <- check_data_format(data_format, format_out)
  metadata_format <- check_metadata_format(metadata_format, "rainbow")

  # `source_file_format` describes the file, and `format_in` is about to be
  # collapsed for parser selection -- and would otherwise fall back to the
  # metadata tag, reporting the file format as "rainbow"
  source_file_format <- format_in
  if (grepl("chemstation", format_in)){
    format_in <- "chemstation"
  }
  converter <- switch(format_in,
                      "agilent_d" = rb$read$read,
                      "waters_raw" = rb$read$read,
                      "masshunter" = rb$read$read,
                      "chemstation" = rb$agilent$chemstation$parse_file,
                      "default" = rb$read$read)
  if (format_in %in% c("chemstation")){
    by <- "single"
  }
  x <- do.call(converter, c(list(path),
                            rb_precision_args(precision, bin_width)))
  if (by == "detector"){
    if (!is.null(what)){
      what_not_present <- which(!(what %in% names(x$by_detector)))
      if (length(what_not_present > 0)){
          warning(paste(what[what_not_present], "is not a recognized detector"))
      }
      dtr.idx <- which(names(x$by_detector) %in% what)
    } else{
      dtr.idx <- seq_along(x$by_detector)
    }
    xx <- lapply(x$by_detector[dtr.idx], function(dtr){
      dtr_dat <- lapply(dtr, function(xx){
        extract_rb_data(xx, format_out = format_out, data_format = data_format,
                        read_metadata = read_metadata, meta = x$metadata,
                        metadata_format = metadata_format, source_file = path,
                        source_file_format = source_file_format,
                        sparse = sparse)
      })
      names(dtr_dat) <- extract_rb_names(dtr)
      if (collapse) dtr_dat <- collapse_list(dtr_dat)
      dtr_dat
    })
  } else if (by == "name"){
    xx <- lapply(x$datafiles, function(xx){
      extract_rb_data(xx, format_out = format_out, data_format = data_format,
                      read_metadata = read_metadata, meta = x$metadata,
                      metadata_format = metadata_format, source_file = path,
                        source_file_format = source_file_format,
                      sparse = sparse)
    })
    names(xx) <- names(x$by_name)
  } else{
    xx <- extract_rb_data(x, format_out = format_out, data_format = data_format,
                          read_metadata = read_metadata, meta = x$metadata,
                          metadata_format = metadata_format, source_file = path,
                        source_file_format = source_file_format,
                          sparse = sparse)
  }
  xx
}

#' Extract data with rainbow
#' This function is called internally by `call_rainbow`.
#' @author Ethan Bass
#' @noRd
extract_rb_data <- function(xx, format_out = "matrix",
                            data_format = c("wide", "long"),
                            read_metadata = TRUE,
                            metadata_format = "rainbow",
                            meta = NULL,
                            source_file, source_file_format = NA,
                            sparse = TRUE){
  data_format <- check_data_format(data_format, format_out)
  data <- xx$data
  try(rownames(data) <- xx$xlabels)
  colnames(data) <- xx$ylabels
  if (data_format == "long"){
    names_to <- switch(xx$detector, "MS" = "mz",
                                    "UV" = "lambda",
                                           "lambda")
    sparse <- ifelse(sparse && xx$detector=="MS", TRUE, FALSE)
    data <- reshape_chrom(data, data_format = "long", names_to = names_to,
                          sparse = sparse)
  }
  data <- convert_chrom_format(data, format_out = format_out,
                               data_format = data_format)
  if (read_metadata){
    meta <- c(meta, xx$metadata, detector = xx$detector)
    data <- attach_metadata(data, meta = meta, format_in = metadata_format,
                            format_out = format_out, data_format = data_format,
                            parser = "rainbow", source_file = source_file,
                            source_file_format = source_file_format)
  }
  data
}

#' Extract 'rainbow' element names.
#' This function is called internally by `call_rainbow`.
#' @noRd
extract_rb_names <- function(xx){
  sapply(xx, function(xxx){
    xxx$name
  })
}

#' Check 'rainbow' configuration
#' This function is called internally by `call_rainbow`.
#' @noRd
check_rb_configuration <- function(format_in = NULL){
  check_py_module("rainbow", format_in = format_in)
  v <- numeric_version(py_import("importlib.metadata")$version("rainbow-api"))
  if (v < numeric_version("1.5.0")) {
    stop("chromConverter requires rainbow-api >= 1.5.0 (found ", v,
         "), which replaced the `precision` argument of `read()` with ",
         "`bin_width` and `display_precision`. Please upgrade: ",
         "pip install --upgrade rainbow-api", call. = FALSE)
  }
  list(read = py_import("rainbow.__init__"),
       agilent = py_import("rainbow.agilent"))
}

#' Translate chromConverter's `precision` argument for 'rainbow'
#'
#' rainbow v1.5.0 replaced the `precision` argument of `read()` and
#' `chemstation.parse_file()` with `bin_width` (the width of the m/z grid, in
#' daltons) and `display_precision` (the number of decimals used to label m/z
#' values). The old `precision = N` did both, and corresponds to
#' `bin_width = 10^-N` with `display_precision = N`.
#'
#' `precision` is kept as chromConverter's own argument -- rainbow has renamed
#' this setting twice (`prec` in 1.0, `precision` in 1.3, `bin_width` in 1.5) --
#' but it can only express grids that are a power of ten, so `bin_width` is
#' accepted directly as an alternative.
#'
#' As of rainbow v1.5.2, `display_precision` does not change the binned
#' intensities -- `bin_width` alone determines `data` -- but it does change the
#' `ylabels` rainbow returns, which become the m/z column names on the R side.
#' With `bin_width = 0.25`, a `display_precision` of 0 or 1 labels the third bin
#' `102.8` where 2 labels it `102.75`. rainbow keeps the labels distinct either
#' way, so a coarse value does not produce duplicated column names, but it does
#' misstate the bin it names. `display_precision` is therefore derived from the
#' grid rather than from `precision` (rainbow's own defaults are `"auto"` for
#' `read()` and `0` for `parse_file()`): just enough decimals to write
#' `bin_width` exactly, which for a power of ten is `precision` itself.
#' @noRd
rb_precision_args <- function(precision, bin_width = NULL){
  if (is.null(bin_width)){
    bin_width <- 10^(-precision)
  } else if (!is.numeric(bin_width) || length(bin_width) != 1 ||
             is.na(bin_width) || bin_width <= 0){
    stop("`bin_width` must be a single positive number.", call. = FALSE)
  }
  list(bin_width = bin_width,
       display_precision = rb_display_precision(bin_width))
}

#' Decimals needed to label a given m/z grid
#'
#' The smallest number of decimals that writes `bin_width` exactly, so that no
#' two bins can round onto the same label.
#' @noRd
rb_display_precision <- function(bin_width){
  for (digits in 0:10){
    if (isTRUE(all.equal(round(bin_width, digits), bin_width))){
      return(as.integer(digits))
    }
  }
  10L
}
