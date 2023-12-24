#' Parse Agilent or Waters files with rainbow parser
#'
#' Uses [rainbow](https://rainbow-api.readthedocs.io) parsers to read in Agilent (.D)
#' and Waters (.raw) files. If \code{format_in} is \code{agilent_d} or
#' \code{waters_raw}, a directory of the appropriate format (\code{.d} or
#' \code{.raw}) should be provided to \code{file}. If \code{format_in} is
#' \code{chemstation_uv} a \code{.uv} file should be provided. Data can be filtered
#' by detector type using the \code{what} argument.
#'
#' @param file Path to file
#' @param format_in Format of the supplied files. Either \code{agilent_d},
#' \code{waters_raw}, or \code{chemstation}.
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param data_format Whether to return data in wide or long format.
#' @param what What types of data to return (e.g. \code{MS}, \code{UV}, \code{CAD},
#' \code{ELSD}). This argument only applies if \code{by == "detector"}.
#' @param by How to order the list that is returned. Either \code{detector}
#' (default) or \code{name}.
#' @param read_metadata Logical. Whether to attach metadata. Defaults to TRUE.
#' @param collapse Logical. Whether to collapse lists that only contain a single
#' element.
#' @param precision Number of decimals to round mz values. Defaults to 1.
#' @author Ethan Bass
#' @return Returns a (nested) list of \code{matrices} or \code{data.frames} according to
#' the value of \code{format_out}. Data is ordered according to the value of
#' \code{by}.
#' @export

call_rainbow <- function(file,
                         format_in = c("agilent_d", "waters_raw", "masshunter",
                                       "chemstation", "chemstation_uv",
                                       "chemstation_fid"),
                         format_out = c("matrix", "data.frame"),
                         data_format = c("wide", "long"),
                         by = c("detector","name"), what = NULL,
                         read_metadata = TRUE, collapse = TRUE,
                         precision = 1){
  check_rb_configuration()
  by <- match.arg(by, c("detector","name"))
  format_out <- match.arg(format_out, c("matrix","data.frame"))
  data_format <- match.arg(data_format, c("wide", "long"))
  # check_rb_dir(file)
  if (grepl("chemstation", format_in)){
    format_in <- "chemstation"
  }
  converter <- switch(format_in,
                      "agilent_d" = rb_read$read,
                      "waters_raw" = rb_read$read,
                      "masshunter" = rb_read$read,
                      "chemstation" = rb_parse_agilent$chemstation$parse_file,
                      "default" = rb_read$read)
  if (format_in %in% c("chemstation")){
    by <- "single"
  }
  x <- converter(file, prec = as.integer(precision))
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
                        read_metadata = read_metadata)
      })
      names(dtr_dat) <- extract_rb_names(dtr)
      if (collapse) dtr_dat <- collapse_list(dtr_dat)
      dtr_dat
    })
  } else if (by == "name"){
    xx <- lapply(x$datafiles, function(xx){
      extract_rb_data(xx, format_out = format_out, data_format = data_format,
                      read_metadata = read_metadata)
    })
    names(xx) <- names(x$by_name)
  } else{
    xx <- extract_rb_data(x, format_out = format_out, data_format = data_format,
                          read_metadata = read_metadata)
  }
  xx
}

#' Extract data with rainbow
#' This function is called internally by \code{call_rainbow}.
#' @author Ethan Bass
#' @noRd
extract_rb_data <- function(xx, format_out = "matrix",
                            data_format = c("wide", "long"),
                            read_metadata = TRUE){
  data_format <- match.arg(data_format, c("wide", "long"))
  data <- xx$data
  try(rownames(data) <- xx$xlabels)
  colnames(data) <- xx$ylabels
  if (data_format == "long"){
    names_to <- switch(xx$detector, "MS" = "mz",
                       "UV" = "lambda",
                              "lambda")
    data <- reshape_chrom(data, data_format = "long", names_to = names_to)
  }
  if (format_out == "data.frame"){
    data <- as.data.frame(data)
  }
  if (read_metadata){
    try(attr(data, "detector") <- xx$detector)
    try(attr(data, "metadata") <- xx$metadata)
    attr(data, "parser") <- "rainbow"
    attr(data, "data_format") <- data_format
  }
  data
}

#' Extract 'rainbow' element names.
#' This function is called internally by \code{call_rainbow}.
#' @noRd
extract_rb_names <- function(xx){
  sapply(xx, function(xxx){
    xxx$name
  })
}

#' Assign 'rainbow' read
#' This function is called internally by \code{call_rainbow}.
#' @noRd
assign_rb_read <- function(){
  pos <- 1
  envir = as.environment(pos)
  assign("rb_read", reticulate::import("rainbow.__init__"), envir = envir)
  assign("rb_parse_agilent", reticulate::import("rainbow.agilent"), envir = envir)
}

#' Check 'rainbow' configuration
#' This function is called internally by \code{call_rainbow}.
#' @noRd
check_rb_configuration <- function(){
  assign_rb_read()
  if (length(rb_read) == 0){
    ans <- readline("rainbow not found. Configure rainbow? (y/n)?")
    if (ans %in% c('y', "Y", "YES", "yes", "Yes")){
      configure_python_environment(parser = "rainbow")
    }
  }
}
