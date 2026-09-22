#' Call Entab
#'
#' Converts chromatography data files using [entab](https://github.com/bovee/entab)
#' parsers.
#'
#' @inheritParams shared_params
#' @param path Path to file.
#' @param format_in Format of input.
#' @inherit shared_params return
#' @examples \dontrun{
#' call_entab("path/to/file.uv", format_in = "chemstation_uv")
#' }
#' @family external parsers
#' @export

call_entab <- function(path, data_format = c("wide", "long"),
                       format_out = c("matrix", "data.frame", "data.table"),
                       format_in = "", read_metadata = TRUE,
                       metadata_format = c("chromconverter", "raw")){
  if (!requireNamespace("entab", quietly = TRUE)){
    stop("The entab R package must be installed to use entab parsers:
      install.packages('entab', repos='https://ethanbass.github.io/drat/')",
         call. = FALSE)
  }
  format_out <- check_format_out(format_out)
  data_format <- check_data_format(data_format, format_out)

  metadata_format <- check_metadata_format(metadata_format,
                                           entab_metadata_tag(format_in))
  r <- entab::Reader(path)
  file_format <- r$parser()
  x <- entab::as.data.frame(r)
  if (grepl("dad$|uv$", file_format)){
    signal.idx <- grep("signal", colnames(x))
    if (length(signal.idx) == 1){
      colnames(x)[signal.idx] <- "wavelength"
    }
    colnames(x) <- c("rt", "lambda", "intensity")
    if (data_format == "wide"){
      x <- reshape_chrom_wide(x, time_var = "rt", lambda_var = "lambda",
                              value_var = "intensity")
      }
  } else if (grepl("fid$|mwd$", file_format)){
    if (data_format == "wide"){
      x <- data.frame(row.names = x$time, intensity = x$intensity)
    }
  } else if (grepl("ms$", file_format)){
    colnames(x)[c(1, 3)] <- c("rt", "intensity")
    data_format <- "long"
  }
  x <- convert_chrom_format(x, format_out = format_out, data_format)
  if (read_metadata){
    meta <- r$metadata()
    meta$run_date <- as.POSIXct(eval(meta$run_date))
    meta$detector <- toupper(strsplit(file_format,"_")[[1]][2])
    meta <- rename_list(meta, c("detector_model" = "instrument", "method" = "method",
                        "operator" = "operator", "date" = "run_date",
                        "sample_name" = "sample",
                        "detector_y_units" = "y_units",
                        "intensity_multiplier" = "mult_correction",
                        "intensity_offset" = "offset_correction"))

    if (grepl("chemstation", format_in)){
      metadata_from_file <- try(read_chemstation_metadata(path), silent = TRUE)
    } else if (format_in == "masshunter_dad"){
      metadata_from_file <- try(read_masshunter_metadata(path), silent = TRUE)
    }
    if (exists("metadata_from_file") && !inherits(metadata_from_file, "try-error")){
      meta <- c(meta, metadata_from_file)
    }
    x <- attach_metadata(x, meta, format_in = metadata_format,
                         format_out = format_out,
                         data_format = data_format, parser = "entab",
                         source_file = path, source_file_format = file_format)
  }
  x
}

#' Metadata format tag for a format read by 'entab'
#'
#' 'entab' reports its own set of metadata fields whatever the file format, and
#' `call_entab` renames them onto the names the `"chemstation"` branch of
#' `attach_metadata` reads, so that branch describes every format it can read.
#' The exception is `masshunter_dad`, whose fields come from the
#' `sample_info.xml` file that `call_entab` merges in.
#'
#' Passing `format_in` through unchanged, as `call_entab` used to, left
#' `format_in = "other"` matching no branch at all, so `attach_metadata`
#' returned `NULL` and the chromatogram was discarded.
#' @noRd
entab_metadata_tag <- function(format_in){
  if (identical(format_in, "masshunter_dad")) "masshunter_dad" else "chemstation"
}
