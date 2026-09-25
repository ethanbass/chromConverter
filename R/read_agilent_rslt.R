#' Read files from 'Agilent' .rslt directories
#'
#' Reads a sequence of injections from an 'Agilent' `.rslt` directory.
#' Combines raw chromatogram data from `.dx` files with injection-level
#' metadata parsed from the accompanying `.acaml` file.
#'
#' Currently this function only reads `.dx` chromatogram files. Peak tables
#' stored in `.rx` files are not yet supported.
#'
#' @inheritParams read_agilent_dx
#' @param path Path to 'Agilent' `.rslt` directory.
#' @param sample_names How to name the chromatograms that are returned. Either
#' `basename` (default), to use the name of the source `.dx` file, or
#' `sample_name`, to use the sample name field from the metadata.
#' @param progress_bar Logical. Whether to show a progress bar while reading
#' the sequence's `.dx` files. Defaults to `FALSE`, since [read_chroms] already
#' reports progress over the directories it found and a second bar inside each
#' one would be redrawn per directory. Set to `TRUE` when calling this function
#' directly on a sequence with many injections.
#' @param cl Argument to [pbapply][pbapply::pbapply] specifying the number
#' of clusters to use or a cluster object created by
#' [makeCluster][parallel::makeCluster]. Defaults to `1`.
#' @return A list of chromatograms (one `read_agilent_dx`-style result per
#' injection in the sequence), in the format specified by `data_format` and
#' `format_out`. If `read_metadata` is `TRUE`, injection-level metadata parsed
#' from the `.acaml` file is attached to each chromatogram as attributes.
#' @examples \dontrun{
#' read_agilent_rslt("path/to/sequence.rslt")
#' }
#' @author Ethan Bass
#' @family 'Agilent' parsers
#' @export
read_agilent_rslt <- function(path, what = c("chroms","dad"), path_out = NULL,
                              format_out = c("matrix", "data.frame", "data.table"),
                              data_format = c("wide", "long"),
                              read_metadata = TRUE,
                              metadata_format = c("chromconverter", "raw"),
                              collapse = TRUE,
                              sample_names = c("basename", "sample_name"),
                              progress_bar = FALSE, cl = 1){
  format_out <- match.arg(format_out, c("matrix", "data.frame", "data.table"))
  data_format <- match.arg(data_format, c("wide", "long"))
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  sample_names <- match.arg(sample_names, c("basename", "sample_name"))
  dx_files <- list.files(path, pattern = "\\.dx$", ignore.case = TRUE,
                         full.names = TRUE)
  if (length(dx_files) == 0){
    stop("No .dx files found in ", path)
  }

  acaml_file <- list.files(path, pattern = "\\.acaml$", ignore.case = TRUE,
                           full.names = TRUE)
  acaml <- if (read_metadata && length(acaml_file) > 0){
    read_acaml(acaml_file[1])
  } else NULL

  laplee <- choose_apply_fnc(progress_bar, cl = cl)
  data <- laplee(dx_files, read_agilent_dx, what = what, path_out = path_out,
                 format_out = format_out, data_format = data_format,
                 read_metadata = read_metadata,
                 metadata_format = metadata_format, collapse = collapse)
  acaml_field_map <- c(
    SampleName           = "sample_name",
    VialNumber           = "sample_position",
    InstrumentName       = "instrument",
    Software             = "software",
    SoftwareVersion      = "software_version",
    AcqMethodName        = "method",
    SequenceName         = "batch",
    InjectionAcqDateTime = "run_datetime",
    InjectionVolume      = "sample_injection_volume",
    SampleAmount         = "sample_amount",
    SampleType           = "sample_type"
  )

  if (!is.null(acaml)){
    data <- mapply(function(chrom, fname){
      meta <- acaml[acaml$RawDataFileName == fname, , drop = FALSE]
      if (nrow(meta) == 0){
        warning("No acaml metadata found for ", fname)
        return(chrom)
      }
      for (col in names(acaml_field_map)){
        if (col %in% names(meta) && !is.na(meta[[col]]) && nzchar(meta[[col]])){
          val <- meta[[col]]
          if (acaml_field_map[[col]] == "run_datetime"){
            val <- as.POSIXct(val, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
          }
          attr(chrom, acaml_field_map[[col]]) <- val
        }
      }
      attr(chrom, "acaml_metadata") <- meta
      chrom
    }, data, basename(dx_files), SIMPLIFY = FALSE)
  }
  file_names <- gsub("\\.dx$", "", basename(dx_files), ignore.case = TRUE)
  names(data) <- if (sample_names == "sample_name"){
    name_by_sample_name(data, file_names)
  } else file_names
  structure(data, class = "chrom_list")
}
