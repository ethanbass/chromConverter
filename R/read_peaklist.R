#' Read peak lists
#'
#' Reads peak lists from specified folders or vector of paths.
#'
#' @inheritParams shared_params
#' @param paths Paths to files or folders containing peak list files.
#' @param find_files Logical. Whether to treat the supplied paths as
#' directories to search for files. Inferred if not supplied, by testing
#' whether every path is a file.
#' @param format_in Format of files to be imported/converted. One of
#' `chemstation` (the default), `shimadzu_fid`, `shimadzu_dad`,
#' `shimadzu_lcd`, `shimadzu_gcd`, or `chromatotec`.
#' @param pattern A pattern (e.g. a file extension). Defaults to `NULL`, in
#' which case the file extension will be deduced from `format_in`.
#' @param peaktable_format Whether to return peak tables in `chromatographr`
#' or `original` format.
#' @param data_format Deprecated. Use `peaktable_format` instead.
#' @return A `peak_list`: a list with one element per sample, holding its peak
#' table, or a list of peak tables named by signal where the file records more
#' than one. Each row is a peak.
#' @import reticulate
#' @importFrom utils write.csv file_test
#' @importFrom purrr partial
#' @examplesIf interactive()
#' path <- "tests/testthat/testdata/RUTIN2.D"
#' peak_list <- read_peaklist(path)
#' peak_list[["RUTIN2"]][["254"]]
#' @author Ethan Bass
#' @export

read_peaklist <- function(paths, find_files,
                        format_in = c("chemstation", "shimadzu_fid",
                                      "shimadzu_dad", "shimadzu_lcd",
                                      "shimadzu_gcd", "chromatotec"),
                        pattern = NULL,
                        peaktable_format = c("chromatographr", "original"),
                        metadata_format = c("chromconverter", "raw"),
                        read_metadata = TRUE, progress_bar, cl = 1,
                        data_format = NULL){
  if (!is.null(data_format)){
    warn_renamed_arg("data_format", "peaktable_format")
    peaktable_format <- data_format
  }
  peaktable_format <- match.arg(tolower(peaktable_format),
                                c("chromatographr", "original"))
  format_in <- match.arg(tolower(format_in),
                         c("chemstation", "shimadzu_fid", "shimadzu_dad",
                           "shimadzu_lcd", "shimadzu_gcd", "chromatotec"))
  if (missing(progress_bar)){
    progress_bar <- check_for_pkg("pbapply", return_boolean = TRUE)
  }
  if (missing(find_files)){
    if (length(format_in) == 1){
      ft <- all(file_test("-f", paths))
      find_files <- !ft
    } else{
      find_files <- FALSE
    }
  }
  exists <- dir.exists(paths) | file.exists(paths)
  if (all(!exists)){
    stop("Cannot locate files. None of the supplied paths exist.")
  }
  # choose parser
  if (format_in == "chemstation"){
    pattern <- ifelse(is.null(pattern), "report.txt", pattern)
    parser <- purrr::partial(read_chemstation_reports,
                             peaktable_format = peaktable_format,
                             metadata_format = metadata_format)
  } else if (format_in %in% c("shimadzu_dad", "shimadzu_fid")){
    pattern <- ifelse(is.null(pattern), ".txt", pattern)
    parser <- partial(read_shimadzu, what = "peak_table",
                         data_format = "wide",
                         read_metadata = read_metadata,
                         peaktable_format = peaktable_format)
  } else if (format_in == "shimadzu_lcd"){
    pattern <- ifelse(is.null(pattern), "\\.lcd$", pattern)
    parser <- partial(read_shimadzu_lcd, what = "peak_table",
                      data_format = "wide", read_metadata = read_metadata)
  } else if (format_in == "shimadzu_gcd"){
    pattern <- ifelse(is.null(pattern), "\\.gcd$", pattern)
    parser <- partial(read_shimadzu_gcd, what = "peak_table",
                      data_format = "wide", read_metadata = read_metadata)
  } else if (format_in == "chromatotec"){
    pattern <- ifelse(is.null(pattern), "\\.Chrom$", pattern)
    parser <- partial(read_chromatotec, what = "peak_table",
                      read_metadata = read_metadata,
                      metadata_format = metadata_format)
  }
  files <- collect_files(paths, pattern, search_dirs = find_files)
  file_names <- extract_filenames(files)
  if (format_in == "chemstation"){
    data <- parser(files)
  } else{
    laplee <- choose_apply_fnc(progress_bar, cl = cl)
    data <- laplee(X = files, function(file){
      try(parser(file), silent = TRUE)
    })
    errors <- which(vapply(data, inherits, logical(1), "try-error"))
    if (length(errors) > 0){
      warning(paste0(unlist(data[errors]), collapse = ""),
              "The following peak tables could not be interpreted: ",
              paste(sQuote(file_names[errors]), collapse = ", "),
              immediate. = TRUE)
      data <- data[-errors]
      file_names <- file_names[-errors]
    }
    data <- lapply(seq_along(data), function(i){
      if (inherits(data[[i]], "list")){
        lapply(data[[i]], function(xx){
          cbind(sample = file_names[i], xx)
        })
      } else {
        cbind(sample = file_names[i], data[[i]])
      }
    })
    class(data) <- "peak_list"
    names(data) <- file_names
  }
  data
}
