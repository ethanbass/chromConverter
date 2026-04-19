#' Read peak lists
#'
#' Reads peak lists from specified folders or vector of paths.
#'
#' @inheritParams shared_params
#' @param paths Paths to files or folders containing peak list files.
#' @param find_files Logical. Set to `TRUE` (default) if you are providing
#' the function with a folder or vector of folders containing the files.
#' Otherwise, set to `FALSE`.
#' @param format_in Format of files to be imported/converted. Current options
#' include: `chemstation`, `shimadzu_fid`, `shimadzu_dad`, `shimadzu_lcd`, and
#' `shimadzu_gcd`.
#' @param pattern A pattern (e.g. a file extension). Defaults to `NULL`, in
#' which case the file extension will be deduced from `format_in`.
#' @param data_format Either `chromatographr` or `original`.
#' @return A list of `data.frame`s containing information about peaks where
#' each list element represents a sample and each row represents an individual
#' peak in that sample.
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
                        data_format = c("chromatographr", "original"),
                        metadata_format = c("chromconverter", "raw"),
                        read_metadata = TRUE, progress_bar, cl = 1){
  data_format <- match.arg(tolower(data_format), c("chromatographr", "original"))
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
                             data_format = data_format,
                             metadata_format = metadata_format)
  } else if (format_in %in% c("shimadzu_dad", "shimadzu_fid")){
    pattern <- ifelse(is.null(pattern), ".txt", pattern)
    parser <- partial(read_shimadzu, what = "peak_table",
                         data_format = "wide",
                         read_metadata = read_metadata,
                         peaktable_format = data_format)
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
  if (find_files){
    files <- find_files(paths, pattern)
  } else {
    files <- paths
    if (!is.null(pattern)){
      match <- grep(pattern, files, ignore.case = TRUE)
      if (length(match) == 0){
        warning("The provided files do not match the expected file extension.
      Please confirm that the specified format ('format_in') is correct.",
                immediate. = TRUE)
      } else if (length(match) < length(files)){
        warning(paste("Some of the files do not have the expected file extension:",
                      files[match]), immediate. = TRUE)
      }
    }
  }
  file_names <- extract_filenames(files)
  if (format_in == "chemstation"){
    data <- parser(files)
  } else{
    laplee <- choose_apply_fnc(progress_bar, cl = cl)
    data <- laplee(X = files, function(file){
      try(parser(file), silent = TRUE)
    })
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
