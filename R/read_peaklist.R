#' Read peak lists
#'
#' Reads peak lists from specified folders or vector of paths.
#'
#' @param paths Paths to files or folders containing peak list files.
#' @param find_files Logical. Set to \code{TRUE} (default) if you are providing
#' the function with a folder or vector of folders containing the files.
#' Otherwise, set to\code{FALSE}.
#' @param format_in Format of files to be imported/converted. Current options
#' include: \code{chemstation}, \code{shimadzu_fid}, \code{shimadzu_dad},
#' \code{shimadzu_lcd}, and \code{shimadzu_gcd}.
#' @param pattern pattern (e.g. a file extension). Defaults to NULL, in which
#' case file extension will be deduced from \code{format_in}.
#' @param data_format Either \code{chromatographr} or \code{original}.
#' @param read_metadata Logical, whether to attach metadata (if it's available).
#' Defaults to TRUE.
#' @param metadata_format Format to output metadata. Either \code{chromconverter} or
#' \code{raw}.
#' @param progress_bar Logical. Whether to show progress bar. Defaults to
#' \code{TRUE} if \code{\link[pbapply]{pbapply}} is installed.
#' @param cl Argument to \code{\link[pbapply]{pbapply}} specifying the number
#' of clusters to use or a cluster object created by
#' \code{\link[parallel]{makeCluster}}. Defaults to 1.
#' @return A list of chromatograms in \code{matrix} or \code{data.frame} format,
#' according to the value of \code{format_out}.
#' @import reticulate
#' @importFrom utils write.csv file_test
#' @importFrom purrr partial
#' @examplesIf interactive()
#' path <- "tests/testthat/testdata/dad1.uv"
#' chr <- read_chroms(path, find_files = FALSE, format_in = "chemstation_uv")
#' @author Ethan Bass
#' @export

read_peaklist <- function(paths, find_files,
                        format_in = c("chemstation", "shimadzu_fid",
                                      "shimadzu_dad", "shimadzu_lcd",
                                      "shimadzu_gcd"),
                        pattern = NULL,
                        data_format = c("chromatographr", "original"),
                        metadata_format = c("chromconverter", "raw"),
                        read_metadata = TRUE, progress_bar, cl = 1){
  data_format <- match.arg(tolower(data_format), c("chromatographr", "original"))
  format_in <- match.arg(tolower(format_in),
                         c("chemstation", "shimadzu_fid", "shimadzu_dad",
                           "shimadzu_lcd", "shimadzu_gcd"))
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
    parser <- partial(read_shimadzu_lcd, what = "peak_table",
                      data_format="wide", read_metadata=read_metadata)
  } else if (format_in == "shimadzu_gcd"){
    parser <- partial(read_shimadzu_gcd, what = "peak_table",
                      data_format="wide", read_metadata=read_metadata)
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
