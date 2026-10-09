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
#' `shimadzu_lcd`, `shimadzu_gcd`, `chromatotec`, or `asm`.
#' @param pattern A pattern (e.g. a file extension). Defaults to `NULL`, in
#' which case the file extension will be deduced from `format_in`.
#' @param peaktable_format Whether to return peak tables in `chromatographr`
#' or `original` format. 'Chromatotec' peak tables are always returned in their
#' original format.
#' @param sort_by How to sort the samples: `auto` (default) sorts files by
#' acquisition time unless `paths` lists the files explicitly or any
#' acquisition time is missing; `none` keeps files in the order given, or in
#' alphabetical order if `find_files = TRUE`; `acquisition_time` sorts by the
#' acquisition time recorded in each file (`run_datetime`); `file_time` sorts
#' by the time when each file was last modified.
#' @param data_format Deprecated. Use `peaktable_format` instead.
#' @return A `peak_list`: a list with one element per sample, holding its peak
#' table, or a list of peak tables where the file records more than one. The
#' tables are named by wavelength (e.g. `"254"`) where the file records one,
#' and otherwise by the file's own name for the signal; a table identical to
#' another at the same wavelength is a copy and is left out. Each row is a
#' peak. Every table starts with a `sample` column and
#' a `lambda` column giving the signal's wavelength, which is `NA` for a
#' detector without one or a file that does not record it.
#' @import reticulate
#' @importFrom utils write.csv file_test
#' @importFrom purrr partial
#' @examples
#' path <- system.file("extdata", "benzoxazinoid_standards",
#'                     package = "chromConverter")
#' peak_list <- read_peaklist(path, progress_bar = FALSE)
#' names(peak_list)
#' peak_list[["BENZOS_250PPM"]][["254"]]
#' @author Ethan Bass
#' @export

read_peaklist <- function(paths, find_files,
                        format_in = c("chemstation", "shimadzu_fid",
                                      "shimadzu_dad", "shimadzu_lcd",
                                      "shimadzu_gcd", "chromatotec", "asm"),
                        pattern = NULL,
                        peaktable_format = c("chromatographr", "original"),
                        metadata_format = c("chromconverter", "raw"),
                        read_metadata = TRUE, progress_bar, cl = 1,
                        sort_by = c("auto", "none", "acquisition_time",
                                    "file_time"),
                        data_format = NULL){
  if (!is.null(data_format)){
    warn_renamed_arg("data_format", "peaktable_format")
    peaktable_format <- data_format
  }
  peaktable_format <- match.arg(tolower(peaktable_format),
                                c("chromatographr", "original"))
  format_in <- match.arg(tolower(format_in),
                         c("chemstation", "shimadzu_fid", "shimadzu_dad",
                           "shimadzu_lcd", "shimadzu_gcd", "chromatotec",
                           "asm"))
  sort_by <- match.arg(sort_by, c("auto", "none", "acquisition_time",
                                   "file_time"))
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
  } else if (format_in == "asm"){
    pattern <- ifelse(is.null(pattern), "\\.json$", pattern)
    parser <- partial(read_asm, what = "peak_table",
                      peaktable_format = peaktable_format,
                      format_out = "data.frame",
                      read_metadata = read_metadata,
                      metadata_format = metadata_format)
  }
  if (peaktable_format == "chromatographr" &&
      format_in %in% c("shimadzu_lcd", "shimadzu_gcd")){
    read <- parser
    parser <- function(...) sz_peaktable_chromatographr(read(...))
  }
  files <- collect_files(paths, pattern, search_dirs = find_files)
  if (sort_by == "file_time"){
    files <- files[order(fs::file_info(files)$modification_time)]
  }
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
    data <- splice_samples(data, file_names)
    file_names <- names(data)
    data <- lapply(seq_along(data), function(i){
      if (inherits(data[[i]], "list")){
        transfer_metadata(lapply(data[[i]], function(xx){
          transfer_metadata(cbind(sample = file_names[i],
                                  lambda = peak_table_lambda(xx), xx), xx)
        }), data[[i]])
      } else {
        transfer_metadata(cbind(sample = file_names[i],
                                lambda = peak_table_lambda(data[[i]]),
                                data[[i]]), data[[i]])
      }
    })
    class(data) <- "peak_list"
    names(data) <- file_names
  }
  data[] <- lapply(data, function(x){
    if (is.data.frame(x)) x else transfer_metadata(name_peak_tables(x), x)
  })
  if (!is.null(attr(data, "lambdas"))) attr(data, "lambdas") <- names(data[[1]])
  if (sort_by == "acquisition_time"){
    if (!read_metadata){
      warning("`sort_by = \"acquisition_time\"` requires `read_metadata = TRUE`; skipping sort.",
              immediate. = TRUE)
    } else {
      data <- sort_chroms_by_time(data)
    }
  } else if (sort_by == "auto" && find_files && read_metadata){
    data <- sort_chroms_by_time(data, quiet = TRUE)
  }
  data
}

#' Convert 'Shimadzu' peak tables to the `chromatographr` format
#' @noRd
sz_peaktable_chromatographr <- function(x){
  if (is.data.frame(x)){
    cols <- match(c("r.time", "i.time", "f.time", "area", "height"),
                  tolower(names(x)))
    if (anyNA(cols)) return(x)
    out <- as.data.frame(x)[cols]
    names(out) <- c("rt", "start", "end", "area", "height")
    transfer_metadata(out, x)
  } else if (is.list(x)){
    transfer_metadata(lapply(x, sz_peaktable_chromatographr), x)
  } else x
}

#' The wavelength of a peak table
#'
#' Read from its `wavelength` attribute, or `NA` for a detector with none.
#' @noRd
peak_table_lambda <- function(x){
  w <- suppressWarnings(as.numeric(attr(x, "wavelength")[1]))
  if (length(w) == 1) w else NA_real_
}

#' Name a sample's peak tables by wavelength
#'
#' A table whose `lambda` is known is named for it (`"254"`); one without keeps
#' its name. A table identical to an earlier one at the same wavelength is a
#' copy and is dropped, and any other tables sharing a name are told apart as
#' `"254"`, `"254_1"`.
#' @noRd
name_peak_tables <- function(tabs){
  lam <- vapply(tabs, function(t){
    if ("lambda" %in% names(t)) suppressWarnings(as.numeric(t$lambda[1])) else NA_real_
  }, numeric(1))
  nms <- ifelse(is.na(lam), names(tabs), as.character(lam))
  copy <- vapply(seq_along(tabs), function(i){
    any(vapply(seq_len(i - 1), function(j){
      nms[j] == nms[i] &&
        isTRUE(all.equal(as.data.frame(tabs[[i]]), as.data.frame(tabs[[j]]),
                         check.attributes = FALSE))
    }, logical(1)))
  }, logical(1))
  tabs <- tabs[!copy]
  names(tabs) <- make.unique(nms[!copy], sep = "_")
  tabs
}
