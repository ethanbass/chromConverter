#' Read Chromatograms
#'
#' Reads chromatograms from specified folders or vector of paths using the
#' [Aston](https://github.com/bovee/aston) or [Entab](https://github.com/bovee/entab)
#' file parsers.
#'
#' Currently recognizes Agilent ChemStation '.uv' and MassHunter '.dad' files.
#'
#' @name read_chroms
#' @param paths paths to files or folders containing files
#' @param find_files Logical. Set to \code{TRUE} (default) if you are providing
#' the function with a folder or vector of folders containing the files.
#' Otherwise, set to\code{FALSE}.
#' @param format.in Format of files to be imported/converted.
#' @param pattern pattern (e.g. a file extension). Defaults to NULL, in which
#' case file extension will be deduced from \code{format.in}.
#' @param parser What parser to use. Current option are \code{aston} or \code{
#' entab}. Entab must be manually installed from github.
#' @param R.format R object format (i.e. data.frame or matrix).
#' @param export Logical. If TRUE, will export files as csvs.
#' @param path.out Path for exporting files. If path not specified, files will
#' export to current working directory.
#' @param format.out Output format. Currently only \code{.csv}.
#' @param dat Existing list of chromatograms to append results.
#' (Defaults to NULL).
#' @return A list of chromatograms in matrix or data.frame format, according to
#' the value of 'R.format'.
#' @import reticulate
#' @importFrom utils write.csv
#' @importFrom purrr partial
#' @examplesIf interactive()
#' path <- "tests/testthat/testdata/dad1.uv"
#' chr <- read_chroms(path, find_files = FALSE, format.in = "chemstation.uv")
#' @author Ethan Bass
#' @export read_chroms

read_chroms <- function(paths, find_files = TRUE,
                        format.in=c("chemstation.uv", "masshunter.dad", "thermoraw"),
                        pattern=NULL, parser=c("aston","entab"),
                        R.format=c("matrix","data.frame"), export=FALSE,
                        path.out=NULL, format.out="csv", dat=NULL){
  parser <- match.arg (parser, c("aston","entab"))
  if (parser == "entab" & !requireNamespace("entab", quietly = TRUE)) {
    stop(
      "The entab R package must be installed to use entab parsers:
      install.packages('entab', repos='https://ethanbass.github.io/drat/')",
      call. = FALSE)
  }
  R.format <- match.arg(R.format, c("matrix", "data.frame"))
  format.in <- match.arg(format.in, c("chemstation.uv", "masshunter.dad", "thermoraw"))
  exists <- dir.exists(paths) | file.exists(paths)
  if (mean(exists) == 0){
    stop("Cannot locate files. None of the supplied paths exist.")
  }
  if (!is.null(path.out)){
    if (substr(path.out,1,1) != "/")
      path.out <- paste0("/", path.out)
    if (substr(path.out, nchar(path.out)-1, nchar(nchar(path.out))) != "/")
      path.out <- paste0(path.out, "/")
  }
  if (export | format.in == "thermoraw"){
    if (is.null(path.out)){
      ans <- readline("Export directory not specified! Export files to `temp` directory (y/n)?")
      if (ans %in% c("y","Y")){
        if (!dir.exists("temp"))
          dir.create("temp")
        path.out <- paste0(getwd(),'/temp/')
      } else{
        stop("Must specify directory to export files.")
      }
    }
    # path.out <- gsub("/$","", path.out)
    if (!dir.exists(path.out)){
      stop(paste0("The export directory '", path.out, "' does not exist."))
    }
  }
  if (is.null(dat)){
    dat<-list()}
  # choose converter
  if (format.in == "masshunter.dad"){
    pattern <- ifelse(is.null(pattern),".sp", pattern)
    converter <- ifelse(parser=="aston", sp_converter, entab_reader)
  } else if (format.in == "chemstation.uv"){
    pattern <- ifelse(is.null(pattern),".uv", pattern)
    converter <- ifelse(parser=="aston", uv_converter, entab_reader)
  } else if (format.in == "thermoraw"){
    pattern <- ".raw"
    converter <- partial(read_thermoraw, path_out = path.out)
    } else{
    converter <- ifelse(parser=="aston", trace_converter, entab_reader)
  }
  if (find_files){
    files <- unlist(lapply(paths, function(path){
      files <- list.files(path = path, pattern = pattern,
                          full.names = TRUE, recursive = TRUE)
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
  } else{
    files <- paths
    match <- grep(pattern, files)
    if (length(match)==0){
      warning("The provided files do not match the expected file extension.
      Please confirm that the specified format ('format.in') is correct.",
              immediate. = TRUE)
    } else if (length(match) < length(files)){
      warning(paste("Some of the files do not have the expected file extension:",
                    files[match]), immediate. = TRUE)
    }
  }
  if (format.in %in% c("chemstation.uv", "masshunter.dad")){
    file_names <- strsplit(files, "/")
    file_names <- gsub("\\.[Dd]", "",
                       sapply(file_names, function(n) n[grep("\\.[Dd]", n)]))
  } else file_names <- sapply(strsplit(basename(files),"\\."), function(x) x[1])
  data <- lapply(X=files, function(f){
    df <- converter(f)
  })
  if (R.format == "matrix"){
    data <- lapply(data, FUN=as.matrix)}
  names(data) <- file_names
  if (export){
    sapply(seq_along(data), function(i){
      write.csv(data[[i]], file = paste0(paste(path.out,names(data)[i], sep="/"),".CSV"))
    })
  }
  dat <- append(dat,data)
  dat
}

#' Converter for Agilent MassHunter UV files
#'
#' Converts a single chromatogram from MassHunter \code{.sp} format to R \code{data.frame}.
#'
#' Uses the [Aston](https://github.com/bovee/aston) file parser.
#'
#' @name sp_converter
#' @param file path to file
#' @return A data.frame object (retention time x wavelength).
#' @import reticulate
#' @export sp_converter
sp_converter <- function(file){
  df <- trace_file$agilent_uv$AgilentDAD(file)
  pd$DataFrame(df$data$values, columns=df$data$columns,
                    index=df$data$index)
}

#' Converter for Agilent ChemStation UV files
#'
#' Converts a single chromatogram from ChemStation \code{.uv} format to R \code{data.frame}.
#'
#' Uses the [Aston](https://github.com/bovee/aston) file parser.
#'
#' @name uv_converter
#' @param file path to file
#' @param correction Logical. Whether to apply empirical correction. Defaults is
#' TRUE.
#' @return A data.frame object (retention time x trace).
#' @import reticulate
#' @export uv_converter
uv_converter <- function(file, correction=TRUE){
  trace_file <- reticulate::import("aston.tracefile")
  pd <- reticulate::import("pandas")
  df <- trace_file$TraceFile(file)
  df <- pd$DataFrame(df$data$values, columns=df$data$columns,
               index=df$data$index)
  # multiply by empirical correction value
  if (correction){
    apply(df,2,function(xx)xx*0.9536743164062551070259132757200859487056732177734375)
  } else df
}

#' Aston TraceFile Converter
#'
#' Uses Aston parser to figure out file-type and convert to R \code{data.frame}.
#' @name trace_converter
#' @title generic converter for other types of files
#' @param file path to file
#' @return A data.frame object (retention time x trace).
#' @import reticulate
#' @noRd
trace_converter <- function(file){
  trace_file <- reticulate::import("aston.tracefile")
  pd <- reticulate::import("pandas")
  df <- trace_file$TraceFile(file)
  pd$DataFrame(df$data$values, columns=df$data$columns,
               index=df$data$index)
}

#' @name entab_reader
#' @title Entab parsers
#' @param file path to file
#' @return a \code{chrom} object
#' @importFrom tidyr pivot_wider
#' @noRd
entab_reader <- function(file, format.out="wide"){
  if (!requireNamespace("entab", quietly = TRUE)) {
    stop(
      "The entab R package must be installed to use entab parsers:
      install.packages('entab', repos='https://ethanbass.github.io/drat/')",
      call. = FALSE
    )
  }
  r <- entab::Reader(file)
  df <- entab::as.data.frame(r)
  if (format.out=="wide"){
    df <- data.frame(pivot_wider(df, id_cols = "time",
                                 names_from = "wavelength", values_from = "intensity"))}
  meta <- r$metadata()
  # structure(as.matrix(df),
  #           instrument = meta$instrument,
  #           method = meta$method,
  #           operator = meta$operator,
  #           class = "chrom")
  df
}
