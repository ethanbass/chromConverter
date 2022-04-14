#' Read Chromatograms
#'
#' Reads chromatograms from specified folders or vector of paths using the
#' [Aston](https://github.com/bovee/aston) file parser.
#'
#' Currently recognizes Agilent ChemStation '.uv' and MassHunter '.dad' files.
#'
#' @name read_chroms
#' @param paths paths to files or folders containing files
#' @param find_files TRUE
#' @param format.in Format of files to be imported/converted.
#' @param pattern pattern (e.g. a file extension). Defaults to NULL, in which
#' case file extension will be deduced from \code{format.in}.
#' @param parser = What parser to use. Currently, the only option is \code{Aston}.
#' @param R.format R object format (i.e. data.frame or matrix).
#' @param export Logical. If true, will export files as csvs.
#' @param path.out Path for exporting files. If path not specified, files will
#' export to current working directory.
#' @param format.out Output format. Currently only \code{.csv}.
#' @param dat Existing list of chromatograms to append results.
#' (Defaults to NULL).
#' @return A list of chromatograms in matrix or data.frame format, according to
#' the value of 'R.format'.
#' @import reticulate
#' @importFrom utils write.csv
#' @examplesIf interactive()
#' path <- "tests/testthat/testdata/dad1.uv"
#' chr <- read_chroms(path, find_files = FALSE, format.in = "chemstation.uv")
#' @author Ethan Bass
#' @export read_chroms

read_chroms <- function(paths, find_files = TRUE,
                        format.in=c("chemstation.uv", "masshunter.dad"),
                        pattern=NULL, parser=c("Aston"),
                        R.format=c("matrix","data.frame"), export=FALSE,
                        path.out=NULL, format.out="csv", dat=NULL){
  R.format <- match.arg(R.format, c("matrix", "data.frame"))
  format.in <- match.arg(format.in, c("chemstation.uv", "masshunter.dad"))
  exists <- dir.exists(paths) | file.exists(paths)
  if (mean(exists) == 0){
    stop("Cannot locate files. None of the supplied paths exist.")
  }
  if (export){
    if (is.null(path.out)){
      ans <- readline(".........Export directory not specified.
.........Export files to current working directory (y/n)? ")
      if (ans %in% c("y","Y")){
        path.out <- getwd()
      } else{
        stop("Must specify directory to export files.")
      }
    }
    path.out <- gsub("/$","", path.out)
    if (!dir.exists(path.out)){
      stop(paste0("The export directory '", path.out, "' does not exist."))
    }
  }
  if (is.null(dat)){
    dat<-list()}
  # choose converter
  if (format.in == "masshunter.dad"){
    pattern <- ifelse(is.null(pattern),".sp", pattern)
    converter <- sp_converter
  } else if (format.in=="chemstation.uv"){
    pattern <- ifelse(is.null(pattern),".uv", pattern)
    converter <- uv_converter
  } else{
    converter <- trace_converter
  }
  if (find_files){
    files <- unlist(lapply(paths, function(path){
      files <- list.files(path = path, pattern = pattern,
                          full.names = TRUE,recursive = TRUE)
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
    } else if (length(match)<length(files)){
      warning(paste("Some of the files do not have the expected file extension:",
                    files[match]), immediate. = TRUE)
    }
  }
  file_names <- strsplit(files, "/")
  file_names <- gsub("\\.[Dd]", "",
                     sapply(file_names, function(n) n[grep("\\.[Dd]", n)]))
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
#' @return A data.frame object (retention time x trace).
#' @import reticulate
#' @export uv_converter
uv_converter <- function(file){
  trace_file <- reticulate::import("aston.tracefile")
  pd <- reticulate::import("pandas")
  df <- trace_file$TraceFile(file)
  df <- pd$DataFrame(df$data$values, columns=df$data$columns,
               index=df$data$index)
  # multiply by empirical correction value
  apply(df,2,function(xx)xx*0.9536743164062551070259132757200859487056732177734375)
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
