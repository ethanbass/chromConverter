#' Read Chromatograms
#'
#' Reads chromatograms from specified folders or vector of paths using file parsers
#' from [Aston](https://github.com/bovee/aston), [Entab](https://github.com/bovee/entab),
#' and [ThermoRawFileParser](https://github.com/compomics/ThermoRawFileParser).
#'
#' Currently recognizes Agilent ChemStation '.uv', MassHunter '.dad' files, and
#' ThermoRaw files.
#'
#' @name read_chroms
#' @param paths paths to files or folders containing files
#' @param find_files Logical. Set to \code{TRUE} (default) if you are providing
#' the function with a folder or vector of folders containing the files.
#' Otherwise, set to\code{FALSE}.
#' @param format.in Format of files to be imported/converted. The current options
#' are: \code{chemstation_uv}, \code{masshunter_dad}, \code{shimadzu_fid}, or
#' \code{chromeleon_uv}.
#' @param pattern pattern (e.g. a file extension). Defaults to NULL, in which
#' case file extension will be deduced from \code{format.in}.
#' @param parser What parser to use. Current option are \code{aston} or \code{
#' entab}. Entab must be manually installed from github.
#' @param R.format R object format (i.e. data.frame or matrix).
#' @param export Logical. If TRUE, will export files as csvs.
#' @param path.out Path for exporting files. If path not specified, files will
#' export to current working directory.
#' @param format.out Output format. Currently only \code{.csv}.
#' @param read_metadata Logical, whether to attach metadata (if it's available).
#' Defaults to TRUE.
#' @param dat Existing list of chromatograms to append results.
#' (Defaults to NULL).
#' @return A list of chromatograms in matrix or data.frame format, according to
#' the value of 'R.format'.
#' @import reticulate
#' @importFrom utils write.csv
#' @importFrom purrr partial
#' @examplesIf interactive()
#' path <- "tests/testthat/testdata/dad1.uv"
#' chr <- read_chroms(path, find_files = FALSE, format.in = "chemstation_uv")
#' @author Ethan Bass
#' @export read_chroms
read_chroms <- function(paths, find_files = TRUE,
                        format.in=c("chemstation_uv", "masshunter_dad", "shimadzu_fid", "chromeleon_uv",
                                   "thermoraw", "mzml"),
                        pattern=NULL, parser=c("aston","entab"),
                        R.format=c("matrix","data.frame"), export=FALSE,
                        path.out=NULL, format.out = "csv", read_metadata = TRUE,
                        dat=NULL){
  format.in <- match.arg(format.in, c("chemstation_uv", "masshunter_dad", "shimadzu_fid", "chromeleon_uv"))
  R.format <- match.arg(R.format, c("matrix", "data.frame"))
  parser <- match.arg (parser, c("aston","entab"))
  if (parser == "entab" & !requireNamespace("entab", quietly = TRUE)) {
    stop("The entab R package must be installed to use entab parsers:
      install.packages('entab', repos='https://ethanbass.github.io/drat/')",
      call. = FALSE)
  }
  R.format <- match.arg(R.format, c("matrix", "data.frame"))
  format.in <- match.arg(format.in, c("chemstation_uv", "masshunter_dad", "shimadzu_fid", "chromeleon_uv",
                                   "thermoraw", "mzml"))
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
  if (format.in == "masshunter_dad"){
    pattern <- ifelse(is.null(pattern), ".sp", pattern)
    converter <- if (parser == "aston"){
      sp_converter
    } else{
      partial(entab_reader, read_metadata = read_metadata, format_out = R.format)
    }
  } else if (format.in == "chemstation_uv"){
    pattern <- ifelse(is.null(pattern), ".uv", pattern)
    converter <- if (parser == "aston"){
      uv_converter
    } else{
      partial(entab_reader, read_metadata = read_metadata, format_out = R.format)
    }
  } else if(format.in == "chromeleon_uv"){
    pattern <- ".txt"
    converter <- partial(chromeleon_converter, read_metadata = read_metadata, format_out = R.format)
  } else if (format.in == "shimadzu_fid"){
    pattern <- ".txt"
    converter <- partial(shimadzu_fid_converter, read_metadata = read_metadata, format_out = R.format)
  } else if (format.in == "thermoraw"){
    pattern <- ".raw"
    converter <- partial(read_thermoraw, path_out = path.out)
  } else if (format.in == "mzml"){
    pattern <- ".mzML"
    converter <- read_mzml
  } else{
    converter <- ifelse(parser == "aston", trace_converter, entab_reader)
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
  if (format.in %in% c("chemstation_uv", "masshunter_dad")){
    file_names <- strsplit(files, "/")
    file_names <- gsub("\\.[Dd]", "",
                       sapply(file_names, function(n) n[grep("\\.[Dd]", n)]))
  } else file_names <- sapply(strsplit(basename(files),"\\."), function(x) x[1])
  data <- lapply(X=files, function(file){
    df <- try(converter(file),silent = TRUE)
  })
  errors <- which(sapply(data, function(x) inherits(x,"try-error")))
  if (length(errors) > 0){
    warning(paste("The following chromatograms could not be interpreted:", errors))
    data <- data[-errors]
    file_names <- file_names[-errors]
  }
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
entab_reader <- function(file, format_data = c("wide","long"),
                         format_out = c("matrix", "data.frame"),
                         read_metadata = TRUE){
  if (!requireNamespace("entab", quietly = TRUE)){
    stop("The entab R package must be installed to use entab parsers:
      install.packages('entab', repos='https://ethanbass.github.io/drat/')",
      call. = FALSE)
  }
  format_out <- match.arg(format_out, c("matrix","data.frame"))
  format_data <- match.arg(format_data, c("wide","long"))
  r <- entab::Reader(file)
  df <- entab::as.data.frame(r)
  if (format_data == "wide"){
    times <- df[df$wavelength == df$wavelength[1], "time"]
    df <- as.data.frame(pivot_wider(df, id_cols = "time",
                                 names_from = "wavelength",
                                 values_from = "intensity"),
                                  row.names = "time")
    rownames(df) <- times
  }
  if (format_out == "matrix"){
    df <- as.matrix(df)
  }
  if (read_metadata){
    meta <- r$metadata()
    df <- structure(df, instrument = meta$instrument,
              detector = NA,
              software = NA,
              method = meta$method,
              batch = NA,
              operator = meta$operator,
              run_date = meta$run_date,
              sample_name = meta$sample,
              sample_id = NA,
              injection_volume = NA,
              time_range = NA,
              time_interval = NA,
              format = format_data,
              parser = "entab",
              class = format_out)
  }
   df
}

#' Chromeleon converter
#'
#' @name chromeleon_converter
#' @importFrom utils tail read.csv
#' @param file path to file
#' @return A matrix object (retention time x trace).
#' @import reticulate
#' @noRd
chromeleon_converter <- function(file, read_metadata = TRUE,
                                 format_out = c("matrix","data.frame")){
  format_out <- match.arg(format_out, c("matrix","data.frame"))
  xx <- readLines(file)
  start <- tail(grep("Data:", xx), 1)
  x <- read.csv(file, skip = start, sep="\t")
  x <- x[,-2]
  if (any(grepl(",",as.data.frame(x)[-1,2]))){
    x <- apply(x, 2, function(x) gsub("\\.", "", x))
    x <- apply(x, 2, function(x) gsub(",", ".", x))
  }
  x <- apply(x, 2, as.numeric)
  x <- as.matrix(x)
  rownames(x) <- x[,1]
  x <- x[,2, drop=F]
  if (read_metadata){
    meta_fields <- grep("Information:", xx)
    meta <- do.call(rbind,strsplit(xx[(meta_fields[1]+1):(meta_fields[3]-1)],"\t"))
    rownames(meta) <- meta[,1]
    meta <- as.list(meta[,-1])
    x <- structure(x, instrument = NA,
                    detector = meta$Detector,
                    software = meta$`Generating Data System`,
                    method = meta$`Instrument Method`,
                    batch = NA,
                    operator = meta$`Operator`,
                    run_date = c(date=meta$`Injection Date`, time=meta$`Injection Time`),
                    sample_name = meta$Injection,
                    sample_id = NA,
                    injection_volume = meta[[grep("Injection Volume", names(meta))]],
                    time_range = c(meta$`Time Min. (min)`, meta$`Time Max. (min)`),
                    time_interval = meta$`Average Step (s)`,
                    format = "long",
                    parser = "chromConverter",
                    class = format_out)
  }
  x
}

#' Shimadzu FID converter
#'
#'
#' @name shimadzu_converter
#' @importFrom utils tail read.csv
#' @param file path to file
#' @return A matrix object (retention time x trace).
#' @import reticulate
#' @noRd
shimadzu_fid_converter <- function(file, read_metadata = TRUE,
                                   format_out = c("matrix","data.frame")){
  format_out <- match.arg(format_out, c("matrix","data.frame"))
  x <- readLines(file)
  headings <- grep("\\[*\\]", x)
  chrom.idx <- grep("\\[Chromatogram .*]", x)
  xx <- read.csv(file, skip = chrom.idx + 4, sep="\t", colClasses="numeric",
                 na.strings=c("[FractionCollectionReport]","#ofFractions"))
  xx <- xx[!is.na(xx[,1]),]
  xx <- as.matrix(xx)
  rownames(xx) <- xx[,1]
  xx <- xx[, 2, drop = FALSE]
  if (format_out == "data.frame")
    xx <- as.data.frame(xx)
  if (read_metadata){
    meta_start <- headings[1]
    meta_end <- headings[7]
    meta <- do.call(rbind, strsplit(x[meta_start+1:(meta_end-1)],"\t"))
    meta2 <- do.call(rbind, strsplit(x[(chrom.idx+1):(chrom.idx+4)],"\t"))
    meta <- rbind(meta ,meta2)
    rownames(meta) <- meta[,1]
    meta <- as.list(meta[,-1])
    xx <- structure(xx, instrument = meta$`Instrument Name`,
                        detector = meta$`Detector Name`,
                        software = c(software = meta$`Application Name`, version = meta$Version),
                        method = meta$`Method File`,
                        batch = meta$`Batch File`,
                        operator = meta$`Operator Name`,
                        run_date = meta$Acquired,
                        sample_name = meta$`Sample Name`,
                        sample_id = meta$`Sample ID`,
                        injection_volume = meta$`Injection Volume`,
                        time_range = c(meta$`Start Time(min)`, meta$`End Time(min)`),
                        time_interval = meta$`Interval(msec)`,
                        format = "long",
                        parser = "chromConverter",
                        class = format_out)
  }
  xx
}
