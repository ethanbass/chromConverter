#' Converter for Agilent MassHunter UV files
#'
#' Converts a single chromatogram from MassHunter \code{.sp} format to R \code{data.frame}.
#'
#' Uses the [Aston](https://github.com/bovee/aston) file parser.
#'
#' @name sp_converter
#' @param file path to file
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param read_metadata Logical. Whether to read metadata and attach it to the
#' chromatogram.
#' @return A chromatogram in \code{data.frame} format (retention time x wavelength).
#' @import reticulate
#' @export sp_converter
sp_converter <- function(file, format_out = c("matrix", "data.frame"),
                         read_metadata = TRUE){
  check_aston_configuration()
  format_out <- match.arg(format_out, c("matrix","data.frame"))
  x <- trace_file$agilent_uv$AgilentDAD(file)
  x <- pd$DataFrame(x$data$values, columns=x$data$columns,
               index=x$data$index)
  if (format_out == "matrix"){
    x <- as.matrix(x)
  }
  if (read_metadata){
    meta <- read_masshunter_metadata(file)
    x <- attach_metadata(x, meta, format_in = "masshunter_dad",
                         format_out = format_out, format_data = "wide",
                         parser = "aston")
  }
  x
}

#' Converter for Agilent ChemStation UV files
#'
#' Converts a single chromatogram from ChemStation \code{.uv} format to R \code{data.frame}.
#'
#' Uses the [Aston](https://github.com/bovee/aston) file parser.
#'
#' @name uv_converter
#' @param file path to file
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param correction Logical. Whether to apply empirical correction. Defaults is
#' TRUE.
#' @param read_metadata Logical. Whether to read metadata and attach it to the
#' chromatogram.
#' @return A chromatogram in \code{data.frame} format (retention time x wavelength).
#' @import reticulate
#' @export uv_converter
uv_converter <- function(file, format_out = c("matrix","data.frame"),
                         correction=TRUE, read_metadata = TRUE){
  check_aston_configuration()
  format_out <- match.arg(format_out, c("matrix","data.frame"))
  trace_file <- reticulate::import("aston.tracefile")
  pd <- reticulate::import("pandas")
  x <- trace_file$TraceFile(file)
  x <- pd$DataFrame(x$data$values, columns=x$data$columns,
                    index=x$data$index)
  if (format_out == "matrix"){
    x <- as.matrix(x)
  }
  if (correction){
    # multiply by empirical correction value
    x <- apply(x,2,function(xx)xx*0.9536743164062551070259132757200859487056732177734375)
  }
  # correct column order
  # x <- lapply(x, function(xx) xx[,order(as.numeric(colnames(xx)))])
  if (read_metadata){
    meta <- read_chemstation_metadata(file)
    x <- attach_metadata(x, meta, format_in = "chemstation_uv",
                         format_out = format_out,format_data = "wide",
                         parser = "Aston")
  }
  x
}

#' Aston TraceFile Converter
#'
#' Uses Aston parser to figure out file-type and convert to R \code{data.frame}.
#' @name trace_converter
#' @title generic converter for other types of files
#' @param file path to file
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @return A chromatogram in \code{data.frame} format (retention time x wavelength).
#' @import reticulate
#' @noRd
trace_converter <- function(file, format_out = c("matrix", "data.frame")){
  check_aston_configuration()
  format_out <- match.arg(format_out, c("matrix","data.frame"))
  trace_file <- reticulate::import("aston.tracefile")
  pd <- reticulate::import("pandas")
  x <- trace_file$TraceFile(file)
  x <- pd$DataFrame(x$data$values, columns=x$data$columns,
               index=x$data$index)
  if (format_out == "matrix"){
    x <- as.matrix(x)
  }
  x
}

#' Configure Aston
#'
#' Configures reticulate to use Aston file parsers.
#' @name configure_aston
#' @return No return value.
#' @author Ethan Bass
#' @import reticulate
#' @export
configure_aston <- function(){
  install <- FALSE
  # path <- miniconda_path()
  if (!dir.exists(miniconda_path())){
    install <- readline("It is recommended to install miniconda in your R library to use Aston parsers. Install miniconda now? (y/n)")
  if (install %in% c('y', "Y", "YES", "yes", "Yes")){
    install_miniconda()
  }
  } # else{
    # envs <- conda_list()
    # use_miniconda(envs[grep("r-reticulate", envs$name)[1],2])
  # }
  env <- reticulate::configure_environment("chromConverter")
  if (!env){
    reqs <- c("pandas","scipy","numpy","aston")
    reqs_available <- sapply(reqs, reticulate::py_module_available)
    if (!all(reqs_available)){
      conda_install(reqs[which(!reqs_available)], pip = TRUE)
    }
  }
    assign_trace_file()
}

check_aston_configuration <- function(){
  assign_trace_file()
  if (length(trace_file) == 0){
    ans <- readline("Aston not found. Configure Aston? (y/n)?")
    if (ans %in% c('y', "Y", "YES", "yes", "Yes")){
      configure_aston()
    }
  }
}

assign_trace_file <- function(){
  pos <- 1
  envir = as.environment(pos)
  assign("trace_file", reticulate::import("aston.tracefile"), envir = envir)
  assign("pd", reticulate::import("pandas"), envir = envir)
  assign("csv", reticulate::import("csv"), envir = envir)
}

#' @name call_entab
#' @title Entab parsers
#' @param file path to file
#' @param format_data Whether to output data in wide or long format.
#' @param format_in Format of input.
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param read_metadata Whether to read metadata from file.
#' @return A chromatogram in the format specified by \code{format_out}
#' (retention time x wavelength).
#' @importFrom tidyr pivot_wider
#' @export
call_entab <- function(file, format_data = c("wide","long"),
                         format_in = "",
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
  x <- entab::as.data.frame(r)
  if (format_data == "wide"){
    times <- x[x$wavelength == x$wavelength[1], "time"]
    x <- as.data.frame(pivot_wider(x, id_cols = "time",
                                   names_from = "wavelength",
                                   values_from = "intensity"),
                       row.names = "time")
    rownames(x) <- times
    x <- x[,-1]
  }
  if (format_out == "matrix"){
    x <- as.matrix(x)
  }
  if (read_metadata){
    meta <- r$metadata()
    if (format_in == "chemstation_uv"){
      meta <- c(meta, read_chemstation_metadata(file))
    }
    if (format_in == "masshunter_dad"){
      meta <- c(meta, read_masshunter_metadata(file))
    }
    x <- attach_metadata(x, meta, format_in = format_in, format_out = format_out,
                         format_data = format_data, parser = "entab")
  }
  x
}

#' Chromeleon ascii reader
#'
#' @importFrom utils tail read.csv
#' @param file path to file
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param read_metadata Whether to read metadata from file.
#' @return A chromatogram in the format specified by \code{format_out}
#' (retention time x wavelength).
#' @author Ethan Bass
#' @export
read_chromeleon <- function(file, format_out = c("matrix","data.frame"),
                            read_metadata = TRUE){
  format_out <- match.arg(format_out, c("matrix","data.frame"))
  xx <- readLines(file)
  start <- tail(grep("Data:", xx), 1)
  x <- read.csv(file, skip = start, sep="\t")
  x <- x[,-2, drop = FALSE]
  if (any(grepl(",",as.data.frame(x)[-1,2]))){
    x <- apply(x, 2, function(x) gsub("\\.", "", x))
    x <- apply(x, 2, function(x) gsub(",", ".", x))
  }
  x <- apply(x, 2, as.numeric)
  x <- as.matrix(x)
  rownames(x) <- x[,1]
  x <- x[,2, drop = FALSE]
  if (read_metadata){
    meta_fields <- grep("Information:", xx)
    meta <- do.call(rbind,strsplit(xx[(meta_fields[1]+1):(meta_fields[3]-1)],"\t"))
    rownames(meta) <- meta[,1]
    meta <- as.list(meta[,-1])
    x <- attach_metadata(x, meta, format_in = "chromeleon", format_out = format_out,
                         parser = "chromConverter")
  }
  x
}

#' Shimadzu ascii reader
#'
#' @name read_shimadzu
#' @importFrom utils tail read.csv
#' @importFrom stringr str_split_fixed
#' @param file path to file
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param format_in Format of files. \code{fid} or \code{dad}.
#' @param read_metadata Whether to read metadata from file.
#' @param what Whether to extract \code{chromatogram}, \code{peak_table} or
#' \code{both}.
#' @return A chromatogram in the format specified by \code{format_out}
#' (retention time x wavelength).
#' @author Ethan Bass
#' @export
read_shimadzu <- function(file, format_in, read_metadata = TRUE,
                          format_out = c("matrix","data.frame"),
                          what = c("chromatogram", "peak_table", "both")){
  if (missing(format_in))
    stop("`format_in` must be specified. The options are `fid` or `dad`.")
  format_out <- match.arg(format_out, c("matrix","data.frame"))
  what <- match.arg(what, c("chromatogram", "peak_table", "both"))
  x <- readLines(file)
  headings <- grep("\\[*\\]", x)
  if (format_in == "fid"){
    chrom.idx <- grep("\\[Chromatogram .*]", x)
    header <- extract_header(x, chrom.idx)
    met<-header[[1]]
    xx <- read.csv(file, skip = header[[2]], sep="\t", colClasses="numeric",
                   na.strings=c("[FractionCollectionReport]","#ofFractions"))
    xx <- as.matrix(xx[!is.na(xx[,1]),])
    rownames(xx) <- xx[,1]
    xx <- xx[, 2, drop = FALSE]
    colnames(xx) <- "Intensity"
    format <- "long"
  } else if (format_in == "dad"){
    format <- "wide"
    chrom.idx <- grep("\\[PDA 3D]", x)
    # grep("\\[PDA Multi Chromatogram", x)
    # grep("\\[LC Status Trace", x)
    peaktab.idx <- grep("\\[Peak Table", x)
    if (what != "peak_table"){
      header <- extract_header(x, chrom.idx)
      met <- header[[1]]
      xx <- read.csv(file, skip = header[[2]], sep="\t", colClasses="numeric",
                     na.strings=c("[FractionCollectionReport]","#ofFractions"), row.names = 1,
                     nrows = as.numeric(met[6,2]))
      xx <- as.matrix(xx[!is.na(xx[,1]),])
      times <- round(seq(met[2,2], met[3,2], length.out = as.numeric(met[7,2])),2)
      wavelengths <- round(seq(met[4,2], met[5,2], length.out = as.numeric(met[6,2])),2)
      colnames(xx) <- wavelengths
    }
    if (what != "chromatogram"){
      peak_tab <-lapply(peaktab.idx, function(idx){
        nrows <- as.numeric(strsplit(x[idx+1],"\t")[[1]][2])
        peak_tab <- read.csv(file, skip = (idx+1), sep="\t", nrows=nrows)
      })
      names(peak_tab) <- gsub("\\[|\\]","", x[peaktab.idx])
    }
  }
  if (format_out == "data.frame"){
    xx <- as.data.frame(xx)
  }
  if (read_metadata){
    meta_start <- headings[1]
    meta_end <- headings[7]
    meta <- x[meta_start+1:(meta_end-1)]
    meta <- meta[meta!=""]
    meta<-meta[-grep("\\[", meta)]
    meta <- stringr::str_split_fixed(meta,"\t",n=2)
    meta <- rbind(meta, met)
    rownames(meta) <- meta[,1]
    meta <- as.list(meta[,2])
    xx <- attach_metadata(xx, meta, format_in = "shimadzu", format_out = format_out,
                          parser = "chromConverter")
  }
  switch(what, "chromatogram" = xx,
         "peak_table" = peak_tab,
         "both" = list(xx, peak_tab))
}

#' Waters ascii (.arw) reader
#'
#' @name read_waters_arw
#' @importFrom utils tail read.csv
#' @param file path to file
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param read_metadata Whether to read metadata from file.
#' @return A chromatogram in the format specified by \code{format_out}
#' (retention time x wavelength).
#' @author Ethan Bass
#' @export
read_waters_arw <- function(file, read_metadata = TRUE,
                            format_out = c("matrix","data.frame")){
  format_out <- match.arg(format_out, c("matrix","data.frame"))
  x <- read.csv(file, sep="\t", skip = 2, header=FALSE, row.names=1)
  if (format_out == "matrix")
    x <- as.matrix(x)
  if (read_metadata){
    meta <- gsub("\\\"", "", do.call(cbind,strsplit(readLines(file, n = 2),"\t")))
    rownames(meta) <- meta[,1]
    meta <- as.list(meta[,-1])
    x <- attach_metadata(x, meta, format_in = "waters_arw",
                         format_out = format_out,
                         parser = "chromConverter")
  }
  x
}

#' Chemstation CSV reader
#'
#' @name read_chemstation_csv
#' @importFrom utils tail read.csv
#' @param file path to file
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @return A chromatogram in the format specified by \code{format_out}
#' (retention time x wavelength).
#' @author Ethan Bass
#' @export
read_chemstation_csv <- function(file, format_out = c("matrix","data.frame")){
  format_out <- match.arg(format_out, c("matrix", "data.frame"))
  x <- read.csv(file, row.names = 1, header = TRUE,
                fileEncoding="utf-16",check.names = FALSE)
  if (format_out == "matrix"){
    x <- as.matrix(x)
  }
  x
}
