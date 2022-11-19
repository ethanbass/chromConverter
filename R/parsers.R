#' Chromeleon ASCII reader
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
    meta <- try(read_chromeleon_metadata(xx))
    if (!inherits(meta, "try-error")){
      x <- attach_metadata(x, meta, format_in = "chromeleon", format_out = format_out,
                           data_format = "wide", parser = "chromConverter")
    }
  }
  x
}

#' Shimadzu ascii reader
#'
#' @name read_shimadzu
#' @importFrom utils tail read.csv
#' @importFrom stringr str_split_fixed
#' @param file path to file
#' @param format_in Format of files. \code{fid} or \code{dad}.
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param read_metadata Whether to read metadata from file.
#' @param what Whether to extract \code{chromatogram}, \code{peak_table} or
#' \code{both}.
#' @return A chromatogram in the format specified by \code{format_out}
#' (retention time x wavelength).
#' @author Ethan Bass
#' @export
read_shimadzu <- function(file, format_in,
                          format_out = c("matrix","data.frame"),
                          data_format = c("wide","long"),
                          what = "chromatogram", read_metadata = TRUE){
  if (missing(format_in))
    stop("`format_in` must be specified. The options are `fid` or `dad`.")
  format_out <- match.arg(format_out, c("matrix","data.frame"))
  data_format <- match.arg(data_format, c("wide","long"))
  what <- match.arg(what, c("chromatogram", "peak_table"), several.ok = TRUE)
  x <- readLines(file)
  headings <- grep("\\[*\\]", x)
  peaktab.idx <- grep("\\[Peak Table", x)
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
    data_format <- "long"
  } else if (format_in == "dad"){
    chrom.idx <- grep("\\[PDA 3D]", x)
    # grep("\\[PDA Multi Chromatogram", x)
    # grep("\\[LC Status Trace", x)
    if (any(what == "chromatogram")){
      header <- extract_header(x, chrom.idx)
      met <- header[[1]]
      xx <- read.csv(file, skip = header[[2]], sep="\t", colClasses="numeric",
                     na.strings=c("[FractionCollectionReport]","#ofFractions"), row.names = 1,
                     nrows = as.numeric(met[6,2]))
      xx <- as.matrix(xx[!is.na(xx[,1]),])
      times <- round(seq(met[2,2], met[3,2], length.out = as.numeric(met[7,2])),2)
      wavelengths <- round(seq(met[4,2], met[5,2], length.out = as.numeric(met[6,2])),2)
      colnames(xx) <- wavelengths
      if (data_format == "long"){
        xx <- reshape_chrom(xx)
      }
    }
  }
    if (any(what == "peak_table")){
      if (length(peaktab.idx) == 0){
        if (length(what) == 1){
          stop("Peak table not found!")
        } else{
          warning("Peak table not found!")
          what <- "chromatogram"
        }
      }
      peak_tab <-lapply(peaktab.idx, function(idx){
        nrows <- as.numeric(strsplit(x[idx+1],"\t")[[1]][2])
        peak_tab <- read.csv(file, skip = (idx+1), sep="\t", nrows=nrows)
      })
      names(peak_tab) <- gsub("\\[|\\]","", x[peaktab.idx])
    }
  if (format_out == "data.frame"){
    xx <- as.data.frame(xx)
  }
  if (read_metadata){
   idx <-  which(x[headings] %in%
            c("[Header]", "[File Information]", "[Sample Information]",
              "[Original Files]", "[File Description]", "[Configuration]")
          )
    meta_start <- headings[min(idx)]
    meta_end <- headings[max(idx) + 1]
    meta <- x[(meta_start+1):(meta_end-1)]
    meta <- meta[meta!=""]
    meta <- meta[-grep("\\[", meta)]
    meta <- stringr::str_split_fixed(meta, "\t", n = 2)
    meta <- rbind(meta, met)
    rownames(meta) <- meta[, 1]
    meta <- as.list(meta[,2])
    data_format <- switch(format_in,
                          "fid" = "long",
                          "dad" = "wide")
    xx <- attach_metadata(xx, meta, format_in = "shimadzu", format_out = format_out,
                          data_format = data_format,
                          parser = "chromConverter")
  }
  if ("peak_table" %in% what & "chromatogram" %in% what)
    what <- "both"
  switch(what, "chromatogram" = xx,
         "peak_table" = peak_tab,
         "both" = list(xx, peak_tab))
}

#' Waters ascii (.arw) reader
#'
#' Reads 'Waters ARW' files.
#'
#' For help exporting files from Empower, you can consult the official
#' documentation: [How_to_export_3D_raw_data_from_Empower](https://support.waters.com/KB_Inf/Empower_Breeze/WKB77571_How_to_export_3D_raw_data_from_Empower_to_a_Microsoft_Excel_spreadsheet).
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
    meta <- try(read_waters_metadata(file))
    if (!inherits(meta, "try-error")){
      x <- attach_metadata(x, meta, format_in = "waters_arw",
                           format_out = format_out,
                           data_format = "wide",
                           parser = "chromConverter")
    }
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

#' Extract data from mzML files
#'
#' Extracts data from mzML files using parsers from either RaMS or mzR. The RaMS
#' parser (default) will only return data in tidy (long) format. mzR will return
#' data in wide format. Currently the mzR-based parser only returns DAD data.
#'
#' @name read_mzml
#' @importFrom RaMS grabMSdata
#' @param path path to file
#' @param format_out R format. Only applies if \code{mzR} is selected.
#' Either \code{matrix} or \code{data.frame}. \code{RaMS} will return
#' a list of data.tables regardless of what is selected here.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param parser What parser to use. Either \code{RaMS} or \code{mzR}.
#' @param what What types of data to return (argument to \code{\link[RaMS]{grabMSdata}}.
#' Options include \code{MS1}, \code{MS2}, \code{BPC}, \code{TIC}, \code{DAD},
#' \code{chroms}, \code{metadata}, or \code{everything}).
#' @param ... Additional arguments to \code{\link[RaMS]{grabMSdata}}.
#' @return If \code{RaMS} is selected, the function will return a list of "tidy"
#' \code{data.table} objects. If \code{mzR} is selected, the function will return a
#' chromatogram in \code{matrix} or \code{data.frame} format according to the
#' value of \code{format_out}.
#' @author Ethan Bass
#' @export read_mzml

read_mzml <- function(path, format_out = c("matrix", "data.frame"),
                      data_format = c("wide","long"),
                      parser=c("RaMS","mzR"),
                      what=c("MS1","MS2", "BPC", "TIC", "DAD",
                             "chroms", "metadata", "everything"), ...){
  parser <- match.arg(parser, c("RaMS", "mzR"))
  format_out <- match.arg(format_out, c("matrix", "data.frame"))
  data_format <- match.arg(data_format, c("wide","long"))
  what <- match.arg(what, c("MS1","MS2", "BPC", "TIC", "DAD",
                            "chroms", "metadata", "everything"), several.ok = TRUE)
  if (all(c("MS1","MS2", "BPC", "TIC", "DAD",
            "chroms", "metadata", "everything") %in% what)){
        what <- grep("everything",what, invert = TRUE,value = TRUE)
            }
  if (parser == "RaMS"){
    data <- RaMS::grabMSdata(path, grab_what = what, ...)
  }
  if (parser == "mzR"){
    if (!requireNamespace("mzR", quietly = TRUE)) {
      stop(
        "The `mzR` package must be installed from Bioconductor to read `mzML` files:
      BiocManager::install('mzR')",
        call. = FALSE)
    }
    x <- mzR::openMSfile(path)
    info <- mzR::header(x)
    UV_scans <- which(info$msLevel==0)
    rts <- info[UV_scans,"retentionTime"]
    lambdas <- seq(info$scanWindowLowerLimit[UV_scans[1]], info$scanWindowUpperLimit[UV_scans[1]])
    pks <- mzR::peaks(x)
    data <- t(sapply(UV_scans, function(j) pks[[j]][,2]))
    rownames(data) <- rts
    colnames(data) <- lambdas
    if (data_format == "long"){
      data <- reshape_chrom(data)
    }
    if (format_out == "data.frame"){
      data <-as.data.frame(data)
    }
  }
  data
}
