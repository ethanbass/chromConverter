#' Chromeleon ASCII reader
#'
#' @importFrom utils tail read.csv
#' @param file path to file
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param read_metadata Whether to read metadata from file.
#' @param metadata_format Format to output metadata. Either \code{chromconverter} or
#' \code{raw}.
#' @return A chromatogram in the format specified by \code{format_out}.
#' (retention time x wavelength).
#' @author Ethan Bass
#' @export

read_chromeleon <- function(file, format_out = c("matrix","data.frame"),
                            data_format = c("wide","long"),
                            read_metadata = TRUE,
                            metadata_format = c("chromconverter", "raw")){
  format_out <- match.arg(format_out, c("matrix","data.frame"))
  data_format <- match.arg(data_format, c("wide","long"))
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  metadata_format <- switch(metadata_format, chromconverter = "chromeleon",
                           raw = raw)
  xx <- readLines(file)
  xx <- remove_unicode_chars(xx)
  start <- tail(grep("Data:", xx), 1)
  x <- read.csv(file, skip = start, sep = "\t", row.names = NULL)
  x <- x[,-2, drop = FALSE]
  x <- x[,colSums(is.na(x)) < nrow(x)]
  if (any(grepl(",",as.data.frame(x)[-1, 2]))){
    decimal_separator <- ","
    x <- apply(x, 2, function(x) gsub("\\.", "", x))
    x <- apply(x, 2, function(x) gsub(",", ".", x))
  } else {
    decimal_separator <- "."
  }
  x <- apply(x, 2, as.numeric)
  colnames(x) <- c("RT","Intensity")
  if (data_format == "wide"){
    rownames(x) <- x[,1]
    x <- x[, 2, drop = FALSE]
  }
  if (format_out == "data.frame"){
    x <- as.data.frame(x)
  }
  if (read_metadata){
    meta <- try(read_chromeleon_metadata(xx))
    if (decimal_separator == ","){
      meta <- lapply(meta, function(x) gsub(",",".",x))
    }
    if (!inherits(meta, "try-error")){
      x <- attach_metadata(x, meta, format_in = metadata_format, format_out = format_out,
                           data_format = data_format, parser = "chromConverter",
                           source_file = file)
    }
  }
  x
}

#' Shimadzu ascii reader
#'
#' Reads 'Shimadzu' ascii files into R. These files are exported from
#' 'Lab Solutions' by right clicking on samples in the sample list and
#' selecting
#'
#' @name read_shimadzu
#' @importFrom utils tail read.csv
#' @importFrom stringr str_split_fixed
#' @param file path to file
#' @param format_in Format of files. \code{fid} or \code{dad}.
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param peaktable_format Whether to return peak tables in \code{chromatographr} or
#' \code{original} format.
#' @param what Whether to extract \code{chromatogram} and/or \code{peak_table}.
#' Accepts multiple arguments.
#' @param read_metadata Whether to read metadata from file.
#' @param metadata_format Format to output metadata. Either \code{chromconverter} or
#' \code{raw}.
#' @return A chromatogram in the format specified by \code{format_out}
#' (retention time x wavelength).
#' @author Ethan Bass
#' @export

read_shimadzu <- function(file, format_in,
                          format_out = c("matrix", "data.frame"),
                          data_format = c("wide", "long"),
                          peaktable_format = c("chromatographr", "original"),
                          what = "chromatogram",
                          read_metadata = TRUE,
                          metadata_format = c("chromconverter", "raw")){
  if (missing(format_in))
    stop("`format_in` must be specified. The options are `fid` or `dad`.")
  format_out <- match.arg(format_out, c("matrix", "data.frame"))
  data_format <- match.arg(data_format, c("wide", "long"))
  peaktable_format <- match.arg(peaktable_format, c("chromatographr","original"))
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  what <- match.arg(what, c("chromatogram", "peak_table"), several.ok = TRUE)
  x <- readLines(file)
  sep <- substr(x[2], 17, 17)
  headings <- grep("\\[*\\]", x)
  peaktab.idx <- grep("\\[Peak Table", x)
  chrom_heading <- switch(format_in,
                          "fid" = "\\[Chromatogram .*]",
                          "dad" = "\\[PDA 3D]")
  chrom.idx <- grep(chrom_heading, x)

  if (any(what == "chromatogram")){
    if (length(chrom.idx) != 0){
      header <- try(extract_shimadzu_header(x = x, chrom.idx = chrom.idx, sep = sep))
      met <- header[[1]]
      decimal_separator <- ifelse(grepl(",", met[2, 2]), ",", ".")
      if (decimal_separator == ","){
        met[c(2:3), 2] <- gsub(",", ".", met[c(2:3), 2])
      }

      if (format_in == "fid"){
        xx <- read.csv(file, skip = header[[2]], sep = sep, colClasses = "numeric",
                       na.strings = c("[FractionCollectionReport]","#ofFractions"),
                       dec = decimal_separator)
        xx <- as.matrix(xx[!is.na(xx[,1]),])
        rownames(xx) <- xx[, 1]
        xx <- xx[, 2, drop = FALSE]
        colnames(xx) <- "Intensity"
        if (data_format == "long"){
          xx <- cbind(RT = as.numeric(rownames(xx)), Intensity = as.numeric(xx[,1]))
        }
      } else if (format_in == "dad"){
        nrows <- as.numeric(met[grep("# of Time Axis Points", met[,1]), 2])
        ncols <- as.numeric(met[grep("# of Wavelength Axis Points", met[,1]), 2])
        xx <- read.csv(file, skip = header[[2]], sep = sep, colClasses="numeric",
                       na.strings = c("[FractionCollectionReport]","#ofFractions"),
                       row.names = 1, nrows = nrows, dec = decimal_separator)
        xx <- as.matrix(xx[!is.na(xx[,1]),])
        colnames(xx) <- as.numeric(gsub("X", "", colnames(xx)))*0.01
        if (data_format == "long"){
          xx <- reshape_chrom(xx, data_format = "long")
        }
      }
      if (format_out == "data.frame"){
        xx <- as.data.frame(xx)
      }
    } else{
      if (length(what) == 1){
        stop("Chromatogram not found.")
      } else{
        warning("Chromatogram not found.")
        what = "peak_table"
      }
    }
  }

  ### extract peak_table
  if (any(what == "peak_table")){
    if (length(peaktab.idx) == 0){
      if (length(what) == 1){
        stop("Peak table not found!")
      } else{
        warning("Peak table not found!")
        what <- "chromatogram"
      }
    }
    peak_tab <- lapply(peaktab.idx, function(idx){
      nrows <- as.numeric(strsplit(x = x[idx + 1], split = sep)[[1]][2])
      if (!is.na(nrows) && nrows > 0){
        time_column <- grep("R.Time", strsplit(x = x[[idx + 2]], split = sep)[[1]])
        t1 <- strsplit(x = x[[idx + 3]], split = sep)[[1]][time_column]
        decimal_separator <- ifelse(grepl(".", t1), ".", ",")

        peak_tab <- read.csv(file, skip = (idx + 1), sep = sep, nrows = nrows,
                             dec = decimal_separator)
        if (peaktable_format == "chromatographr"){
          peak_tab <- peak_tab[, c("R.Time", "I.Time", "F.Time", "Area", "Height")]
          colnames(peak_tab) <- c("rt", "start", "end", "area", "height")
          # cbind(sample = gsub("\\[|\\]","", x[idx]), peak_tab)
        }
        peak_tab
      } else{NA}
    })
    names(peak_tab) <- gsub("\\[|\\]","", x[peaktab.idx])
  }
  if ("peak_table" %in% what & "chromatogram" %in% what){
    what <- "both"
  }
  if (format_out == "data.frame"){
    xx <- as.data.frame(xx)
  }
  xx <- switch(what, "chromatogram" = xx,
               "peak_table" = peak_tab,
               "both" = list(chromatogram = xx, peak_table = peak_tab))
  if (read_metadata){
    idx <- which(x[headings] %in% c("[Header]", "[File Information]",
                                    "[Sample Information]", "[Original Files]",
                                    "[File Description]", "[Configuration]") )
    meta_start <- headings[min(idx)]
    meta_end <- headings[max(idx) + 1]
    meta <- x[(meta_start + 1):(meta_end - 1)]
    meta <- meta[meta!=""]
    meta <- meta[-grep("\\[", meta)]
    meta <- stringr::str_split_fixed(meta, pattern = sep, n = 2)
    if (exists("met")){
      meta <- rbind(meta, met)
    }
    rownames(meta) <- meta[, 1]
    meta <- as.list(meta[,2])
    if (inherits(xx, "list")){
      xx <- lapply(xx, function(xxx){
        attach_metadata(xxx, meta, format_in = "shimadzu",
                        source_file = file, format_out = format_out,
                        data_format = data_format, parser = "chromConverter")
      })
    } else{
      xx <- attach_metadata(xx, meta, format_in = "shimadzu",
                            source_file = file, format_out = format_out,
                            data_format = data_format,
                            parser = "chromConverter")
    }
  }
  xx
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
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param read_metadata Whether to read metadata from file.
#' @param metadata_format Format to output metadata. Either \code{chromconverter}
#' or \code{raw}.
#' @return A chromatogram in the format specified by \code{format_out}
#' (retention time x wavelength).
#' @author Ethan Bass
#' @export

read_waters_arw <- function(file, format_out = c("matrix", "data.frame"),
                            data_format = c("wide", "long"),
                            read_metadata = TRUE,
                            metadata_format = c("chromconverter", "raw")){
  format_out <- match.arg(format_out, c("matrix", "data.frame"))
  data_format <- match.arg(data_format, c("wide", "long"))
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  metadata_format <- switch(metadata_format,
                            chromconverter = "waters_arw", raw = "raw")
  x <- read.csv(file, sep = "\t", skip = 2, header = FALSE, row.names = 1)
  # PDA (3D)
  if (rownames(x)[1] == "Wavelength"){
    colnames(x) <- x[1,]
    rm <- 1
    if (rownames(x)[2] == "Time"){
      rm <- c(rm, 2)
    }
    x <- x[-rm,]
    if (data_format == "long"){
      x <- as.data.frame(reshape_chrom(x, data_format = "long"))
    }
  } else if (ncol(x) == 1){
    colnames(x) <- "Intensity"
    if (data_format == "long"){
      x <- data.frame(RT = rownames(x), Intensity = x[,1])
    }
  }
  if (format_out == "matrix"){
    x <- as.matrix(x)
  }
  if (read_metadata){
    meta <- try(read_waters_metadata(file))
    if (!inherits(meta, "try-error")){
      x <- attach_metadata(x, meta, format_in = metadata_format,
                           format_out = format_out,
                           data_format = data_format,
                           parser = "chromConverter",
                           source_file = file)
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
#' @param verbose Argument to \code{\link[RaMS]{grabMSdata}} controlling \code{verbosity}.
#' @param ... Additional arguments to \code{\link[RaMS]{grabMSdata}}.
#' @return If \code{RaMS} is selected, the function will return a list of "tidy"
#' \code{data.table} objects. If \code{mzR} is selected, the function will return a
#' chromatogram in \code{matrix} or \code{data.frame} format according to the
#' value of \code{format_out}.
#' @author Ethan Bass
#' @export read_mzml

read_mzml <- function(path, format_out = c("matrix", "data.frame"),
                      data_format = c("long","wide"),
                      parser=c("RaMS","mzR"),
                      what=c("MS1","MS2", "BPC", "TIC", "DAD",
                             "chroms", "metadata", "everything"),
                      verbose = FALSE,
                      ...){
  parser <- match.arg(parser, c("RaMS", "mzR"))
  format_out <- match.arg(format_out, c("matrix", "data.frame"))
  data_format <- match.arg(data_format, c("long","wide"))
  what <- match.arg(what, c("MS1","MS2", "BPC", "TIC", "DAD",
                            "chroms", "metadata", "everything"), several.ok = TRUE)
  if (all(c("MS1","MS2", "BPC", "TIC", "DAD",
            "chroms", "metadata", "everything") %in% what)){
        what <- grep("everything",what, invert = TRUE,value = TRUE)
            }
  if (parser == "RaMS"){
    data <- RaMS::grabMSdata(path, grab_what = what, verbosity = verbose, ...)
    if (data_format == "wide"){
      data <- reshape_chroms(data, data_format = "wide")
    }
  } else if (parser == "mzR"){
      if (!requireNamespace("mzR", quietly = TRUE)) {
        stop(
          "The `mzR` package is not installed. Please install it from Bioconductor:
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
        data <- as.data.frame(data)
    }
  }
  data
}
