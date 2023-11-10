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
        xx <- read.csv(file, skip = header[[2]], sep = sep, colClasses = "numeric",
                       na.strings = c("[FractionCollectionReport]", "#ofFractions"),
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

