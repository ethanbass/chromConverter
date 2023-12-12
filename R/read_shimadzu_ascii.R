#' Shimadzu ascii reader
#'
#' Reads 'Shimadzu' ascii files into R. These files are exported from
#' 'Lab Solutions' by right clicking on samples in the sample list and
#' selecting
#'
#' @name read_shimadzu
#' @importFrom utils tail read.csv
#' @importFrom stringr str_split_fixed
#' @param file Path to file.
#' @param what Whether to extract \code{chromatogram}, \code{peak_table}, and/or
#' \code{ms_spectra}. Accepts multiple arguments.
#' @param include Which chromatograms to include. Options are \code{fid},
#' \code{dad}, \code{uv}, \code{tic}, and \code{status}.
#' @param format_in This argument is deprecated and is no longer required.
#' @param format_out R format. Either \code{matrix} or \code{data.frame}.
#' @param data_format Whether to return data in \code{wide} or \code{long} format.
#' @param peaktable_format Whether to return peak tables in \code{chromatographr} or
#' \code{original} format.
#' @param read_metadata Whether to read metadata from file.
#' @param metadata_format Format to output metadata. Either \code{chromconverter} or
#' \code{raw}.
#' @param collapse Whether to collapse lists that only contain a single element.
#' @return A nested list of elements from the specified \code{file}, where the
#' top levels are chromatograms, peak tables, and/or mass spectra according to
#' the value of \code{what}. Chromatograms are returned in the format specified
#' by \code{format_out} (retention time x wavelength).
#' @author Ethan Bass
#' @export

read_shimadzu <- function(file, what = "chromatogram",
                          format_in = NULL,
                          include =  c("fid", "dad", "uv", "tic", "status"),
                          format_out = c("matrix", "data.frame"),
                          data_format = c("wide", "long"),
                          peaktable_format = c("chromatographr", "original"),
                          read_metadata = TRUE,
                          metadata_format = c("chromconverter", "raw"),
                          collapse = TRUE){
  if (!is.null(format_in)){
    warning("The `format_in` argument is deprecated, since the `read_shimadzu`
    function no longer requires you to specify the file format. Please use the
    `include` argument instead to specify which chromatograms you'd like to read.")
  }
  what <- match.arg(what, c("chromatogram", "peak_table", "ms_spectra"),
                    several.ok = TRUE)
  include <- match.arg(include, c("fid", "dad", "uv", "tic", "status"),
                       several.ok = TRUE)
  format_out <- match.arg(format_out, c("matrix", "data.frame"))
  data_format <- match.arg(data_format, c("wide", "long"))
  peaktable_format <- match.arg(peaktable_format, c("chromatographr","original"))
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))

  x <- readLines(file)
  sep <- substr(x[grep("Type", x)[1]], 5, 5)

  ### extract chromatograms ###
  if (any(what == "chromatogram")){

    regex <- c("fid" = "\\[Chromatogram .*]",
               "dad" = "\\[PDA 3D]",
               "uv" = "\\[PDA Multi Chromatogram\\(Ch\\d+\\)]",
               "status" = "\\[LC Status Trace\\(.+\\)]",
               "tic" = "\\[MS Chromatogram\\]")
    regex <- regex[include]
    chrom.idx <- lapply(regex, function(reg) grep(reg,x))
    chrom.idx <- chrom.idx[lengths(chrom.idx) > 0]

    if (length(chrom.idx) == 0){
      if (length(what) == 1){
        stop("Chromatograms not found!")
      } else {
        warning("Chromatograms not found.")
        what <- what[grep("chroms", what, invert = TRUE)]
      }
    }
    chromatogram <- lapply(seq_along(chrom.idx), function(i){
      read_shimadzu_chrom <- switch(names(chrom.idx)[i], "dad" = read_shimadzu_dad,
                                    read_shimadzu_chromatogram)
      xx <- lapply(chrom.idx[[i]], function(idx){
        read_shimadzu_chrom(file = file, x = x, chrom.idx = idx,
                            sep = sep, data_format = data_format,
                            read_metadata = TRUE, format_out = format_out)
      })
      names(xx) <- x[chrom.idx[[i]]]
      if (collapse) xx <- collapse_list(xx)
      xx
    })
    names(chromatogram) <- names(chrom.idx)
  }

  ### extract peak tables ###
  if (any(what == "peak_table")){
    peaktab.idx <- grep("\\[Peak Table|\\[MC Peak Table", x)
    pktab_type <- substr(x[peaktab.idx], 2, 3)

    if (length(peaktab.idx) == 0){
      if (length(what) == 1){
        stop("Peak table not found!")
      } else{
        warning("Peak table not found!")
        what <- what[grep("peak_table", what, invert = TRUE)]
      }
    }
    peak_table <- lapply(seq_along(peaktab.idx), function(i){
      read_shimadzu_peaktable(file = file, x, idx = peaktab.idx[i], sep = sep,
                              format_in = pktab_type[i],
                              format_out = peaktable_format)
    })
    names(peak_table) <- gsub("\\[|\\]","", x[peaktab.idx])
  }

  ### extract MS spectra ###
  if (any(what == "ms_spectra")){
    spectra.idx <- grep("\\[MS Spectrum\\]", x)
    if (length(spectra.idx) == 0){
      if (length(what) == 1){
        stop("MS spectra were not found!")
      } else{
        warning("MS spectra were not found!")
        what <- what[grep("spectra", what, invert = TRUE)]
      }
    }
    ms_spectra <- lapply(spectra.idx, function(idx){
      read_shimadzu_spectrum(x, idx = idx, sep = sep)
    })
  }

  xx <- mget(what)
  if (collapse) xx <- collapse_list(xx)
  xx
}

#' @noRd
collapse_list <- function(x){
  while(is.list(x) && length(x) == 1){
    x <- x[[1]]
  }
  x
}

#' Read Shimadzu Metadata
#' @noRd
read_shimadzu_metadata <- function(x, met = NULL, sep){

  headings <- grep("\\[*\\]", x)
  names(headings) <- x[headings]

  idx <- which(x[headings] %in% c("[Header]", "[File Information]",
                                  "[Sample Information]", "[Original Files]",
                                  "[File Description]", "[Configuration]") )
  meta_start <- headings[min(idx)]
  meta_end <- headings[max(idx) + 1]
  meta <- x[(meta_start + 1):(meta_end - 1)]
  meta <- meta[meta != ""]
  meta <- meta[-grep("\\[", meta)]
  meta <- stringr::str_split_fixed(meta, pattern = sep, n = 2)
  if (!is.null("met")){
    meta <- rbind(meta, met)
  }
  rownames(meta) <- meta[, 1]
  meta <- as.list(meta[,2])
  meta
}

#' Read Shimadzu Chromatogram
#' @noRd
read_shimadzu_chromatogram <- function(file, x, chrom.idx, sep, data_format,
                                       read_metadata, format_out){
  header <- try(extract_shimadzu_header(x = x, chrom.idx = chrom.idx, sep = sep))
  met <- header[[1]]
  first_time <- strsplit(x[header[[2]]+2], "\t")[[1]][1]
  decimal_separator <- ifelse(grepl(",", first_time), ",", ".")

  if (decimal_separator == ","){
    times.idx <- grep("Time|Intensity Multiplier", met[,1])
    met[times.idx, 2] <- gsub(",", ".", met[times.idx, 2])
  }

  xx <- read.csv(file, skip = header[[2]], sep = sep, colClasses = "numeric",
                 # na.strings = c("[FractionCollectionReport]", "#ofFractions", "\\["),
                 dec = decimal_separator,
                 nrows = as.numeric(met[grep("# of Points", met),2]))
  xx <- as.matrix(xx[!is.na(xx[,1]),])
  if (data_format == "wide"){
    rownames(xx) <- xx[, 1]
    xx <- xx[, 2, drop = FALSE]
    colnames(xx) <- "Intensity"
  }
  # if (data_format == "long"){
  #   xx <- cbind(RT = as.numeric(rownames(xx)), Intensity = as.numeric(xx[,1]))
  # }
  if (format_out == "data.frame"){
    xx <- as.data.frame(xx)
  }
  if (read_metadata){
    meta <- read_shimadzu_metadata(x, met = met, sep = sep)
    xx <- attach_metadata(xx, meta, format_in = "shimadzu_chrom",
                          source_file = file, format_out = format_out,
                          data_format = data_format,
                          parser = "chromConverter")
  }
  xx
}

#' Read Shimadzu DAD Array
#' @noRd
read_shimadzu_dad <- function(file, x, chrom.idx, sep, data_format,
                              read_metadata, format_out){
  header <- try(extract_shimadzu_header(x = x, chrom.idx = chrom.idx, sep = sep))
  met <- header[[1]]
  first_time <- strsplit(x[header[[2]]+3], "\t")[[1]][1]
  decimal_separator <- ifelse(grepl(",", first_time), ",", ".")

  if (decimal_separator == ","){
    times.idx <- grep("Time|Intensity Multiplier", met[,1])
    met[times.idx, 2] <- gsub(",", ".", met[times.idx, 2])
  }

  nrows <- as.numeric(met[grep("# of Time Axis Points", met[,1]), 2])
  ncols <- as.numeric(met[grep("# of Wavelength Axis Points", met[,1]), 2])
  xx <- read.csv(file, skip = header[[2]]+1, sep = sep, colClasses = "numeric",
                 na.strings = c("[FractionCollectionReport]", "#ofFractions"),
                 row.names = 1, nrows = nrows, dec = decimal_separator)
  xx <- as.matrix(xx[!is.na(xx[,1]),])
  colnames(xx) <- as.numeric(gsub("X", "", colnames(xx)))*0.01
  if (data_format == "long"){
    xx <- reshape_chrom(xx, data_format = "long")
  }
  if (format_out == "data.frame"){
    xx <- as.data.frame(xx)
  }
  if (read_metadata){
    meta <- read_shimadzu_metadata(x, met = met, sep = sep)
    xx <- attach_metadata(xx, meta, format_in = "shimadzu_chrom",
                          source_file = file, format_out = format_out,
                          data_format = data_format,
                          parser = "chromConverter")
  }
  xx
}

#' Read Shimadzu Peak Table
#' @noRd
read_shimadzu_peaktable <- function(file, x, idx, sep, format_in, format_out){
  nrows <- as.numeric(strsplit(x = x[idx + 1], split = sep)[[1]][2])
  table_start <- grep("Peak#", x[idx:(idx + nrows)]) + idx - 1
  if (!is.na(nrows) && nrows > 0){
    time_column <- grep("R.Time|Ret.Time", strsplit(x = x[[table_start]], split = sep)[[1]])
    t1 <- strsplit(x = x[[table_start + 3]], split = sep)[[1]][time_column]
    decimal_separator <- ifelse(grepl(",", t1), ",", ".")

    peak_tab <- read.csv(file, skip = table_start-1, sep = sep, nrows = nrows,
                         dec = decimal_separator)
    if (format_out == "chromatographr"){
      column_names <- switch(format_in, "MC" = c("Ret.Time", "Proc.From", "Proc.To", "Area", "Height"),
                             c("R.Time", "I.Time", "F.Time", "Area", "Height"))
      peak_tab <- peak_tab[, column_names]
      colnames(peak_tab) <- c("rt", "start", "end", "area", "height")
      # cbind(sample = gsub("\\[|\\]","", x[idx]), peak_tab)
    }
    peak_tab
  } else {
    NA
  }
}

#' Read Shimadzu MS Spectrum
#' @noRd
read_shimadzu_spectrum <- function(x, idx, sep){
  nrows <- as.numeric(strsplit(x = x[idx + 1], split = sep)[[1]][2])
  table_start <- grep("Intensity", x[idx:(idx + nrows)]) + idx
  decimal_separator <- ifelse(grepl(".", strsplit(x[table_start + 4], split = sep)[[1]][1]), ".", ",")

  spectrum <- read.csv(file, skip = table_start-1, sep = sep, nrows = nrows,
           dec = decimal_separator)
  spectrum
}

#' Extract Header from Shimadzu ASCII Files
#' @noRd
extract_shimadzu_header <- function(x, chrom.idx, sep){
  index <- chrom.idx + 1
  line <- x[index]
  l <- length(strsplit(x = line, split = sep)[[1]])
  header <- strsplit(x = line, split = sep)[[1]]
  while (l > 1) {
    index <- index + 1
    line <- strsplit(x = x[index], split = sep)[[1]]
    l <- length(line)
    if (l == 1 | suppressWarnings(!is.na(as.numeric(line[1]))) | grepl("R.Time|Ret.Time", line[1]))
      break
    header <- rbind(header, line)
  }
  list(header, index-1)
}
