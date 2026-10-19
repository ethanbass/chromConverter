#' Read 'Agilent ChemStation' report files
#'
#' Reads peak tables from 'Agilent ChemStation' `Report.TXT` files, or from the
#' `Result.xml` files 'ChemStation' writes when set up to export its results.
#' Only `Result.xml` records where each peak starts and ends, so it is read in
#' preference to a `Report.TXT` in the same directory.
#'
#' @inheritParams shared_params
#' @param paths Paths to 'ChemStation' `Report.TXT` or `Result.xml` files.
#' @param metadata_format Format to output metadata. Either `chromconverter` or
#' `raw`.
#' @param data_format Deprecated. Use `peaktable_format` instead.
#' @return A `peak_list`: a list with one element per report, named by its
#' `.D` directory, each holding one peak table per signal, named by
#' wavelength.
#' @examples
#' path <- system.file("extdata", "benzoxazinoid_standards", "BENZOS_250PPM.D",
#'                     "Report.TXT", package = "chromConverter")
#' read_chemstation_reports(path)
#' @author Ethan Bass
#' @family 'Agilent' parsers
#' @export

read_chemstation_reports <- function(paths,
                                     peaktable_format = c("chromatographr",
                                                          "original"),
                                     metadata_format = c("chromconverter", "raw"),
                                     data_format = NULL){
  if (!is.null(data_format)){
    warn_renamed_arg("data_format", "peaktable_format")
    peaktable_format <- data_format
  }
  peaktable_format <- match.arg(tolower(peaktable_format),
                                c("chromatographr", "original"))
  metadata_format = match.arg(metadata_format, c("chromconverter", "raw"))
  xml <- grepl("\\.xml$", paths, ignore.case = TRUE)
  paths <- paths[xml | !dirname(paths) %in% dirname(paths)[xml]]
  names(paths) <- sub(".*/([^/]+)\\.D/.*$", "\\1", paths)

  pks <- lapply(seq_along(paths), function(i){
    read <- if (grepl("\\.xml$", paths[[i]], ignore.case = TRUE)){
      read_chemstation_xml
    } else read_chemstation_report
    xx <- read(paths[[i]], peaktable_format = peaktable_format,
               metadata_format = metadata_format)
    dat <- lapply(seq_along(xx), function(ii){
      lambda <- suppressWarnings(as.numeric(sub(".*Sig=([0-9]+).*", "\\1",
                                                names(xx)[ii])))
      transfer_metadata(cbind(sample = names(paths)[i], lambda = lambda,
                              xx[[ii]]), xx[[ii]])
    })
    names(dat) <- sub(".*Sig=([0-9]+).*", "\\1", names(xx))
    # `read_chemstation_report` attaches the sample's metadata to the list it
    # returns, so rebuilding that list here would otherwise discard it
    transfer_metadata(dat, xx)
  })
  names(pks) <- names(paths)
  structure(pks,
            chrom_list = NA,
            lambdas = names(pks[[1]]), fit = "chemstation", sd.max = NA,
            max.iter = NA,
            time.units = "min",
            class = "peak_list")
}

#' Read 'Agilent ChemStation' report
#' @param path Path to file
#' @param peaktable_format Whether to return peak tables in `chromatographr`
#' or `original` format.
#' @param combine Whether to combine peaklists into a single data.frame.
#' @param metadata_format Format to output metadata. Either `chromconverter` or
#' `raw`.
#' @author Ethan Bass
#' @family {Agilent parsers}
#' @noRd

read_chemstation_report <- function(path,
                                    peaktable_format = c("chromatographr",
                                                         "original"),
                                    combine = FALSE,
                                    read_metadata = TRUE,
                                    metadata_format = c("chromconverter", "raw")){
  peaktable_format <- match.arg(tolower(peaktable_format),
                                c("chromatographr", "original"))
  metadata_format <- check_metadata_format(metadata_format, "chemstation_peaklist")
  x <- read_chemstation_text(path)

  sections <- grep("=====================================================================", x)

  signals <- grep("Signal [0-9]:", x)
  signals <- c(signals, (grep("End of Report", x)-2))

  peak_lists <- lapply(seq_along(signals[-length(signals)]),function(i){
    table <- x[signals[i]:(signals[i+1] - 1)]
    convert_chemstation_peaklist(table, peaktable_format = peaktable_format)
  })
  names(peak_lists) <- x[signals[-length(signals)]]
  if (read_metadata){
    metadata <- x[(sections[1]+1):(sections[2]-1)]
    metadata <- remove_blank_lines(metadata)
    metadata <- gsub("^\\s+","", metadata)
    metadata <- gsub("\\s+\\:\\s+", " : ", metadata)
    metadata <- vapply(split(metadata, cumsum(grepl(":", metadata))), paste,
                       character(1), collapse = "", USE.NAMES = FALSE)
    metadata <- unlist(strsplit(metadata, "(?<!\\s:\\s)\\s{2,}(?!\\s)",
                                perl = TRUE))

    sample_info <- trimws(x[seq_len(sections[1] - 1)])
    sample_info <- sub("^Data File ", "Data File: ", sample_info)
    sample_info <- grep("^(Data File|Sample Name):", sample_info, value = TRUE)

    metadata <- c(sample_info, metadata)

    metadata <- strsplit(metadata, " ?: ")

    names(metadata) <- sapply(metadata, function(x)x[1])
    metadata <- lapply(metadata, function(x) x[2])
    peak_lists <- attach_metadata(peak_lists, metadata, format_in = metadata_format,
                                  source_file = path,
                                  data_format = peaktable_format,
                                  format_out = "data.frame")
    peak_lists[] <- lapply(peak_lists, function(tab){
      transfer_metadata(tab, peak_lists)
    })
    if (metadata_format != "raw"){
      peak_lists[] <- lapply(seq_along(peak_lists), function(i){
        chemstation_signal_attrs(peak_lists[[i]],
                                 sub("^Signal [0-9]+:\\s*", "",
                                     names(peak_lists)[i]))
      })
    }
  }
  peak_lists
}

#' Attach what a 'ChemStation' signal descriptor records to a peak table
#' @param desc The descriptor, e.g. `DAD1 A, Sig=254,8 Ref=off`.
#' @noRd
chemstation_signal_attrs <- function(tab, desc){
  channel <- chemstation_channel_id(desc)
  if (!is.null(channel)){
    attr(tab, "detector") <- chemstation_detector(channel)
    attr(tab, "channel_id") <- channel
  }
  attr(tab, "wavelength") <- chemstation_sig(desc, 1)
  attr(tab, "bandwidth") <- chemstation_sig(desc, 2)
  attr(tab, "signal_descriptor") <- desc
  tab
}

#' Read an 'Agilent ChemStation' `Result.xml` file
#'
#' `Result.xml` is the XML export of a run's results, which 'ChemStation'
#' writes when it is set up to. Unlike `Report.TXT`, it records where the
#' integration of each peak starts and ends (`TimeStart`, `TimeEnd`).
#' @param path Path to file.
#' @return A list with one peak table per signal, named by its descriptor.
#' @noRd
read_chemstation_xml <- function(path,
                                 peaktable_format = c("chromatographr",
                                                      "original"),
                                 read_metadata = TRUE,
                                 metadata_format = c("chromconverter", "raw")){
  peaktable_format <- match.arg(tolower(peaktable_format),
                                c("chromatographr", "original"))
  metadata_format <- check_metadata_format(metadata_format,
                                           "chemstation_peaklist")
  doc <- xml2::read_xml(path)
  signals <- xml2::xml_find_all(doc, ".//Chromatograms/Signal")
  descs <- trimws(xml2::xml_text(xml2::xml_find_first(signals,
                                                      "./Description")))
  peaks <- xml2::xml_find_all(doc, ".//Peak")
  peak_desc <- trimws(xml2::xml_text(xml2::xml_find_first(peaks,
                                                          "./SignalDesc")))
  peak_rt <- as.numeric(xml2::xml_text(xml2::xml_find_first(peaks,
                                                            "./MeasRetTime")))
  peak_type <- trimws(xml2::xml_text(xml2::xml_find_first(peaks,
                                                          "./PeakType")))
  peak_lists <- lapply(seq_along(signals), function(i){
    res <- xml2::xml_find_all(signals[[i]], "./IntegrationResults")
    fields <- unique(xml2::xml_name(xml2::xml_children(res)))
    tab <- lapply(fields, function(field){
      as.numeric(xml2::xml_text(xml2::xml_find_first(res,
                                                     paste0("./", field))))
    })
    names(tab) <- fields
    tab <- as.data.frame(tab)
    own <- peak_desc == descs[i]
    tab$PeakType <- peak_type[own][match(round(tab$RetTime, 4),
                                         round(peak_rt[own], 4))]
    if (peaktable_format == "chromatographr"){
      tab <- chromatographr_peak_table(tab, list(
        rt = "RetTime", start = "TimeStart", end = "TimeEnd", area = "Area",
        height = "Height", FWHM = "Width"))
    }
    tab
  })
  names(peak_lists) <- sub(",\\s*$", "", descs)
  if (read_metadata){
    fields <- xml2::xml_children(xml2::xml_find_first(doc,
                                                      ".//SampleInformation"))
    meta <- as.list(trimws(xml2::xml_text(fields)))
    names(meta) <- xml2::xml_name(fields)
    meta <- c(meta, list("Acq. Instrument" = meta$AcqInstName,
                         "Method" = meta$Method,
                         "Acq. Operator" = meta$Operator,
                         "Injection Date" = meta$InjectionDateTime,
                         "Sample Name" = meta$SampleName,
                         "Inj Volume" = meta$InjVolume,
                         "Actual Inj Volume" = meta$ActInjVolume))
    peak_lists <- attach_metadata(peak_lists, meta, format_in = metadata_format,
                                  source_file = path,
                                  data_format = peaktable_format,
                                  format_out = "data.frame")
    peak_lists[] <- lapply(peak_lists, function(tab){
      transfer_metadata(tab, peak_lists)
    })
    if (metadata_format != "raw"){
      peak_lists[] <- lapply(seq_along(peak_lists), function(i){
        chemstation_signal_attrs(peak_lists[[i]], names(peak_lists)[i])
      })
    }
  }
  peak_lists
}

#' Convert 'ChemStation' REPORT peak list to data.frame
#' @param table The table to convert.
#' @param peaktable_format Whether to return the peak table in
#' `chromatographr` or `original` format.
#' @author Ethan Bass
#' @noRd
convert_chemstation_peaklist <- function(table, peaktable_format =
                                           c("chromatographr", "original")){
  markdown_table <- table[-which(table == "")]
  split.pos <- c(1,gregexpr("\\|",markdown_table[4])[[1]])
  header1 <- sapply(seq_len(length(split.pos)-1), function(i){
    substr(markdown_table[2], split.pos[i], split.pos[i+1]-1)
  })
  header2 <- sapply(seq_len(length(split.pos)-1), function(i){
    substr(markdown_table[3], split.pos[i], split.pos[i+1]-1)
  })
  header <- paste(header1, header2)
  header <- gsub("\\s+$|^\\s+", "", header)
  header <- gsub("\\s+", " ", header)
  rows <- lapply(markdown_table[5:(length(markdown_table)-1)], function(line){
    gsub("\\s+", "", sapply(seq_len(length(split.pos)-1), function(i){
      substr(line, split.pos[i], split.pos[i+1]-1)
    }))
  })

  # Create the dataframe
  df <- data.frame(matrix(unlist(rows), nrow = length(rows), byrow = TRUE),
                   stringsAsFactors = FALSE)
  colnames(df) <- header
  df2 <- as.data.frame(purrr::map_df(df[,-c(which(colnames(df) == "Type"))],
                                     as.numeric))
  df2 <- cbind(df2, df[, "Type", drop = FALSE])
  if (peaktable_format == "chromatographr"){
    find <- function(prefix) grep(prefix, names(df2), value = TRUE)[1]
    df2 <- chromatographr_peak_table(df2, list(rt = find("^RetTime"),
                                               area = find("^Area \\["),
                                               height = find("^Height"),
                                               FWHM = find("^Width")))
  }
  df2
}

#' Remove blank lines
#' This function is called internally by `read_chemstation_reports`.
#' @noRd
remove_blank_lines <- function(x){
  x[which(x != "")]
}
