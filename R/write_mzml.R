#' Write mzML
#'
#' Writes spectra and chromatograms to an mzML file.
#'
#' Mass spectra and DAD spectra are written to the `spectrumList`, while the
#' total ion current (`TIC`) and the base peak chromatogram (`BPC`) go to the
#' `chromatogramList`, since the controlled vocabulary has terms for those two
#' summaries. DAD spectra are written as electromagnetic radiation spectra
#' (`MS:1000804`) using Thermo's naming convention, with `controllerType=4` in
#' the spectrum ID for compatibility with existing tools.
#'
#' Asking for both `MS1` and `MS2` writes them into one `spectrumList`,
#' interleaved in acquisition order: on the `scan` column they share, or on
#' retention time where neither has one. Each spectrum is then named for its
#' scan (`scan=417`) rather than for its position in the list, which keeps the
#' names unique across the levels. An MS2 spectrum carries the
#' precursor it came from as `MS:1000744` ("selected ion m/z"), and a
#' `spectrumRef` to the MS1 spectrum that precedes it. Collision energy,
#' isolation window and precursor charge are not written, as no parser in the
#' package reads them.
#'
#' Retention times are written in minutes (`UO:0000031`), as chromConverter
#' reports them, rather than converted to seconds as [write_andi_ms] does.
#'
#' The streams to write come from the names of `data`, so a bare chromatogram
#' has to say what it holds through its `detector` attribute: `UV` and `DAD`
#' are written as a DAD stream, and `MS` as `MS1`, or as `MS2` where the table
#' also carries an `ms_level` attribute above 1. Any other `detector`,
#' including a missing or `NA` one (which is how several parsers report an
#' unknown detector), is an error: the function stops rather than guess, and
#' asks for a named list instead.
#'
#' A one-dimensional DAD stream (a single wavelength) is refused: mzML has no
#' axis to write it along, so it would become one single-point spectrum per
#' retention time. Use
#' [write_andi_chrom] for a single trace. If it is the only stream requested
#' this is an error; otherwise it is dropped with a warning and the rest is
#' written.
#'
#' The file's metadata are taken from the `MS1` stream if it is written, and
#' otherwise from the first stream requested. That stream's `sample_name`
#' attribute names the file unless `sample_name` is supplied.
#'
#' If `indexed = TRUE`, the function will generate an indexed mzML file, which
#' allows faster random access to spectra. The file is assembled by writing XML
#' strings straight to a connection rather than by building a document in
#' memory.
#'
#' @importFrom utils packageVersion
#' @param data A named list of `data.frame`s or `data.table`s, keyed by stream
#' (`MS1`, `MS2`, `TIC`, `BPC`, `DAD`), or a single chromatogram carrying a
#' `detector` attribute that says which stream it is.
#' @param path_out The path to write the file.
#' @param sample_name The name of the file. If a name is not provided, the name
#' will be derived from the `sample_name` attribute, and it is an error if
#' there is no such attribute.
#' @param what Which streams to write to mzML: `"MS1"`, `"MS2"`, `"TIC"`,
#' `"BPC"`, and/or `"DAD"`. Defaults to every element of `data` that holds any
#' rows.
#' @param instrument_info Controlled-vocabulary terms describing the
#' instrument, as a list of lists with elements `cvRef`, `accession`, `name`
#' and `value`, each written as one `cvParam` of the
#' `instrumentConfiguration`. Defaults to `NULL`, in which case `MS:1000031`
#' ("instrument model") is written with the chromatogram's `detector_model` or
#' `instrument` as its value, or bare where it records neither.
#' @param centroided Logical. Whether the spectra are centroided, written as
#' `MS:1000127` or, when `FALSE`, `MS:1000128` ("profile spectrum"). Defaults
#' to `TRUE`. Set it to `FALSE` for the profile scan types of a triple
#' quadrupole (a full scan or a product-ion scan, as opposed to SIM or MRM).
#' @param compress Logical. Whether to use zlib compression. Defaults to `TRUE`.
#' @param indexed Logical. Whether to write indexed mzML. Defaults to `TRUE`.
#' @param force Logical. Whether to overwrite existing files at `path_out`.
#' Defaults to `FALSE`.
#' @param show_progress Logical. Whether to show progress bar. Defaults to `TRUE`.
#' @param verbose Logical. Whether or not to print status messages.
#' @return Invisibly returns the path to the written mzML file.
#' @examples \dontrun{
#' chrom <- read_chroms("path/to/file.qgd", progress_bar = FALSE)
#' write_mzml(chrom[[1]], path_out = "path/to/directory")
#' }
#' @author Ethan Bass
#' @family write functions
#' @export

write_mzml <- function(data, path_out, sample_name = NULL, what = NULL,
                      instrument_info = NULL, centroided = TRUE,
                      compress = TRUE, indexed = TRUE,
                      force = FALSE, show_progress = TRUE,
                       verbose = getOption("verbose")) {
  if (!inherits(data, "list")){
    # A bare chromatogram has to say which kind of data it holds, since the
    # mzML spectra are named for it. A list is already keyed by stream, so it
    # skips this. Not every parser records a detector: 'Shimadzu' ASCII files
    # give a `detector_model` but no `detector`, and 'ChemStation' `.ch` files
    # report `NA`, so guessing here would silently mislabel the data.
    streams <- c(UV = "DAD", MS = "MS1", DAD = "DAD")
    detector <- attr(data, "detector")
    # `NA` is how a parser says it looked and found nothing, so report it the
    # same way as an absent attribute rather than as a detector called "NA"
    unrecorded <- length(detector) == 0 ||
      (length(detector) == 1 && is.na(detector))
    if (unrecorded || !all(detector %in% names(streams)) ||
        length(detector) != 1){
      stop(sprintf(paste0("Could not determine what kind of data this is: ",
                          "the `detector` attribute is %s.\nSet it to one of ",
                          "%s, or supply a named list instead, ",
                          "e.g. `list(DAD = x)`."),
                   if (unrecorded) "missing" else
                     paste(sQuote(detector), collapse = ", "),
                   paste(sQuote(names(streams)), collapse = ", ")),
           call. = FALSE)
    }
    detector <- streams[[detector]]
    # a file holding one level and nothing else comes back as a bare table, so
    # the detector alone would send an MRM or product-ion run to MS1
    if (identical(detector, "MS1") && isTRUE(attr(data, "ms_level") > 1)){
      detector <- "MS2"
    }
    data <- setNames(list(data), detector)
    what <- detector
  }
  names(data) <- toupper(names(data))
  populated <- names(data)[vapply(data, NROW, integer(1)) > 0]
  if (is.null(what)){
    what <- populated
  }
  what <- match.arg(toupper(what), c("MS1", "MS2", "TIC", "BPC", "DAD"),
                    several.ok = TRUE)
  # mzML stores scans of (m/z or wavelength, intensity), so a single trace has
  # no axis to put in one: written as spectra it becomes one single-point scan
  # per retention time. `TIC` and `BPC` are the exception, since the CV has
  # terms for those MS-derived summaries and they go to `chromatogramList`.
  # A UV or FID trace belongs in ANDI chrom instead. Checked here rather than
  # in the branch above, so that a named list cannot slip past it.
  if ("DAD" %in% what && is_unidimensional(data$DAD)){
    why <- paste0("mzML has no place for a one-dimensional chromatogram: it ",
                  "would be written as one single-point spectrum per ",
                  "retention time.\nUse `write_andi_chrom()` for it, or ",
                  "supply the full two-dimensional DAD data to write spectra.")
    if (length(setdiff(what, "DAD")) == 0){
      # dropping it would leave an empty file, so say so instead
      stop(why, call. = FALSE)
    }
    warning("Skipping the DAD data. ", why, call. = FALSE, immediate. = TRUE)
    what <- setdiff(what, "DAD")
  }
  # A stream that was asked for but is empty or absent is dropped here, rather
  # than at the branch that would have written it: the header has to describe
  # the streams that actually follow it, and one of them has to supply the
  # metadata --- `MS1` where there is one, since it usually carries the most.
  avail <- intersect(what, populated)
  if (length(avail) == 0){
    stop("None of the requested data is present in `data`.", call. = FALSE)
  }
  for (i in setdiff(what, avail)) warning(sprintf("%s data not found.", i))
  what <- avail
  meta <- attributes(data[[if ("MS1" %in% what) "MS1" else what[1]]])
  if (is.null(sample_name)){
    sample_name <- meta$sample_name
    # a zero-length `sample_name` would collapse the `sprintf` calls in the
    # header to `character(0)`, dropping whole elements from the file
    if (length(sample_name) != 1 || is.na(sample_name)){
      stop("Could not find a `sample_name` in the data. Please supply one ",
           "with the `sample_name` argument.", call. = FALSE)
    }
  }
  file_out <- get_filepath(path_out = path_out, sample_name = sample_name,
                           force = force, ext = "mzML")

  # More than one MS level goes into one `spectrumList`, in acquisition order,
  # so the levels are merged before anything is written: the header has to
  # count the spectra that follow it. A single level keeps the per-stream path,
  # which pads the spectra out to the TIC for the parsers that need it.
  ms_what <- intersect(what, c("MS1", "MS2"))
  roster <- if ("MS2" %in% ms_what) ms_scan_roster(data, ms_what) else NULL

  w <- new_mzml_writer(file_out)
  on.exit(close(w$con))

  counted <- intersect(what, if (is.null(roster)) c("MS1", "DAD") else "DAD")
  n_scan <- sum(vapply(counted, function(i){
    tryCatch(n_spectra(data, i), error = function(cond) NA_real_)
  }, numeric(1)), na.rm = TRUE)
  if (!is.null(roster)) n_scan <- n_scan + nrow(roster$info)
  write_mzml_header(w, meta = meta, n_scan = n_scan, what = what,
                    indexed = indexed, instrument_info = instrument_info,
                    sample_name = sample_name)
  spectrum_indices <- c()
  if (!is.null(roster)){
    spectrum_indices <- write_ms_spectra(w, roster = roster, indexed = indexed,
                                         idx_start = 0, compress = compress,
                                         centroided = centroided,
                                         show_progress = show_progress,
                                         verbose = verbose)
  } else if (length(ms_what) == 1){
    spectrum_indices <- write_spectra(w, data = data, what = ms_what,
                                      indexed = indexed, idx_start = 0,
                                      compress = compress,
                                      centroided = centroided,
                                      show_progress = show_progress,
                                      verbose = verbose)
  }
  if (any(what == "DAD")){
    # the DAD spectra carry on from the MS1 spectra. `spectrum_indices` has
    # one element per spectrum written, whether or not it holds an offset,
    # so the count is right even when `indexed` is `FALSE`.
    start <- length(spectrum_indices)
    DAD <- write_spectra(w, data, what = "DAD", indexed = indexed,
                         idx_start = start, compress = compress,
                         centroided = centroided,
                         show_progress = show_progress, verbose = verbose)
    spectrum_indices <- c(spectrum_indices, DAD)
  }

  mz_write(w, '  </spectrumList>\n') # close spectrumList
  if (any(what %in% c("TIC","BPC"))){
    chrom_indices <- write_mzml_chromlist(w, data,
                                         what = what[what %in% c("TIC", "BPC")],
                                         indexed = indexed, compress = compress,
                                         verbose = verbose)
  } else chrom_indices <- NULL
  mz_write(w, '   </run>\n  </mzML>\n')
  if (indexed){
    index_count <- 0
    if (length(spectrum_indices) > 0) index_count <- index_count + 1
    if (length(chrom_indices) > 0) index_count <- index_count + 1

    indexListOffset <- w$pos

    mz_write(w, sprintf('<indexList count="%d">\n', index_count))

    if (length(spectrum_indices) > 0) {
      mz_write(w, '\t<index name="spectrum">\n')
      for (entry in spectrum_indices) {
        mz_write(w, sprintf('\t\t<offset idRef="%s">%d</offset>\n',
                            entry$id, entry$offset))
      }
      mz_write(w, '\t</index>\n')
    }

    if (length(chrom_indices) > 0) {
      mz_write(w, '\t<index name="chromatogram">\n')
      for (entry in chrom_indices) {
        mz_write(w, sprintf('\t\t<offset idRef="%s">%d</offset>\n',
                            entry$id, entry$offset))
      }
      mz_write(w, '\t</index>\n')
    }
    mz_write(w, '</indexList>\n')

    # The checksum covers the file through the opening `fileChecksum` tag, so
    # it has to be written in two parts, with the file flushed in between.
    mz_write(w, sprintf('<indexListOffset>%d</indexListOffset>\n',
                        indexListOffset))
    mz_write(w, '<fileChecksum>')
    flush(w$con)
    checksum <- digest::digest(file = file_out, algo = "sha1")
    mz_write(w, checksum, '</fileChecksum>\n</indexedmzML>\n')
}
  return(invisible(file_out))
}

#' Open an mzML file for writing
#'
#' Returns an environment holding the connection and the number of bytes
#' written so far. The byte count is what the `indexList` offsets are built
#' from: `seek()` is documented as unreliable on a connection opened in text
#' mode, and it does not account for the connection's write buffer, so the
#' position is tracked explicitly instead. The connection is opened in binary
#' mode so that a byte written is a byte on disk (no CRLF translation on
#' Windows), which is what makes the offsets portable.
#' @noRd
new_mzml_writer <- function(file_out){
  w <- new.env(parent = emptyenv())
  w$con <- file(file_out, "wb")
  w$pos <- 0
  w
}

#' Write to an mzML file and advance the byte counter
#'
#' Arguments are pasted together and written as UTF-8. The counter is advanced
#' by the number of *bytes*, not characters, so that non-ASCII metadata (sample
#' names, Windows method paths) cannot desynchronize the index.
#' @noRd
mz_write <- function(w, ...){
  bytes <- charToRaw(enc2utf8(paste0(...)))
  writeBin(bytes, w$con)
  w$pos <- w$pos + length(bytes)
  invisible(w$pos)
}

#' Count the scans in a chromatogram
#'
#' Wide data carries one scan per row; long data carries one scan per unique
#' retention time.
#' @noRd
count_scans <- function(x){
  if (identical(attr(x, "data_format"), "wide")){
    nrow(x)
  } else {
    length(unique(get_column(x, "rt")))
  }
}


#' Count the spectra that will be written for a stream
#'
#' `write_spectra` pads the MS1 spectra out to the TIC when the TIC leads (see
#' the acquisition delay there), so the header count has to be taken from the
#' TIC as well or it will not match the spectra that follow it.
#' @noRd
n_spectra <- function(data, what){
  if (what == "MS1" && !is.null(data$TIC)){
    count_scans(data$TIC)
  } else {
    count_scans(data[[what]])
  }
}


#' Write mzML header
#' @param w mzML writer (see `new_mzml_writer`).
#' @param n_scan Number of scans to be included in mzML file.
#' @param indexed Logical. Whether mzML file is to be indexed.
#' @author Ethan Bass
#' @noRd
write_mzml_header <- function(w, meta, n_scan, what = "MS1", indexed = TRUE,
                              instrument_info = NULL, sample_name){
  # Write XML declaration and opening tags
  mz_write(w,
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    ifelse(indexed, '<indexedmzML xmlns="http://psi.hupo.org/ms/mzml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://psi.hupo.org/ms/mzml http://psi.hupo.org/ms/mzml">\n', ''),
    sprintf('<mzML xmlns="http://psi.hupo.org/ms/mzml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://psi.hupo.org/ms/mzml http://psi.hupo.org/ms/mzml" id="%s" version="1.1.0">\n',
            mzml_escape(sample_name)),
    '<cvList count="2">
          <cv id="MS" fullName="Proteomics Standards Initiative Mass Spectrometry Ontology" version="4.1.0" URI="https://raw.githubusercontent.com/HUPO-PSI/psi-ms-CV/master/psi-ms.obo"/>
          <cv id="UO" fullName="Unit Ontology" version="releases/2020-03-10" URI="http://data.bioontology.org/ontologies/UO"/>
      </cvList>\n',
  create_mzml_file_description(meta, what = what),
  create_mzml_sample_list(meta, sample_name = sample_name),
  create_mzml_software_list(meta),
  '<instrumentConfigurationList count="1">
    <instrumentConfiguration id="IC">\n')


  if (!is.null(instrument_info)) {
    for (param in instrument_info) {
      mz_write(w, sprintf('      <cvParam cvRef="%s" accession="%s" name="%s" value="%s"/>\n',
                  param$cvRef, param$accession, param$name, param$value))
    }
  } else {
    mz_write(w, mzml_instrument_param(meta))
  }
  run_datetime <- meta$run_datetime[1]
  date_time <- if (inherits(run_datetime, c("POSIXct", "POSIXlt", "Date"))){
    tryCatch(format(run_datetime, "%Y-%m-%dT%H:%M:%SZ"),
             error = function(err) NA)
  } else NA
  timestamp_attr <- if(is.na(date_time)) "" else sprintf(' startTimeStamp="%s"', date_time)
  mz_write(w, sprintf('    </instrumentConfiguration>
  </instrumentConfigurationList>
  <dataProcessingList count="1">
    <dataProcessing id="chromConverter_processing">
      <processingMethod order="0" softwareRef="chromConverter">
        <cvParam cvRef="MS" accession="MS:1000544" name="Conversion to mzML"/>
      </processingMethod>
    </dataProcessing>
  </dataProcessingList>
  <run id="run1" defaultInstrumentConfigurationRef="IC"%s>
    <spectrumList count="%d" defaultDataProcessingRef="%s">\n',
              timestamp_attr,
              n_scan,
              "chromConverter_processing"))
}

#' Create mzml sample list
#' @noRd
create_mzml_sample_list <- function(meta, sample_name = NULL){
  # `id` is an `xs:ID`, so an absent `sample_id` cannot be pasted in as the
  # string "NA" and a real one has to lose any character the type disallows
  id <- gsub("[^A-Za-z0-9_.-]", "_", mzml_value(meta[["sample_id"]]) %||% "1")
  # the name `write_mzml` resolved, which is the `sample_name` attribute unless
  # the caller supplied one of their own
  name <- mzml_value(sample_name) %||% mzml_value(meta[["sample_name"]]) %||% ""
  sprintf(
  '<sampleList count="1">
    <sample id="%s" name="%s">
    </sample>
  </sampleList>
          ', paste0("s", id), mzml_escape(name))
}

#' Create mzml file description
#' @noRd
create_mzml_file_description <- function(meta, what = "MS1"){
  # a missing field would make `sprintf` return `character(0)`, which `paste0`
  # in `mz_write` then drops, taking the whole element out of the file
  source_file <- meta$source_file %||% NA
  source_sha1 <- meta$source_sha1 %||% NA
  content <- '<cvParam cvRef="MS" accession="MS:1000294" name="mass spectrum"/>'
  if ("MS1" %in% what){
    content <- c(content,
      '<cvParam cvRef="MS" accession="MS:1000579" name="MS1 spectrum"/>')
  }
  if ("MS2" %in% what){
    content <- c(content,
      '<cvParam cvRef="MS" accession="MS:1000580" name="MSn spectrum"/>')
  }
  sprintf(
  '  <fileDescription>
        <fileContent>
          %s
        </fileContent>
        <sourceFileList count="1">
          <sourceFile id="SF1" name="%s" location="%s">
            %s
            <cvParam cvRef="MS" accession="MS:1000569" name="SHA-1" value="%s"/>
            <cvParam cvRef="MS" accession="MS:1000776" name="scan number only nativeID format"/>
          </sourceFile>
        </sourceFileList>%s
    </fileDescription>',
          paste(content, collapse = "\n          "),
          mzml_escape(ifelse(is.na(source_file), "", basename(source_file))),
          mzml_escape(ifelse(is.na(source_file), "", source_file)),
          mzml_source_format_param(meta[["source_file_format"]]),
          ifelse(is.na(source_sha1), "", source_sha1),
          mzml_contact(meta[["operator"]]))
}

#' A metadata value fit to write, or `NULL`
#' @noRd
mzml_value <- function(x){
  andi_ms_setting(x[1])
}

#' Escape a value for an XML attribute
#' @noRd
mzml_escape <- function(x){
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

#' Instrument model cvParam
#'
#' `MS:1000031` is written bare where the model is unknown, since the schema
#' asks the `instrumentConfiguration` for at least one `cvParam`.
#' @noRd
mzml_instrument_param <- function(meta){
  model <- mzml_value(meta[["detector_model"]]) %||%
    mzml_value(meta[["instrument"]])
  if (is.null(model)){
    '      <cvParam cvRef="MS" accession="MS:1000031" name="instrument model"/>\n'
  } else {
    sprintf(paste0('      <cvParam cvRef="MS" accession="MS:1000031" ',
                   'name="instrument model" value="%s"/>\n'),
            mzml_escape(model))
  }
}

#' File format cvParam for the source file
#'
#' Only a few of the formats chromConverter reads have a term of their own, so
#' anything else is described by the parent term rather than by a format it is
#' not. `andi_chrom` sits under `chromatograph file format` instead.
#' @noRd
mzml_source_format_param <- function(format){
  terms <- list(andi_ms = c("MS:1002441", "Andi-MS format"),
                andi_chrom = c("MS:1002443", "Andi-CHROM format"),
                thermoraw = c("MS:1000563", "Thermo RAW format"),
                waters_raw = c("MS:1000526", "Waters raw format"),
                mzml = c("MS:1000584", "mzML format"),
                mzxml = c("MS:1000566", "ISB mzXML format"))
  term <- terms[[tolower(mzml_value(format) %||% "")]] %||%
    c("MS:1000560", "mass spectrometer file format")
  sprintf('<cvParam cvRef="MS" accession="%s" name="%s"/>', term[1], term[2])
}

#' Contact block naming the operator
#' @noRd
mzml_contact <- function(operator){
  operator <- mzml_value(operator)
  if (is.null(operator)) return("")
  sprintf(paste0('\n        <contact>\n          <cvParam cvRef="MS" ',
                 'accession="MS:1000586" name="contact name" value="%s"/>',
                 '\n        </contact>'), mzml_escape(operator))
}

#' Create mzml software list
#' @noRd
create_mzml_software_list <- function(meta){
  entries <- sprintf(
  '      <software id="chromConverter" version="%s">
        <cvParam cvRef="MS" accession="MS:1000799" name="custom unreleased software tool" value="chromConverter R package"/>
      </software>', as.character(packageVersion("chromConverter")))
  acquisition <- mzml_value(meta[["software"]])
  if (!is.null(acquisition)){
    entries <- c(sprintf(
  '      <software id="acquisition" version="%s">
        <cvParam cvRef="MS" accession="MS:1001455" name="acquisition software" value="%s"/>
      </software>', mzml_escape(mzml_value(meta[["software_version"]]) %||% "unknown"),
      mzml_escape(acquisition)), entries)
  }
  sprintf('  <softwareList count="%d">\n%s\n    </softwareList>',
          length(entries), paste(entries, collapse = "\n"))
}


#' Write mzML spectra
#' @importFrom data.table .SD
#' @author Ethan Bass
#' @noRd
write_spectra <- function(w, data, what = c("MS1", "DAD"),
                          indexed = TRUE, idx_start = 0, compress = TRUE,
                          centroided = TRUE, show_progress = TRUE,
                          verbose = getOption("verbose")){
  what <- match.arg(toupper(what), c("MS1", "DAD"))

  if (verbose)
    message(sprintf("Writing %s spectra.", toupper(what)))

  laplee <- ifelse(show_progress, pbapply::pblapply, lapply)

  # read before `prepare_spectra`, which does not carry attributes over
  polarity <- stream_polarity(data[[toupper(what)]])
  spectra_data <- prepare_spectra(data[[toupper(what)]], what)

  create_spectrum <- switch(what,
                            "MS1" = function(...){
                              create_mzml_ms_spectrum(..., polarity = polarity)
                            },
                            "DAD" = create_mzml_dad_spectrum)

  scans <- group_scans(spectra_data)
  rts <- scans$keys
  get_scan <- scans$get_scan

  if (what == 'MS1'){
    if (!is.null(data$TIC) && attr(data$TIC, "data_format") == "wide"){
      data$TIC <- data.frame(rt = as.numeric(rownames(data$TIC)),
                             intensity = data$TIC[,"intensity"])
    }
    n_scan <- ifelse(!is.null(data$TIC), nrow(data$TIC), length(rts))
    extra_vals <- n_scan - length(rts)

    if (extra_vals > 0){
      # the acquisition delay at the head of the TIC has no spectra of its own,
      # so it is padded with empty scans
      rts <- c(get_column(data$TIC, "rt")[seq_len(extra_vals)], rts)
      empty_scan <- spectra_data[0]
      inner_scan <- get_scan
      get_scan <- function(i){
        if (i <= extra_vals) empty_scan else inner_scan(i - extra_vals)
      }
    }
  } else if (what == "DAD"){
    n_scan <- length(rts)
  }

  laplee(seq_len(n_scan), function(i){
    if (indexed){
      offset <- w$pos
    }

    scan_data <- get_scan(i)

    # Create and write spectrum
    spectrum_xml <- create_spectrum(scan_data = scan_data, scan = i,
                                    index = (i + idx_start - 1),
                                    rt = rts[i], compress = compress,
                                    centroided = centroided,
                                    tic = ifelse(!is.null(data$TIC),
                                                 data$TIC[[i, "intensity"]],
                                                 sum(scan_data$intensity)),
                                    bpc = ifelse(!is.null(data$BPC),
                                                 data$BPC[[i, "intensity"]],
                                                 ifelse(length(scan_data$intensity) == 0,
                                                        0, max(scan_data$intensity))))
    mz_write(w, spectrum_xml, "\n")
    if (indexed){
      prefix <- switch(what, "MS1" = "scan=",
                            "DAD" = "controllerType=4 controllerNumber=1 scan=")
      list(id = paste0(prefix, i), offset = offset)
    }
  })
}

#' Group a long-format table into scans
#'
#' Returns the retention time of each scan and an accessor for its rows.
#' Reshaping emits all of the rows for one retention time together, so the
#' scans are contiguous ranges and can be sliced directly; materializing them
#' with `split()` would copy the whole table into a list of per-scan tables.
#' Retention times that are *not* contiguous (the same time appearing in two
#' separate blocks) fall back to `split()`, which gathers them.
#'
#' The keys are taken from the same grouping as the rows, so scan `i` and
#' `keys[i]` cannot disagree.
#'
#' @param by Column to group on: `rt` for a stream of one level, `scan` where
#' MS1 and MS2 are interleaved and two levels can share a retention time.
#' @noRd
group_scans <- function(x, by = "rt"){
  key <- get_column(x, by)
  runs <- rle(key)
  if (length(runs$values) == length(unique(key))){
    ends <- cumsum(runs$lengths)
    starts <- ends - runs$lengths + 1L
    list(keys = runs$values,
         get_scan = function(i) x[starts[i]:ends[i]])
  } else {
    scan_list <- split(x, key)
    list(keys = as.numeric(names(scan_list)),
         get_scan = function(i) scan_list[[i]])
  }
}

#' Normalize a spectral table for writing
#' @param x A chromatogram.
#' @param what The stream `x` holds.
#' @return `x`, long, as a `data.table`.
#' @author Ethan Bass
#' @noRd
prepare_spectra <- function(x, what){
  if (identical(attr(x, "data_format"), "wide")){
    x <- reshape_chrom_long(x)
  }
  if (!inherits(x, "data.table")){
    x <- data.table::as.data.table(x)
    attr(x, "data_format") <- "long"
  }
  if (what != "DAD"){
    aliases <- c(fragmz = "mz", premz = "precursor_mz")
    hits <- intersect(names(aliases), names(x))
    hits <- hits[!aliases[hits] %in% names(x)]
    if (length(hits) > 0){
      # `setnames` renames in place, which would rename the caller's columns
      x <- data.table::copy(x)
      data.table::setnames(x, hits, unname(aliases[hits]))
    }
  }
  required <- c(if (what == "DAD") "lambda" else "mz", "intensity")
  absent <- setdiff(required, names(x))
  if (length(absent) > 0){
    stop(sprintf(paste0("The %s data has no %s column, so there is nothing to ",
                        "write as a spectrum.\nIt has %s."),
                 what, paste(sQuote(absent), collapse = " or "),
                 paste(sQuote(names(x)), collapse = ", ")), call. = FALSE)
  }
  x
}

#' Roster of the mass spectra to write
#' @param data Named list of chromatograms.
#' @param what Which of `MS1` and `MS2` to include.
#' @return `info`, one row per spectrum with `scan`, `rt`, `ms_level`,
#' `polarity` and `precursor_mz`, and `get_scan`, an accessor for its peaks.
#' @author Ethan Bass
#' @noRd
ms_scan_roster <- function(data, what){
  lvls <- intersect(c("MS1", "MS2"), what)
  parts <- lapply(lvls, function(lvl){
    # read before the coercion below, which does not carry attributes over
    info <- attr(data[[lvl]], "scan_info")
    default_polarity <- stream_polarity(data[[lvl]])
    x <- prepare_spectra(data[[lvl]], lvl)
    by <- if ("scan" %in% names(x)) "scan" else "rt"
    grp <- group_scans(x, by = by)
    lvl_n <- as.integer(sub("MS", "", lvl))
    if (!is.null(info) && nrow(info) > 0 && by == "scan"){
      info <- as.data.frame(info)
      info <- data.frame(key = info$scan, rt = info$rt, ms_level = lvl_n,
                         polarity = info$polarity %||% default_polarity,
                         precursor_mz = info$precursor_mz %||% NA_real_)
    } else {
      # `match` rather than the first row of each group: `group_scans` sorts
      # the keys when it has to fall back on `split`
      rt <- get_column(x, "rt")[match(grp$keys, get_column(x, by))]
      info <- data.frame(key = grp$keys, rt = rt, ms_level = lvl_n,
                         polarity = default_polarity,
                         precursor_mz = NA_real_)
    }
    info$j <- match(info$key, grp$keys)
    list(info = info, by = by, get_scan = grp$get_scan, empty = x[0])
  })
  names(parts) <- lvls

  info <- do.call(rbind, lapply(parts, function(p) p$info))
  info$part <- rep(seq_along(parts),
                   vapply(parts, function(p) nrow(p$info), integer(1)))
  # a spectrum is named for its scan, so the whole roster has to be on one
  # numbering. Where a table has no `scan` column the levels are ordered by
  # retention time and renumbered, since a time is not an mzML scan number.
  by <- vapply(parts, function(p) p$by, character(1))
  if (length(unique(by)) > 1){
    stop("Only some of the MS levels carry a `scan` column, so there is no ",
         "one axis to interleave them on.", call. = FALSE)
  }
  on_scan <- by[1] == "scan"
  info <- info[order(info$key, info$ms_level), , drop = FALSE]
  if (on_scan){
    if (anyDuplicated(info$key)){
      stop("The MS levels share scan numbers, so their spectra cannot be told ",
           "apart in the mzML index.", call. = FALSE)
    }
    info$scan <- as.integer(info$key)
  } else {
    info$scan <- seq_len(nrow(info))
  }
  row.names(info) <- NULL
  list(info = info,
       get_scan = function(i){
         p <- parts[[info$part[i]]]
         if (is.na(info$j[i])) p$empty else p$get_scan(info$j[i])
       })
}

#' Write the mass spectra of every level as one spectrumList
#' @author Ethan Bass
#' @noRd
write_ms_spectra <- function(w, roster, indexed = TRUE, idx_start = 0,
                             compress = TRUE, centroided = TRUE,
                             show_progress = TRUE,
                             verbose = getOption("verbose")){
  info <- roster$info
  if (verbose){
    message(sprintf("Writing %d mass spectra.", nrow(info)))
  }
  laplee <- ifelse(show_progress, pbapply::pblapply, lapply)

  ids <- sprintf("scan=%d", info$scan)
  parent <- rep(NA_character_, nrow(info))
  ms1 <- which(info$ms_level == 1)
  if (length(ms1) > 0){
    j <- findInterval(seq_len(nrow(info)), ms1)
    parent[j > 0] <- ids[ms1[j[j > 0]]]
  }
  parent[info$ms_level == 1] <- NA_character_

  laplee(seq_len(nrow(info)), function(i){
    if (indexed){
      offset <- w$pos
    }
    scan_data <- roster$get_scan(i)
    precursor <- info$precursor_mz[i]
    if (is.na(precursor) && !is.null(scan_data$precursor_mz)){
      # an MRM or SIM record carries a Q1 per transition rather than one for
      # the scan, so `scan_info` leaves it out. It still describes the spectrum
      # where every transition shares it.
      precursor <- unique(scan_data$precursor_mz)
      if (length(precursor) != 1) precursor <- NA_real_
    }
    spectrum_xml <- create_mzml_ms_spectrum(
      scan_data = scan_data, scan = info$scan[i], index = i + idx_start - 1,
      rt = info$rt[i], ms_level = info$ms_level[i], precursor_mz = precursor,
      parent_id = parent[i], polarity = info$polarity[i],
      centroided = centroided, compress = compress,
      tic = sum(scan_data$intensity),
      bpc = if (nrow(scan_data) == 0) 0 else max(scan_data$intensity))
    mz_write(w, spectrum_xml, "\n")
    if (indexed){
      list(id = ids[i], offset = offset)
    }
  })
}

#' The polarity of a whole stream, or `NA` where its scans disagree
#'
#' `write_spectra` writes one polarity for every spectrum it emits, so a run
#' that switched polarity mid-acquisition records none rather than the wrong
#' one. Only the roster, which `write_ms_spectra` uses, is per-scan.
#' @noRd
stream_polarity <- function(x){
  info <- attr(x, "scan_info", exact = TRUE)
  recorded <- unique(info$polarity[!is.na(info$polarity)])
  if (length(recorded) > 1) return(NA_character_)
  as.character(recorded %||% attr(x, "polarity", exact = TRUE) %||% NA)
}

#' Scan start time cvParam
#' @noRd
mzml_scan_time_param <- function(rt){
  if (length(rt) != 1 || is.na(rt)) return("")
  sprintf(paste0('\n        <cvParam cvRef="MS" accession="MS:1000016" ',
                 'name="scan start time" value="%s" unitCvRef="UO" ',
                 'unitAccession="UO:0000031" unitName="minute"/>'),
          as.character(rt))
}

#' Scan polarity cvParam
#' @noRd
mzml_polarity_param <- function(polarity){
  polarity <- andi_ms_polarity(polarity)
  if (is.null(polarity)) return("")
  positive <- polarity == "Positive Polarity"
  sprintf('\n    <cvParam cvRef="MS" accession="%s" name="%s scan"/>',
          if (positive) "MS:1000130" else "MS:1000129",
          if (positive) "positive" else "negative")
}

#' Precursor block of an MSn spectrum
#'
#' `activation` is required by the schema even where nothing is recorded about
#' it, so the parent term is written rather than a specific method: no parser
#' in the package reads a dissociation method or a collision energy.
#' @noRd
mzml_precursor_list <- function(ms_level, precursor_mz, parent_id){
  if (ms_level <= 1 || length(precursor_mz) != 1 || is.na(precursor_mz)){
    return("")
  }
  ref <- if (length(parent_id) == 1 && !is.na(parent_id)){
    sprintf(' spectrumRef="%s"', parent_id)
  } else ""
  sprintf('
    <precursorList count="1">
      <precursor%s>
        <selectedIonList count="1">
          <selectedIon>
            <cvParam cvRef="MS" accession="MS:1000744" name="selected ion m/z" value="%s" unitCvRef="MS" unitAccession="MS:1000040" unitName="m/z"/>
          </selectedIon>
        </selectedIonList>
        <activation>
          <cvParam cvRef="MS" accession="MS:1000044" name="dissociation method"/>
        </activation>
      </precursor>
    </precursorList>', ref, as.character(precursor_mz))
}

#' Create mzML mass spectrum node
#' This function generates an mzML-formatted XML string for a single mass
#' spectrum of any level. It is designed to be used as part of a larger process
#' for creating mzML files. Mass and intensity data are encoded (and optionally
#' compressed, according to the value of `compress`) into base64 format.
#' @param scan The scan number (integer).
#' @param index The scan index (integer).
#' @param rt The retention time of the scan in minutes (numeric).
#' @param scan_data: A `data.frame` or `data.table` containing the mass of each
#' peak (in the `'mz'` column) and the intensity of each peak (in the
#' `'intensity'` column).
#' @param ms_level The MS level of the scan (integer).
#' @param precursor_mz The m/z selected for fragmentation, or `NA`.
#' @param parent_id The `id` of the spectrum the precursor was selected from,
#' or `NA`.
#' @param polarity `"positive"`, `"negative"`, or `NA`.
#' @param centroided Logical. Whether the spectrum is centroided.
#' @param tic The total ion current intensity (numeric).
#' @param bpc The peak peak current intensity (numeric).
#' @param compress Logical. Whether to compress the binary data. Defaults to
#' `TRUE`.
#' @author Ethan Bass
#' @noRd

create_mzml_ms_spectrum <- function(scan_data, scan, index, rt, ms_level = 1,
                                precursor_mz = NA_real_,
                                parent_id = NA_character_,
                                polarity = NA_character_, centroided = TRUE,
                                compress = TRUE, tic = NULL, bpc = NULL) {

  # Encode mz and intensity data
  if (nrow(scan_data) > 0){
    mz_encoded <- encode_data(scan_data$mz, compress = compress)
    int_encoded <- encode_data(scan_data$intensity, compress)
  } else{
    # an empty array still has to declare the compression the file is using
    empty <- list(base64 = "",
                  compression_param = compression_param(compress))
    mz_encoded <- empty
    int_encoded <- empty
  }
  spectrum_type <- if (ms_level > 1){
    '<cvParam cvRef="MS" accession="MS:1000580" name="MSn spectrum"/>'
  } else {
    '<cvParam cvRef="MS" accession="MS:1000579" name="MS1 spectrum"/>'
  }
  peak_mode <- if (centroided){
    '<cvParam cvRef="MS" accession="MS:1000127" name="centroid spectrum"/>'
  } else {
    '<cvParam cvRef="MS" accession="MS:1000128" name="profile spectrum"/>'
  }

  sprintf('<spectrum id="scan=%d" index="%d" defaultArrayLength="%d">
    %s
    <cvParam cvRef="MS" accession="MS:1000511" name="ms level" value="%d"/>
    %s%s
    <cvParam cvRef="MS" accession="MS:1000505" name="base peak intensity" unitAccession="MS:1000131" unitName="number of detector counts" unitCvRef="MS" value="%f"/>
    <cvParam cvRef="MS" accession="MS:1000285" name="total ion current" value="%f"/>
    <scanList count="1">
    <cvParam cvRef="MS" accession="MS:1000795" name="no combination" value=""/>
      <scan>%s
      </scan>
    </scanList>%s
    <binaryDataArrayList count="2">
      <binaryDataArray encodedLength="%d">
        <cvParam cvRef="MS" accession="MS:1000514" name="m/z array" unitCvRef="MS" unitAccession="MS:1000040" unitName="m/z"/>
        <cvParam cvRef="MS" accession="MS:1000523" name="64-bit float"/>
        %s
        <binary>%s</binary>
      </binaryDataArray>
      <binaryDataArray encodedLength="%d">
        <cvParam cvRef="MS" accession="MS:1000515" name="intensity array" unitCvRef="MS" unitAccession="MS:1000131" unitName="number of detector counts"/>
        <cvParam cvRef="MS" accession="MS:1000523" name="64-bit float"/>
        %s
        <binary>%s</binary>
      </binaryDataArray>
    </binaryDataArrayList>
  </spectrum>',
          scan, index, nrow(scan_data), spectrum_type, ms_level, peak_mode,
          mzml_polarity_param(polarity), bpc, tic, mzml_scan_time_param(rt),
          mzml_precursor_list(ms_level, precursor_mz, parent_id),
          nchar(mz_encoded$base64), mz_encoded$compression_param, mz_encoded$base64,
          nchar(int_encoded$base64), int_encoded$compression_param, int_encoded$base64)
}

#' Create mzML DAD spectrum node
#' This function generates an mzML-formatted XML string for a single HPLC-DAD
#' spectrum. It is designed to be used as part of a larger process for creating
#' mzML files from HPLC-DAD data. Wavelength and intensity data are encoded (and
#' optionally compressed, according to the value of `compress`) into base64
#' format.
#' @param scan The scan number (integer).
#' @param index The scan index (integer).
#' @param rt The retention time of the scan in minutes (numeric).
#' @param scan_data: A `data.frame` or `data.table` containing the
#' wavelength of each scan (in the `'lambda'` column) and the intensity of
#' each scan (in the `'int'` column).
#' @param tic Extra argument.
#' @param bpc Extra argument.
#' @param centroided Extra argument.
#' @param compress Logical. Whether to compress the binary data. Defaults to
#' `TRUE`.
#' @author Ethan Bass
#' @noRd

create_mzml_dad_spectrum <- function(scan_data, scan, index, rt, tic = NULL,
                                     bpc = NULL, centroided = TRUE,
                                     compress = TRUE) {
  # Encode wavelength and intensity data
  wavelength_encoded <- encode_data(scan_data$lambda, compress = compress)
  int_encoded <- encode_data(scan_data$intensity, compress = compress)
  ID <- sprintf('controllerType=4 controllerNumber=1 scan=%d', scan)
  block <- sprintf('<spectrum id="%s" index="%d" defaultArrayLength="%d">
    <cvParam cvRef="MS" accession="MS:1000804" value="" name="electromagnetic radiation spectrum" />
    <cvParam cvRef="MS" accession="MS:1000525" value="" name="spectrum representation" />
    <cvParam cvRef="UO" accession="MS:1000619" value="%s" name="lowest observed wavelength" unitAccession="UO:0000018" unitName="nanometer" unitCvRef="MS" />
    <cvParam cvRef="MS" accession="MS:1000618" value="%s" name="highest observed wavelength" unitAccession="UO:0000018" unitName="nanometer" unitCvRef="UO" />
    <scanList count="1">
      <cvParam cvRef="MS" accession="MS:1000795" value="" name="no combination" />
      <scan>%s
      </scan>
    </scanList>
    <binaryDataArrayList count="2">
      <binaryDataArray encodedLength="%d">
        <cvParam cvRef="MS" accession="MS:1000617" value="" name="wavelength array" unitAccession="UO:0000018" unitName="nanometer" unitCvRef="UO" />
        <cvParam cvRef="MS" accession="MS:1000523" value="" name="64-bit float" />
        %s
        <binary>%s</binary>
      </binaryDataArray>
      <binaryDataArray encodedLength="%d">
        <cvParam cvRef="MS" accession="MS:1000515" value="" name="intensity array" unitAccession="UO:0000269" unitName="absorbance unit" unitCvRef="UO" />
        <cvParam cvRef="MS" accession="MS:1000523" value="" name="64-bit float" />
        %s
        <binary>%s</binary>
      </binaryDataArray>
    </binaryDataArrayList>
  </spectrum>',
          ID, index, length(scan_data$lambda),
          min(scan_data$lambda), max(scan_data$lambda),
          mzml_scan_time_param(rt),
          nchar(wavelength_encoded$base64),
          wavelength_encoded$compression_param, wavelength_encoded$base64,
          nchar(int_encoded$base64),
          int_encoded$compression_param, int_encoded$base64)
}

#' Write mzML chromList
#' @author Ethan Bass
#' @noRd
write_mzml_chromlist <- function(w, data, what = c("TIC", "BPC"),
                                 indexed = TRUE, compress = TRUE,
                                 verbose = getOption("verbose")){
  chroms <- match.arg(toupper(what), c("TIC", "BPC"), several.ok = TRUE)
  if (length(chroms) > 0){
    chrom_index <- vector("list", length(chroms))
    mz_write(w, sprintf('    <chromatogramList count="%d" defaultDataProcessingRef="chromConverter_processing">\n',
                length(chroms)))
    c_index <- 0
    if (any(chroms == "TIC")){
      if (indexed){
        chrom_index[[c_index + 1]] <- list(id = "TIC", offset = w$pos)
      }
      write_mzml_chrom(w = w, data = data, index = c_index, what = "TIC",
                       compress = compress, verbose = verbose)
      c_index <- c_index + 1
    }
    if (any(chroms == "BPC")){
      if (indexed){
        # `idRef` must reference the element's `id`, which is the name
        chrom_index[[c_index + 1]] <- list(id = "BPC", offset = w$pos)
      }
      write_mzml_chrom(w = w, data = data, index = c_index, what = "BPC",
                       compress = compress, verbose = verbose)
      c_index <- c_index + 1
    }
    mz_write(w, '      </chromatogramList>\n')
  }
  chrom_index
}

#' Write mzML chromatogram
#' @author Ethan Bass
#' @noRd
write_mzml_chrom <- function(w, index, data, what = c("TIC", "BPC"),
                             compress = TRUE, verbose = getOption("verbose")){
  what <- match.arg(toupper(what), c("TIC", "BPC"))
  if (verbose) message(sprintf("Writing %s.", toupper(what)))
  cdata <- data[[what]]
  if (attr(cdata, "data_format") == "wide"){
    cdata <- reshape_chrom_long(cdata, format_out = "data.frame")
  }
  if (!inherits(cdata, "data.frame")){
    cdata <- as.data.frame(cdata)
  }
  array_length <- nrow(cdata)
  rt_encoded <- encode_data(cdata$rt, compress = compress)
  int_encoded <- encode_data(cdata$intensity, compress = compress)
  id <- what
  cv_param <- switch(what, "TIC" = '<cvParam cvRef="MS" accession="MS:1000235" name="total ion current chromatogram" value=""/>',
                     "BPC" = '<cvParam cvRef="MS" accession="MS:1000628" name="basepeak chromatogram" value=""/>')

  mz_write(w, sprintf('<chromatogram id="%s" index="%d" defaultArrayLength="%d">
      %s
        <binaryDataArrayList count="2">
            <binaryDataArray encodedLength="%d">
              <cvParam cvRef="MS" accession="MS:1000595" value="" name="time array" unitAccession="UO:0000031" unitName="minute" unitCvRef="UO" />
              <cvParam cvRef="MS" accession="MS:1000523" value="" name="64-bit float" />
              %s
              <binary>%s</binary>
      </binaryDataArray>
      <binaryDataArray encodedLength="%d">
        <cvParam cvRef="MS" accession="MS:1000515" value="" name="intensity array" unitAccession="MS:1000131" unitName="number of counts" unitCvRef="MS" />
        <cvParam cvRef="MS" accession="MS:1000523" name="64-bit float"/>
        %s
        <binary>%s</binary>
      </binaryDataArray>
    </binaryDataArrayList>
        </chromatogram>\n', id, index, array_length, cv_param,
              nchar(rt_encoded$base64), rt_encoded$compression_param, rt_encoded$base64,
              nchar(int_encoded$base64), int_encoded$compression_param, int_encoded$base64))
}

#' Encode mzml data
#' Encodes array in base64 and optionally compresses using zlib compression
#' according to the value of `compress`.
#' @param x A numeric vector containing the data to be encoded.
#' @param compress Logical. Whether to compress the data using zlib compression.
#' Defaults to `TRUE`.
#' @author Ethan Bass
#' @noRd
encode_data <- function(x, compress) {
  bin_data <- writeBin(as.double(x), raw(), endian = "little")
  if (compress) {
    bin_data <- memCompress(bin_data, type = "gzip")
  }
  list(base64 = base64enc::base64encode(bin_data),
       compression_param = compression_param(compress))
}

#' Compression cvParam
#' `memCompress(type = "gzip")` emits zlib-format output, which is what
#' `MS:1000574` calls for.
#' @noRd
compression_param <- function(compress){
  if (compress){
    '<cvParam cvRef="MS" accession="MS:1000574" name="zlib compression" />'
  } else {
    '<cvParam cvRef="MS" accession="MS:1000576" name="no compression" />'
  }
}
