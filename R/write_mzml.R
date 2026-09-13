#' Write mzML
#'
#' This function constructs mzML files by writing XML strings directly to a file
#' connection. While this approach is fast, it may be less flexible than
#' methods based on an explicit Document Object Model (DOM).
#'
#' The function supports writing various types of spectral data including MS1,
#' TIC (Total Ion Current), BPC (Base Peak Chromatogram), and DAD
#' (Diode Array Detector) data. DAD spectra are written as electromagnetic
#' radiation spectra (`MS:1000804`) using Thermo's naming convention with
#' `controllerType=4` in the spectrum ID for compatibility with existing
#' tools. Support for MS2 may be added in a future release.
#'
#' If `indexed = TRUE`, the function will generate an indexed mzML file, which
#' allows faster random access to spectra.
#'
#' @importFrom utils packageVersion
#' @param data List of `data.frame`s or `data.table`s containing spectral data.
#' @param path_out The path to write the file.
#' @param sample_name The name of the file. If a name is not provided, the name
#' will be derived from the `sample_name` attribute.
#' @param what Which streams to write to mzML: `"MS1"`, `"MS2"`, `"TIC"`,
#' `"BPC"`, and/or `"DAD"`.
#' @param instrument_info Instrument info to write to mzML file.
#' @param compress Logical. Whether to use zlib compression. Defaults to `TRUE`.
#' @param indexed Logical. Whether to write indexed mzML. Defaults to `TRUE`.
#' @param force Logical. Whether to overwrite existing files at `path_out`.
#' Defaults to `FALSE`.
#' @param show_progress Logical. Whether to show progress bar. Defaults to `TRUE`.
#' @param verbose Logical. Whether or not to print status messages.
#' @return Invisibly returns the path to the written mzML file.
#' @author Ethan Bass
#' @family write functions
#' @export

write_mzml <- function(data, path_out, sample_name = NULL, what = NULL,
                      instrument_info = NULL, compress = TRUE, indexed = TRUE,
                      force = FALSE, show_progress = TRUE,
                       verbose = getOption("verbose")) {
  if (!inherits(data, "list")){
    detector <- attr(data,"detector")
    detector <- switch(detector, "UV" = "DAD", "MS" = "MS1", "DAD" = "DAD")
    data <- setNames(list(data), detector)
    what <- detector
  }
  names(data) <- toupper(names(data))
  if (is.null(what)){
    what <- names(data[sapply(data, nrow) > 0])
  }
  what <- match.arg(toupper(what), c("MS1", "MS2", "TIC", "BPC", "DAD"),
                    several.ok = TRUE)
  if (is.null(sample_name)){
    sample_name <- ifelse(inherits(data, "list"),
                          attr(data[[1]], "sample_name"),
                          attr(data, "sample_name"))
  }
  file_out <- get_filepath(path_out = path_out, sample_name = sample_name,
                           force = force, ext = "mzML")

  w <- new_mzml_writer(file_out)
  on.exit(close(w$con))

  if (any(what %in% c("MS1", "MS2", "DAD"))){
    n_scan <- sum(sapply(what[what %in% c("MS1", "MS2", "DAD")], function(i){
      tryCatch(count_scans(data[[i]]), error = function(cond) NA)
    }), na.rm = TRUE)
  } else n_scan <- 0
  if ("MS1" %in% what){
    meta <- attributes(data$MS1)
    } else meta <- attributes(data$DAD)
  write_mzml_header(w, meta = meta, n_scan = n_scan,
                    indexed = indexed, instrument_info = instrument_info,
                    sample_name = meta$sample_name)
  spectrum_indices <- c()
  if (any(what == "MS1")){
    if ("MS1" %in% names(data)){
      MS1 <- write_spectra(w, data = data, what = "MS1", indexed = indexed,
                             idx_start = 0, compress = compress,
                             show_progress = show_progress, verbose = verbose)
      spectrum_indices <- c(spectrum_indices, MS1)
    } else{
      warning("MS1 data not found.")
    }
  }
  if (any(what == "DAD")){
    if ("DAD" %in% names(data)){
      idx <- try(spectrum_indices[[length(spectrum_indices)]]$id)
      start <- ifelse(is.null(idx), 0, as.numeric(gsub("scan=", "", idx)))
      DAD <- write_spectra(w, data, what = "DAD", indexed = indexed,
                             idx_start = start, compress = compress,
                             show_progress = show_progress, verbose = verbose)
      spectrum_indices <- c(spectrum_indices, DAD)
    } else{
      warning("DAD data not found.")
    }
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


#' Write mzML header
#' @param w mzML writer (see `new_mzml_writer`).
#' @param n_scan Number of scans to be included in mzML file.
#' @param indexed Logical. Whether mzML file is to be indexed.
#' @author Ethan Bass
#' @noRd
write_mzml_header <- function(w, meta, n_scan, indexed = TRUE,
                              instrument_info = NULL, sample_name){
  # Write XML declaration and opening tags
  mz_write(w,
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    ifelse(indexed, '<indexedmzML xmlns="http://psi.hupo.org/ms/mzml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://psi.hupo.org/ms/mzml http://psi.hupo.org/ms/mzml">\n', ''),
    sprintf('<mzML xmlns="http://psi.hupo.org/ms/mzml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://psi.hupo.org/ms/mzml http://psi.hupo.org/ms/mzml" id="%s" version="1.1.0">\n',
            sample_name),
    '<cvList count="2">
          <cv id="MS" fullName="Proteomics Standards Initiative Mass Spectrometry Ontology" version="4.1.0" URI="https://raw.githubusercontent.com/HUPO-PSI/psi-ms-CV/master/psi-ms.obo"/>
          <cv id="UO" fullName="Unit Ontology" version="releases/2020-03-10" URI="http://data.bioontology.org/ontologies/UO"/>
      </cvList>\n',
  create_mzml_file_description(meta),
  create_mzml_sample_list(meta),
  create_mzml_software_list(),
  '<instrumentConfigurationList count="1">
    <instrumentConfiguration id="IC">\n')


  if (!is.null(instrument_info)) {
    for (param in instrument_info) {
      mz_write(w, sprintf('      <cvParam cvRef="%s" accession="%s" name="%s" value="%s"/>\n',
                  param$cvRef, param$accession, param$name, param$value))
    }
  } else {
    mz_write(w, '      <cvParam cvRef="MS" accession="MS:1000031" name="instrument model"/>\n')
  }
  date_time <- tryCatch(format(meta$run_datetime[1], "%Y-%m-%dT%H:%M:%SZ"), error = function(err) NA)
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
create_mzml_sample_list <- function(meta){
  sprintf(
  '<sampleList count="1">
    <sample id="%s" name="%s">
    </sample>
  </sampleList>
          ', paste0("s", meta$sample_id), meta$sample_name)
}

#' Create mzml file description
#' @noRd
create_mzml_file_description <- function(meta){
  sprintf(
  '  <fileDescription>
        <fileContent>
          <cvParam cvRef="MS" accession="MS:1000294" name="mass spectrum"/>
        </fileContent>
        <sourceFileList count="1">
          <sourceFile id="SF1" name="%s" location="%s">
            <cvParam cvRef="MS" accession="MS:1002597" name="MS1 format"/>
            <cvParam cvRef="MS" accession="MS:1000569" name="SHA-1" value="%s"/>
            <cvParam cvRef="MS" accession="MS:1000776" name="scan number only nativeID format"/>
          </sourceFile>
        </sourceFileList>
    </fileDescription>',
          ifelse(is.na(meta$source_file), "", basename(meta$source_file)),
          ifelse(is.na(meta$source_file), "", meta$source_file),
          ifelse(is.na(meta$source_sha1), "", meta$source_sha1))
}

#' Create mzml software list
#' @noRd
create_mzml_software_list <- function(){
  sprintf(
  '  <softwareList count="1">
      <software id="chromConverter" version="%s">
        <cvParam cvRef="MS" accession="MS:1000799" name="custom unreleased software tool" value="chromConverter R package"/>
      </software>
    </softwareList>', as.character(packageVersion("chromConverter")))
}


#' Write mzML spectra
#' @importFrom data.table .SD
#' @author Ethan Bass
#' @noRd
write_spectra <- function(w, data, what = c("MS1", "MS2", "TIC", "DAD"),
                          indexed = TRUE, idx_start = 0, compress = TRUE,
                          show_progress = TRUE,
                          verbose = getOption("verbose")){
  what <- match.arg(toupper(what), c("MS1", "MS2", "TIC", "DAD"))

  if (verbose)
    message(sprintf("Writing %s spectra.", toupper(what)))

  laplee <- ifelse(show_progress, pbapply::pblapply, lapply)

  spectra_data <- data[[toupper(what)]]

  if (attr(spectra_data, "data_format") == "wide"){
    spectra_data <- reshape_chrom_long(spectra_data)
  }
  if (!inherits(spectra_data, "data.table")){
    spectra_data <- data.table::as.data.table(spectra_data)
    attr(spectra_data, "data_format") <- "long"
  }

  create_spectrum <- switch(what,
                            "MS1" = create_mzml_ms1_spectrum,
                            "DAD" = create_mzml_dad_spectrum)

  scans <- group_scans(spectra_data)
  rts <- scans$rts
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
#' The retention times are taken from the same grouping as the rows, so scan
#' `i` and `rts[i]` cannot disagree.
#' @noRd
group_scans <- function(x){
  rt <- get_column(x, "rt")
  starts <- which(!duplicated(rt))
  if (length(starts) == length(unique(rt))){
    ends <- c(starts[-1] - 1L, length(rt))
    list(rts = rt[starts],
         get_scan = function(i) x[starts[i]:ends[i]])
  } else {
    scan_list <- split(x, rt)
    list(rts = as.numeric(names(scan_list)),
         get_scan = function(i) scan_list[[i]])
  }
}

#' Create mzML MS1 spectrum node
#' This function generates an mzML-formatted XML string for a single MS1 scan.
#' It is designed to be used as part of a larger process for creating
#' mzML files. Wavelength and intensity data are encoded (and optionally
#' compressed, according to the value of `compress`) into base64 format.
#' @param scan The scan number (integer).
#' @param index The scan index (integer).
#' @param rt The retention time of the scan in minutes (numeric).
#' @param scan_data: A `data.frame` or `data.table` containing the
#' wavelength of each scan (in the `'lambda'` column) and the intensity of
#' each scan (in the `'int'` column).
#' @param tic The total ion current intensity (numeric).
#' @param bpc The peak peak current intensity (numeric).
#' @param compress Logical. Whether to compress the binary data. Defaults to
#' `TRUE`.
#' @author Ethan Bass
#' @noRd

create_mzml_ms1_spectrum <- function(scan_data, scan, index, rt, ms_level = 1,
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

  sprintf('<spectrum id="scan=%d" index="%d" defaultArrayLength="%d">
    <cvParam cvRef="MS" accession="MS:1000580" name="MSn spectrum"/>
    <cvParam cvRef="MS" accession="MS:1000511" name="ms level" value="%d"/>
    <cvParam cvRef="MS" accession="MS:1000127" name="centroid spectrum"/>
    <cvParam cvRef="MS" accession="MS:1000505" name="base peak intensity" unitAccession="MS:1000131" unitName="number of detector counts" unitCvRef="MS" value="%f"/>
    <cvParam cvRef="MS" accession="MS:1000285" name="total ion current" value="%f"/>
    <scanList count="1">
    <cvParam cvRef="MS" accession="MS:1000795" name="no combination" value=""/>
      <scan>
        <cvParam cvRef="MS" accession="MS:1000016" name="scan start time" value="%s" unitCvRef="UO" unitAccession="UO:0000031" unitName="minute"/>
      </scan>
    </scanList>
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
          scan, index, nrow(scan_data), ms_level, bpc, tic, as.character(rt),
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
#' @param compress Logical. Whether to compress the binary data. Defaults to
#' `TRUE`.
#' @author Ethan Bass
#' @noRd

create_mzml_dad_spectrum <- function(scan_data, scan, index, rt, tic = NULL,
                                     bpc = NULL, compress = TRUE) {
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
      <scan>
        <cvParam cvRef="MS" accession="MS:1000016" value="%s" name="scan start time" unitAccession="UO:0000031" unitName="minute" unitCvRef="UO" />
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
          as.character(rt),
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
