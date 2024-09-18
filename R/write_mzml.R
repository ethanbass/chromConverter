#' Write mzML
#'
#' This function constructs mzML files using a streaming XML-writing approach,
#' generating and writing XML content directly to a file connection. This method
#' is memory-efficient and fast, making it well-suited for large mzML files.
#' However, it may be less flexible than DOM-based methods for handling complex
#' XML structures or large-scale modifications after writing has begun.
#'
#' This function constructs mzML files by writing XML strings directly to a file
#' connection. While this approach is memory-efficient and fast, it may be less
#' flexible for handling complex XML structures.
#'
#' The function supports writing various types of spectral data including MS1,
#' MS2, TIC (Total Ion Current), BPC (Base Peak Chromatogram), and DAD
#' (Diode Array Detector) data.
#'
#' If indexed = TRUE, the function will create an indexed mzML file, which
#' allows faster random access to spectra.
#'
#' @importFrom utils packageVersion
#' @param data List of data.frames or data.tables containing spectral data.
#' @param path_out Path to write mzML file.
#' @param what Which streams to write to mzML: \code{"ms1"}, \code{"ms2"},
#' \code{"tic"}, \code{"bpc"}, and/or \code{"dad"}.
#' @param instrument_info Instrument info to write to mzML file.
#' @param compress Logical. Whether to use zlib compression. Defaults to
#' \code{TRUE}.
#' @param indexed Logical. Whether to write indexed mzML. Defaults to
#' \code{TRUE}.
#' @param force Logical. Whether to overwrite existing files at \code{path_out}.
#' Defaults to \code{FALSE}.
#' @param show_progress Logical. Whether to show progress bar. Defaults to
#' \code{TRUE}.
#' @param verbose Logical. Whether or not to print status messages.
#' @author Ethan Bass
#' @return Invisibly returns the path to the written mzML file.


write_mzml <- function(data, path_out, what = NULL,
                      instrument_info = NULL, compress = TRUE, indexed = TRUE,
                      force = FALSE, show_progress = TRUE,
                       verbose = getOption("verbose")) {
  if (!inherits(data, "list")){
    data <- list(MS1 = data)
  }
  # sapply(data[what], nrow)
  # check_for_pkg("data.table")

  if (is.null(what)){
    what <- names(data[sapply(data,nrow) > 0])
  }
  what <- match.arg(toupper(what), c("MS1", "MS2", "TIC", "BPC", "DAD"),
                    several.ok = TRUE)

  if (fs::file_exists(path_out)){
    if (force){
      fs::file_delete(path_out)
    } else{
      stop("File already exists and will not be overwritten. To overwrite files set `force == TRUE`.")
    }
  }

  # Create a connection to write the file
  con <- file(path_out, "wt")
  on.exit(close(con))

  n_scan <- sum(sapply(what[what %in% c("MS1","MS2","DAD")], function(i){
    tryCatch(length(unique(data[[i]][,"rt"])), error = function(cond) NA)
  }), na.rm = TRUE)

  write_mzml_header(con, meta = attributes(data$MS1), n_scan = n_scan,
                    indexed = indexed, instrument_info = instrument_info)

  if (any(what == "MS1")){
    if ("MS1" %in% names(data)){
    index <- write_spectra(con, data = data, what = "MS1", indexed = indexed,
                           idx_start = 0, show_progress = show_progress,
                           verbose = verbose)
    } else{
      warning("MS1 data not found.")
    }
  }
  if (any(what == "DAD")){
    if ("DAD" %in% names(data)){
      idx <- try(index[[length(index)]]$id)
      start <- ifelse(is.null(idx), 0, as.numeric(gsub("scan=", "", idx)))
      index <- write_spectra(con, data, what = "DAD", indexed = indexed,
                             idx_start = start, show_progress = show_progress, verbose = verbose)
    } else{
      warning("DAD data not found.")
    }
  }

  cat('    </spectrumList>\n', file = con) # close spectrumList

  cat('</run>
</mzML>\n', file = con)

  if (indexed){
    indexListOffset <- seek(con)
    cat('<indexList count="1">
    <index name="spectrum">\n', file = con)
    for (entry in index) {
      cat(sprintf('    <offset idRef="%s">%d</offset>\n', entry$id, entry$offset),
          file = con)
    }
    cat('  </index>
  </indexList>\n', file = con)

    # Write final tags
    cat(sprintf('<indexListOffset>%d</indexListOffset>
  </indexedmzML>', indexListOffset), file = con)
}
  return(invisible(path_out))
}

#' Write mzML header
#' @param con Connection to write mzML file.
#' @param n_scan Number of scans to be included in mzML file.
#' @param indexed Logical. Whether mzML file is to be indexed.
#' @author Ethan Bass
#' @noRd
write_mzml_header <- function(con, meta, n_scan, indexed = TRUE,
                              instrument_info = NULL){
  start_tag <- ifelse(indexed,
                      '<indexedmzML xmlns="http://psi.hupo.org/ms/mzml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://psi.hupo.org/ms/mzml http://psi.hupo.org/ms/mzml">
                        <mzML version="1.1.0">',
                      '<mzML xmlns="http://psi.hupo.org/ms/mzml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://psi.hupo.org/ms/mzml http://psi.hupo.org/ms/mzml">
                        version="1.1.0">')
  # Write XML declaration and opening tags
  cat(
    '<?xml version="1.0" encoding="UTF-8"?>\n', start_tag,
    '  <cvList count="2">
          <cv id="MS" fullName="Proteomics Standards Initiative Mass Spectrometry Ontology" version="4.1.0" URI="https://raw.githubusercontent.com/HUPO-PSI/psi-ms-CV/master/psi-ms.obo"/>
          <cv id="UO" fullName="Unit Ontology" version="releases/2020-03-10" URI="http://data.bioontology.org/ontologies/UO"/>
      </cvList>',
  create_mzml_file_description(meta),
  create_mzml_sample_list(meta),
  create_mzml_software_list(),
  '<instrumentConfigurationList count="1">
    <instrumentConfiguration id="IC">\n', file = con, sep = "")


  if (!is.null(instrument_info)) {
    for (param in instrument_info) {
      cat(sprintf('      <cvParam cvRef="%s" accession="%s" name="%s" value="%s"/>\n',
                  param$cvRef, param$accession, param$name, param$value), file = con)
    }
  } else {
    cat('      <cvParam cvRef="MS" accession="MS:1000031" name="instrument model"/>\n', file = con)
  }

  cat(sprintf('    </instrumentConfiguration>
  </instrumentConfigurationList>
  <dataProcessingList count="1">
    <dataProcessing id="RaMS_processing">
      <processingMethod order="0" softwareRef="chromConverter">
        <cvParam cvRef="MS" accession="MS:1000544" name="Conversion to mzML"/>
      </processingMethod>
    </dataProcessing>
  </dataProcessingList>
  <run id="run1" defaultInstrumentConfigurationRef="IC" startTimeStamp="%s">
    <spectrumList count="%d">\n',
              format(meta$run_datetime[1], "%Y-%m-%dT%H:%M:%SZ"), n_scan),
      file = con, sep = "")
}

create_mzml_sample_list <- function(meta){
  sprintf(
  '<sampleList count="1">
    <sample id="%d" name="%s">
    </sample>
  </sampleList>
          ', meta$sample_id, meta$sample_name)
}

create_mzml_file_description <- function(meta){
  sprintf('<fileDescription>
    <fileContent>
      <cvParam cvRef="MS" accession="MS:1000294" name="mass spectrum"/>
    </fileContent>
    <sourceFileList count="1">
      <sourceFile id="SF1" name="%s" location="%s">
      <cvParam cvRef="MS" accession="MS:1000769" name="file creation time" value="%s"/>
    </sourceFile>
  </sourceFileList>
  </fileDescription>', basename(meta$source_file), meta$source_file,
          format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"))
}

create_mzml_software_list <- function(){
  sprintf('<softwareList count="1">
    <software id="chromConverter" version="%s">
      <cvParam cvRef="MS" accession="MS:1000799" name="custom unreleased software tool" value="chromConverter R package"/>
    </software>
  </softwareList>', as.character(packageVersion("chromConverter")))
}


#' Write mzML spectra
#' @importFrom data.table .SD
#' @author Ethan Bass
#' @noRd
write_spectra <- function(con, data, what = c("MS1", "MS2", "TIC", "DAD"),
                          indexed = TRUE, idx_start = 0, show_progress = TRUE,
                          verbose = getOption("verbose")){
  what <- match.arg(toupper(what), c("MS1", "MS2", "TIC", "DAD"))

  if (verbose)
    message(sprintf("Writing %s spectra.", toupper(what)))

  laplee <- if (show_progress) pbapply::pblapply else lapply

  spectra_data <- data[[toupper(what)]]

  if (!inherits(spectra_data, "data.table")){
    spectra_data <- data.table::as.data.table(spectra_data)
  }

  create_spectrum <- switch(what,
                            "MS1" = create_mzml_ms1_spectrum,
                            "DAD" = create_mzml_dad_spectrum)

  # Write spectra and build index
  if (!is.null(data$TIC) && attr(data$TIC,"data_format") == "wide"){
    data$TIC <- data.frame(rt = as.numeric(rownames(data$TIC)),
                           intensity = data$TIC[,"intensity"])
  }
  rts <- if (!is.null(data$TIC)) data$TIC[,"rt"] else unique(spectra_data$rt)
  n_scan <- ifelse(!is.null(data$TIC), nrow(data$TIC),
                   length(unique(spectra_data$rt)))
  laplee(seq_len(n_scan), function(i) {
    scan_data <- spectra_data[rt == rts[i]]
    # Create and write spectrum
    spectrum_xml <- create_spectrum(scan = i, index = i + idx_start - 1,
                                    rt = rts[i],
                                    tic = ifelse(!is.null(data$TIC),
                                                 data$TIC[[i, "intensity"]],
                                                 ifelse(length(scan_data$intensity),
                                                        0, sum(scan_data$intensity))),
                                    bpc = ifelse(!is.null(data$BPC),
                                                 data$BPC[[i, "intensity"]],
                                                 ifelse(length(scan_data$intensity) == 0,
                                                        0, max(scan_data$intensity))),
                                    scan_data = scan_data)
    writeLines(spectrum_xml, con)

    if (indexed){
      offset <- seek(con) #- nchar(spectrum_xml)
      list(id = paste0("scan=", i), offset = offset)
    }
  })
}

#' Create mzML MS1 spectrum node
#' This function generates an mzML-formatted XML string for a single MS1 scan.
#' It is designed to be used as part of a larger process for creating
#' mzML files. Wavelength and intensity data are encoded (and optionally
#' compressed, according to the value of \code{compress}) into base64 format.
#' @param scan The scan number (integer).
#' @param index The scan index (integer).
#' @param rt The retention time of the scan in minutes (numeric).
#' @param scan_data: A \code{data.frame} or \code{data.table} containing the
#' wavelength of each scan (in the \code{'lambda'} column) and the intensity of
#' each scan (in the \code{'int'} column).
#' @param tic The total ion current intensity (numeric).
#' @param bpc The peak peak current intensity (numeric).
#' @param compress Logical. Whether to compress the binary data. Defaults to
#' \code{TRUE}.
#' @author Ethan Bass
#' @noRd

create_mzml_ms1_spectrum <- function(scan, index, rt, scan_data, ms_level = 1,
                                compress = TRUE, tic = NULL, bpc = NULL) {

  # Encode mz and intensity data
  mz_encoded <- encode_data(scan_data$mz, compress = compress)
  int_encoded <- encode_data(scan_data$intensity, compress)

  sprintf('
  <spectrum id="scan=%d" index="%d">
    <cvParam cvRef="MS" accession="MS:1000511" name="ms level" value="%d"/>
    <cvParam cvRef="MS" accession="MS:1000127" name="centroid spectrum"/>
    <cvParam cvRef="MS" accession="MS:1000505" name="base peak intensity" value="%f"/>
    <cvParam cvRef="MS" accession="MS:1000285" name="total ion current" value="%f"/>
    <scanList count="1">
      <scan>
        <cvParam cvRef="MS" accession="MS:1000016" name="scan start time" value="%s" unitCvRef="UO" unitAccession="UO:0000031" unitName="minute"/>
      </scan>
    </scanList>
    <binaryDataArrayList count="2">
      <binaryDataArray>
        <cvParam cvRef="MS" accession="MS:1000514" name="m/z array" unitCvRef="MS" unitAccession="MS:1000040" unitName="m/z"/>
        <cvParam cvRef="MS" accession="MS:1000523" name="64-bit float"/>
        %s
        <binary>%s</binary>
      </binaryDataArray>
      <binaryDataArray>
        <cvParam cvRef="MS" accession="MS:1000515" name="intensity array" unitCvRef="MS" unitAccession="MS:1000131" unitName="number of detector counts"/>
        <cvParam cvRef="MS" accession="MS:1000523" name="64-bit float"/>
        %s
        <binary>%s</binary>
      </binaryDataArray>
    </binaryDataArrayList>
  </spectrum>',
          scan, index, ms_level, bpc, tic, as.character(rt),
          mz_encoded$compression_param, mz_encoded$base64,
          int_encoded$compression_param, int_encoded$base64)
}

#' Create mzML DAD spectrum node
#' This function generates an mzML-formatted XML string for a single HPLC-DAD
#' spectrum. It is designed to be used as part of a larger process for creating
#' mzML files from HPLC-DAD data. Wavelength and intensity data are encoded (and
#' optionally compressed, according to the value of \code{compress}) into base64
#' format.
#' @param scan The scan number (integer).
#' @param index The scan index (integer).
#' @param rt The retention time of the scan in minutes (numeric).
#' @param scan_data: A \code{data.frame} or \code{data.table} containing the
#' wavelength of each scan (in the \code{'lambda'} column) and the intensity of
#' each scan (in the \code{'int'} column).
#' @param tic Extra argument.
#' @param bpc Extra argument.
#' @param compress Logical. Whether to compress the binary data. Defaults to
#' \code{TRUE}.
#' @author Ethan Bass
#' @noRd

create_mzml_dad_spectrum <- function(scan, index, rt, scan_data, tic = NULL,
                                     bpc = NULL, compress = TRUE) {
  # Encode wavelength and intensity data
  wavelength_encoded <- encode_data(scan_data$lambda, compress = compress)
  int_encoded <- encode_data(scan_data$intensity, compress = compress)

  sprintf('
  <spectrum id="controllerType=4 controllerNumber=1 scan=%d" index="%d" defaultArrayLength="%d">
    <cvParam cvRef="MS" accession="MS:1000804" value="" name="electromagnetic radiation spectrum" />
    <cvParam cvRef="MS" accession="MS:1000504" value="0" name="base peak m/z" unitAccession="MS:1000040" unitName="m/z" unitCvRef="MS" />
    <cvParam cvRef="MS" accession="MS:1000505" value="0" name="base peak intensity" unitAccession="MS:1000131" unitName="number of detector counts" unitCvRef="MS" />
    <cvParam cvRef="UO" accession="MS:1000619" value="%s" name="lowest observed wavelength" unitAccession="UO:0000018" unitName="nanometer" unitCvRef="MS" />
    <cvParam cvRef="MS" accession="MS:1000618" value="%s" name="highest observed wavelength" unitAccession="UO:0000018" unitName="nanometer" unitCvRef="UO" />
    <scanList count="1">
      <cvParam cvRef="MS" accession="MS:1000795" value="" name="no combination" />
      <scan>
        <cvParam cvRef="MS" accession="MS:1000016" value="%s" name="scan start time" unitAccession="UO:0000031" unitName="minute" unitCvRef="UO" />
        <scanWindowList count="1">
          <scanWindow>
            <cvParam cvRef="MS" accession="MS:1000501" value="%s" name="scan window lower limit" unitAccession="UO:0000018" unitName="nanometer" unitCvRef="UO" />
            <cvParam cvRef="MS" accession="MS:1000500" value="%s" name="scan window upper limit" unitAccession="UO:0000018" unitName="nanometer" unitCvRef="UO" />
          </scanWindow>
        </scanWindowList>
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
          scan, index, length(scan_data$lambda),
          min(scan_data$lambda), max(scan_data$lambda),
          as.character(rt),
          min(scan_data$lambda), max(scan_data$lambda),
          nchar(wavelength_encoded$base64),
          wavelength_encoded$compression_param, wavelength_encoded$base64,
          nchar(int_encoded$base64),
          int_encoded$compression_param, int_encoded$base64)
}

# write TIC, etc to chromlist as well

#' Write mzML chromList
#' @author Ethan Bass
#' @noRd
write_mzml_chromlist <- function(con, data, what = c("TIC","BPC"),
                                 indexed = TRUE, compress = TRUE,
                                 verbose = getOption("verbose")){
  what <- match.arg(toupper(what), c("TIC","BPC"))
  chroms <- what[what %in% c("TIC", "BPC")]
  if (length(chroms) > 0){
    chrom_index <- vector("list", length(chroms))
    cat(sprintf('    <chromatogramList count="%d">', length(chroms)), file = con)
    c_index <- 0
    if (any(chroms == "tic")){
      if (indexed){
        chrom_index[[c_index + 1]] <- list(id = c_index, offset = seek(con, NA))
      }
        chrom_index <- seek(con, NA)
      write_mzml_chrom(con = con, data = data, index = c_index, what = "tic",
                       compress = compress, verbose = verbose)
      c_index <- c_index + 1
    }
    if (any(chroms == "bpc")){
      if (indexed){
        chrom_index[[c_index + 1]] <- list(id = c_index, offset = seek(con, NA))
      }
      write_mzml_chrom(con = con, data = data, index = c_index, what = "bpc",
                       compress = compress, verbose = verbose)
      c_index <- c_index + 1
    }
    cat('
      </chromatogramList>    ', file = con)
  }
}

#' Write mzML chromatogram
#' @author Ethan Bass
#' @noRd
write_mzml_chrom <- function(con, index, data, what = c("tic", "bpc"),
                             compress = TRUE, verbose = getOption("verbose")){
  what <- match.arg(what, c("tic", "bpc"))
  if (verbose) message(sprintf("Writing %s.", toupper(what)))
  cdata <- data[[toupper(what)]]
  rt_encoded <- encode_data(cdata$rt, compress = compress)
  int_encoded <- encode_data(cdata$intensity, compress = compress)
  id <- toupper(what)
  cv_param <- switch(what, "tic" = '<cvParam cvRef="MS" accession="MS:1000235" name="total ion current chromatogram" value=""/>',
                     "bpc" = '<cvParam cvRef="MS" accession="MS:1000628" name="basepeak chromatogram" value=""/>')

  cat(sprintf('    <chromatogram index="%d" id="%s">
      %s
        <binaryDataArrayList count="2">
            <binaryDataArray>
              <cvParam cvRef="MS" accession="MS:1000595" value="" name="time array" unitAccession="UO:0000031" unitName="minute" unitCvRef="UO" />
              <cvParam cvRef="MS" accession="MS:1000523" value="" name="64-bit float" />
              %s
              <binary>%s</binary>
      </binaryDataArray>
      <binaryDataArray>
        <cvParam cvRef="MS" accession="MS:1000515" value="" name="intensity array" unitAccession="MS:1000131" unitName="number of counts" unitCvRef="MS" />
        <cvParam cvRef="MS" accession="MS:1000523" name="64-bit float"/>
        %s
        <binary>%s</binary>
      </binaryDataArray>
    </binaryDataArrayList>
        </chromatogram>', index, id, cv_param,
              rt_encoded$compression_param, rt_encoded$base64,
              int_encoded$compression_param, int_encoded$base64), file = con)
}

#' Encode mzml data
#' Encodes array in base64 and optionally compresses using zlib compression
#' according to the value of \code{compress}.
#' @param x A numeric vector containing the data to be encoded.
#' @param compress Logical. Whether to compress the data using zlib compression.
#' Defaults to \code{TRUE}.
#' @author Ethan Bass
#' @noRd
encode_data <- function(x, compress) {
  bin_data <- writeBin(x, raw(), endian = "little")
  if (compress) {
    bin_data <- memCompress(bin_data, type = "gzip")
    compression_param <- '<cvParam cvRef="MS" accession="MS:1000574" name="zlib compression" />'
  } else {
    compression_param <- '<cvParam cvRef="MS" accession="MS:1000574" name="none" />'
  }
  list(base64 = base64enc::base64encode(bin_data), compression_param = compression_param)
}
