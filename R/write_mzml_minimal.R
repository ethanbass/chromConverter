
writeMzMLFromScratch_XML <- function(spectra_data, path_out, instrument_info = NULL, show_progress = TRUE) {
  laplee <- choose_apply_fnc(show_progress)

  # Create a connection to write the file
  con <- file(path_out, "wt")
  on.exit(close(con))

  # Write XML declaration
  writeLines('<?xml version="1.0" encoding="UTF-8"?>', con)

  # Create the root element and mzML
  root <- newXMLNode("mzML")
  addAttributes(root,
                "xmlns" = "http://psi.hupo.org/ms/mzml",
                "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance"

                "xsi:schemaLocation" = "http://psi.hupo.org/ms/mzml http://psi.hupo.org/ms/mzml",
  )
  ,
                     namespaceDefinitions = c("xmlns" = "http://psi.hupo.org/ms/mzml",
                                              "xsi" = "http://www.w3.org/2001/XMLSchema-instance",
                                              "xsi:schemaLocation" = "http://psi.hupo.org/ms/mzml http://psi.hupo.org/ms/mzml"))
  root
  mzML <- newXMLNode("mzML", attrs = c(version = "1.1.0"), parent = root)

  # Add cvList
  cvList <- newXMLNode("cvList", attrs = c(count = "2"), parent = mzML)
  newXMLNode("cv", attrs = c(id = "MS", fullName = "Proteomics Standards Initiative Mass Spectrometry Ontology",
                             version = "4.1.0", URI = "https://raw.githubusercontent.com/HUPO-PSI/psi-ms-CV/master/psi-ms.obo"), parent = cvList)
  newXMLNode("cv", attrs = c(id = "UO", fullName = "Unit Ontology",
                             version = "releases/2020-03-10", URI = "http://data.bioontology.org/ontologies/UO"), parent = cvList)

  # Add fileDescription
  fileDescription <- newXMLNode("fileDescription", parent = mzML)
  fileContent <- newXMLNode("fileContent", parent = fileDescription)
  newXMLNode("cvParam", attrs = c(cvRef = "MS", accession = "MS:1000294", name = "mass spectrum"), parent = fileContent)

  # Add softwareList
  softwareList <- newXMLNode("softwareList", attrs = c(count = "1"), parent = mzML)
  software <- newXMLNode("software", attrs = c(id = "chromConverter", version = as.character(packageVersion("chromConverter"))), parent = softwareList)
  newXMLNode("cvParam", attrs = c(cvRef = "MS", accession = "MS:1000799", name = "custom unreleased software tool", value = "chromConverter R package"), parent = software)

  # Add instrumentConfigurationList
  instrumentConfigurationList <- newXMLNode("instrumentConfigurationList", attrs = c(count = "1"), parent = mzML)
  instrumentConfiguration <- newXMLNode("instrumentConfiguration", attrs = c(id = "IC"), parent = instrumentConfigurationList)

  if (!is.null(instrument_info)) {
    for (param in instrument_info) {
      newXMLNode("cvParam", attrs = c(cvRef = param$cvRef, accession = param$accession, name = param$name, value = param$value), parent = instrumentConfiguration)
    }
  } else {
    newXMLNode("cvParam", attrs = c(cvRef = "MS", accession = "MS:1000031", name = "instrument model"), parent = instrumentConfiguration)
  }

  # Add dataProcessingList
  dataProcessingList <- newXMLNode("dataProcessingList", attrs = c(count = "1"), parent = mzML)
  dataProcessing <- newXMLNode("dataProcessing", attrs = c(id = "RaMS_processing"), parent = dataProcessingList)
  processingMethod <- newXMLNode("processingMethod", attrs = c(order = "0", softwareRef = "RaMS"), parent = dataProcessing)
  newXMLNode("cvParam", attrs = c(cvRef = "MS", accession = "MS:1000544", name = "Conversion to mzML"), parent = processingMethod)

  # Write the root and mzML opening tags to the file incrementally
  writeLines(saveXML(root, indent = TRUE, prefix = ""), con)

  # Add run and spectrumList
  rts <- unique(spectra_data$rt)
  run <- newXMLNode("run", attrs = c(id = "run1", defaultInstrumentConfigurationRef = "IC"))
  spectrumList <- newXMLNode("spectrumList", attrs = c(count = as.character(length(rts))), parent = run)

  # Write the run and spectrumList to the file incrementally
  writeLines(saveXML(run, indent = TRUE, prefix = ""), con)

  # Initialize index
  index <- list()

  # Write mzML content and build index
  laplee(seq_along(rts), function(i) {
    scan_data <- spectra_data[rt == rts[i]]

    # Create a spectrum node
    spectrum <- newXMLNode("spectrum", attrs = c(id = paste0("scan=", i), index = as.character(i-1)))

    # Add cvParams
    newXMLNode("cvParam", attrs = c(cvRef = "MS", accession = "MS:1000511", name = "ms level", value = as.character(1)), parent = spectrum)
    newXMLNode("cvParam", attrs = c(cvRef = "MS", accession = "MS:1000127", name = "centroid spectrum"), parent = spectrum)

    # Add scanList and scan information
    scanList <- newXMLNode("scanList", attrs = c(count = "1"), parent = spectrum)
    scan <- newXMLNode("scan", parent = scanList)
    newXMLNode("cvParam", attrs = c(cvRef = "MS", accession = "MS:1000016", name = "scan start time",
                                    value = as.character(scan_data[[1,"rt"]]), unitCvRef = "UO", unitAccession = "UO:0000031", unitName = "minute"), parent = scan)

    # Add m/z and intensity arrays
    binaryDataArrayList <- newXMLNode("binaryDataArrayList", attrs = c(count = "2"), parent = spectrum)

    mzArray <- newXMLNode("binaryDataArray", parent = binaryDataArrayList)
    newXMLNode("cvParam", attrs = c(cvRef = "MS", accession = "MS:1000514", name = "m/z array", unitCvRef = "MS", unitAccession = "MS:1000040", unitName = "m/z"), parent = mzArray)
    newXMLNode("cvParam", attrs = c(cvRef = "MS", accession = "MS:1000523", name = "64-bit float"), parent = mzArray)
    newXMLNode("binary", base64enc::base64encode(writeBin(scan_data$mz, raw(), endian = "little")), parent = mzArray)

    intArray <- newXMLNode("binaryDataArray", parent = binaryDataArrayList)
    newXMLNode("cvParam", attrs = c(cvRef = "MS", accession = "MS:1000515", name = "intensity array", unitCvRef = "MS", unitAccession = "MS:1000131", unitName = "number of detector counts"), parent = intArray)
    newXMLNode("cvParam", attrs = c(cvRef = "MS", accession = "MS:1000523", name = "64-bit float"), parent = intArray)
    newXMLNode("binary", base64enc::base64encode(writeBin(scan_data$int, raw(), endian = "little")), parent = intArray)

    # Write the spectrum node to the file incrementally
    writeLines(saveXML(spectrum, indent = TRUE, prefix = ""), con)

    # Add to index
    offset <- seek(con)
    index[[paste0("scan=", i)]] <- offset
  })

  # Write the closing tags and index list
  writeLines('</spectrumList>\n</run>\n', con)

  indexListOffset <- seek(con)
  writeLines('<indexList count="1">\n <index name="spectrum">', con)
  for (id in names(index)) {
    writeLines(sprintf('  <offset idRef="%s">%d</offset>', id, index[[id]]), con)
  }
  writeLines(' </index>\n</indexList>', con)
  writeLines(sprintf('<indexListOffset>%d</indexListOffset>', indexListOffset), con)
  writeLines('</indexedmzML>', con)

  return(invisible(path_out))
}
