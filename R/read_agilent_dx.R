#' Read 'Agilent' DX files
#'
#' Reads 'Agilent' `.dx` files.
#'
#' The archive is extracted to `path_out`, or to a temporary directory that is
#' deleted afterwards, and its `.ch`, `.uv` and `.it` files are read with
#' [read_chemstation_ch], [read_chemstation_uv] and an internal reader for
#' `.it` files.
#'
#' The mass spectra of an MS injection are not in the archive. They are in the
#' `.MSScan.bin` and `.MSPeak.bin` files of the same name beside it. The first
#' is needed for `MS1`, `TIC` and `BPC`, the second for `MS1`. `MSScan.bin` holds one
#' record per scan, laid out as the archive's `MSData/MSScan.xsd` describes,
#' which gives the retention time, TIC and base peak of the scan and the
#' location of its spectrum in `MSPeak.bin`. Each spectrum there is the scan's
#' m/z values followed by as many intensities, as 32- or 64-bit floats or
#' 32-bit integers depending on the scan's spectrum format and size. Only scans
#' at MS level 1 are read into `MS1`, while the `TIC` and `BPC` cover every
#' scan.
#'
#' Where the archive holds an `injection.acmd` file, the `run_datetime` and
#' `sample_injection_volume` attributes are taken from it. Its run time
#' records the offset from UTC, while the `.ch` and `.uv` headers give only
#' the local time.
#'
#' @importFrom utils unzip
#' @inheritParams shared_params
#' @param path Path to Agilent `.dx` file.
#' @param what Whether to extract chromatograms (`chroms`), DAD data
#' (`dad`), auxiliary instrumental data (`instrument`), such as
#' temperature, pressure or solvent composition, and/or mass spectra (`MS1`)
#' with the total ion chromatogram (`TIC`) and base peak chromatogram (`BPC`).
#' Accepts multiple arguments, and defaults to `chroms` and `dad`. When more than one is requested, any the archive does not contain
#' are left out; a single one that is missing is an error.
#' @param path_out A directory to export unzipped files. If a path is not
#' specified, a temporary directory is used. The files are extracted into a
#' folder named for `path`, overwriting any files of the same name already
#' there.
#' @return A list with one element per type in `what`, each a chromatogram or
#' a list of chromatograms named by signal, in the format specified by
#' `format_out` and `data_format`. Mass spectra and the `BPC` are always in
#' long format, with `rt`, `mz` and `intensity` columns, and are returned as a
#' `data.table` when `format_out` is `matrix`. With `collapse = TRUE`, a list of one
#' element is replaced by that element.
#' @examples \dontrun{
#' read_agilent_dx(path)
#' }
#' @author Ethan Bass
#' @family 'Agilent' parsers
#' @export

read_agilent_dx <- function (path,  what = c("chroms", "dad"), path_out = NULL,
                             format_out = c("matrix", "data.frame", "data.table"),
                             data_format = c("wide", "long"), read_metadata = TRUE,
                             metadata_format = c("chromconverter", "raw"),
                             collapse = TRUE) {
  format_out <- check_format_out(format_out)
  data_format <- check_data_format(data_format, format_out)
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  what <- match.arg(what, c("chroms", "dad", "instrument", "MS1", "TIC", "BPC"),
                    several.ok = TRUE)
  files <- unzip(path, list = TRUE)
  acmd <- grep("^injection\\.acmd$", files$Name, ignore.case = TRUE,
               value = TRUE)
  ms_types <- c("MS1", "TIC", "BPC")
  ms_xml <- "^MSData/(MSScan\\.xsd|Contents\\.xml|Devices\\.xml|MSTS\\.xml)$"
  exts <- c(chroms = "\\.ch$", dad = "\\.uv$", instrument = "\\.it$",
            MS1 = ms_xml, TIC = ms_xml, BPC = ms_xml)
  files <- lapply(exts[what], function(ext){
    grep(ext, files$Name, ignore.case = TRUE, value = TRUE)
  })
  ms_bins <- paste0(fs::path_ext_remove(path), c(".MSScan.bin", ".MSPeak.bin"))
  ms_note <- sprintf(paste(" Mass spectra are read from %s and %s, which must be",
                           "in the same directory as the archive."),
                     sQuote(basename(ms_bins[1])), sQuote(basename(ms_bins[2])))
  if (!file.exists(ms_bins[1])){
    files[intersect(names(files), ms_types)] <- list(character())
  } else if (!file.exists(ms_bins[2])){
    files[intersect(names(files), "MS1")] <- list(character())
  }
  if (length(files) > 1){
    what <- what[vapply(files, length, FUN.VALUE = numeric(1)) > 0]
    if (length(what) == 0){
      stop("None of the requested types could be found in ", sQuote(path), ".",
           if (any(names(files) %in% ms_types)) ms_note, call. = FALSE)
    }
  }
  if (is.null(path_out)) {
    tmp <- temp_directory(path)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  } else {
    tmp <- fs::path_expand(path_out)
  }
  path_out <- fs::path(tmp, basename(path))
  fs::dir_create(path_out, force = TRUE)
  if (length(unlist(files)) > 0){
    unzip(path, files = unique(unlist(files)), exdir = path_out)
  }
  files.path <- lapply(files, function(fl){
    fs::path(path_out, fl)
  })
  acmd <- if (read_metadata && metadata_format == "chromconverter" &&
              length(acmd) > 0) read_acmd(unz(path, acmd[1]))
  if (any(what == "chroms")) {
    if (length(files.path$chroms) > 0){
      chroms <- lapply(files.path$chroms, read_chemstation_ch, format_out = format_out,
                       data_format = data_format, read_metadata = read_metadata,
                       metadata_format = metadata_format, source_file = path)
      chroms <- lapply(chroms, add_acmd_metadata, acmd = acmd)
      names(chroms) <- get_signal_names(chroms)
      chroms <- collapse_list(chroms)
    } else{
      stop("Trace data could not be found.")
    }
  }
  if (any(what == "dad")) {
    if (length(files.path$dad) > 0){
      dad <- read_chemstation_uv(files.path$dad, format_out = format_out,
                                 data_format = data_format,
                                 read_metadata = read_metadata,
                                 metadata_format = metadata_format,
                                 source_file = path)
      dad <- add_acmd_metadata(dad, acmd)
    } else{
      stop("DAD data could not be found.")
    }
  }
  if (any(what == "instrument")){
    if (length(files.path$instrument) > 0){
      instrument <- lapply(files.path$instrument, read_chemstation_it,
                           format_out = format_out, data_format = data_format,
                           read_metadata = read_metadata,
                           metadata_format = metadata_format, source_file = path)
      instrument <- lapply(instrument, add_acmd_metadata, acmd = acmd)
      names(instrument) <- get_signal_names(instrument)
      instrument <- collapse_list(instrument)
    } else{
      stop("Instrument data could not be found.")
    }
  }
  ms <- list()
  ms_what <- intersect(what, ms_types)
  if (length(ms_what) > 0){
    ms_files <- files.path[[ms_what[1]]]
    xsd <- grep("MSScan\\.xsd$", ms_files, ignore.case = TRUE, value = TRUE)
    if (length(xsd) == 0){
      stop("MS data could not be found.", ms_note, call. = FALSE)
    }
    ms <- read_agilent_ms(ms_bins[1], ms_bins[2], xsd[1], what = ms_what,
                          format_out = format_out, data_format = data_format,
                          read_metadata = read_metadata,
                          metadata_format = metadata_format, source_file = path)
    ms <- lapply(ms, add_acmd_metadata, acmd = acmd)
  }
  dat <- c(mget(setdiff(what, ms_types)), ms)[what]
  if (collapse){
    dat <- collapse_list(dat)
  }
  dat
}

#' Read the injection metadata of an 'OpenLab' `.dx` archive
#'
#' `injection.acmd` records the run time with its offset from UTC, which the
#' `.ch` and `.uv` headers leave out, and the injection volume, which they do
#' not record at all.
#' @param path Path to, or connection to, `injection.acmd`.
#' @return A named list of metadata fields, `NA` where the file has none.
#' @noRd
read_acmd <- function(path){
  acmd_metadata(read_acmd_fields(path))
}

#' Read the fields of `injection.acmd` that hold a single value
#' @return A named list of strings, `NA` where a field is empty.
#' @noRd
read_acmd_fields <- function(path){
  info <- xml2::xml_find_first(xml2::read_xml(path),
                               "./*[local-name()='InjectionInfo']")
  nodes <- xml2::xml_children(info)
  nodes <- nodes[xml2::xml_length(nodes) == 0]
  vals <- trimws(xml2::xml_text(nodes))
  vals[!nzchar(vals)] <- NA_character_
  stats::setNames(as.list(vals), xml2::xml_name(nodes))
}

#' Run time and injection volume from the fields of `injection.acmd`
#' @noRd
acmd_metadata <- function(fields){
  field <- function(name) fields[[name]] %||% NA_character_
  volume <- suppressWarnings(as.numeric(field("InjectionVolume")))
  if (!isTRUE(field("InjectionVolumeUnits") %in% c("\u00b5L", "\u03bcL", "uL"))){
    volume <- NA_real_
  }
  list(run_datetime = parse_iso8601(field("RunDateTime")),
       sample_injection_volume = volume)
}

#' Read the metadata of an 'OpenLab' `.dx` archive without its data
#'
#' Reads `injection.acmd` alone, which records the fields that describe the
#' whole injection. Those of the individual traces, such as `detector` or
#' `time_range`, are left out. An archive without `injection.acmd` is read in
#' full.
#' @noRd
read_agilent_dx_metadata <- function(path,
                                     metadata_format = c("chromconverter",
                                                         "raw")){
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  acmd <- grep("^injection\\.acmd$", unzip(path, list = TRUE)$Name,
               ignore.case = TRUE, value = TRUE)
  if (length(acmd) == 0){
    return(read_agilent_dx(path, metadata_format = metadata_format))
  }
  fields <- read_acmd_fields(unz(path, acmd[1]))
  attrs <- if (metadata_format == "raw"){
    list(metadata = fields)
  } else {
    c(list(sample_name = fields[["SampleName"]],
           sample_position = fields[["Location"]],
           operator = fields[["RunOperator"]],
           method = fields[["AcquisitionMethod"]]),
      acmd_metadata(fields))
  }
  attrs <- attrs[!vapply(attrs, function(v) length(v) == 1 && is.na(v),
                         logical(1))]
  finalize_metadata(matrix(numeric(0), 0, 0), attrs,
                    list(source_file = path, parser = "chromconverter"))
}

#' Attach `injection.acmd` metadata to a trace read from the archive
#' @noRd
add_acmd_metadata <- function(x, acmd){
  for (field in names(acmd)){
    if (!is.na(acmd[[field]])) attr(x, field) <- acmd[[field]]
  }
  x
}

#' Name traces by their signal descriptor
#'
#' Every 'ChemStation' version that records a signal descriptor (e.g.
#' `"RID1G,Board Temperature"`) now attaches it as `signal_descriptor`, so
#' there is one field to read regardless of version. Versions that do not
#' record one (8, 81) give `NA`.
#' @noRd
get_signal_names <- function(x){
  vapply(x, function(xx){
    nm <- attr(xx, "signal_descriptor")
    if (is.null(nm) || length(nm) == 0) NA_character_ else as.character(nm)[1]
  }, FUN.VALUE = character(1))
}

#' Read the mass spectra of an 'Agilent' MS acquisition with its metadata
#' @param scan_file,peak_file Paths to `MSScan.bin` and `MSPeak.bin`.
#' @param xsd Path to `MSScan.xsd`, which is beside the `Contents.xml`,
#' `Devices.xml` and `MSTS.xml` files the metadata are read from.
#' @param source_file Path to the file or directory the data were read from.
#' @return A named list of the streams in `what`.
#' @noRd
read_agilent_ms <- function(scan_file, peak_file, xsd, what, format_out,
                            data_format, read_metadata, metadata_format,
                            source_file){
  format_out <- check_format_out(format_out)
  data_format <- check_data_format(data_format, format_out)
  ms <- read_agilent_msdata(scan_file, peak_file, xsd, what = what,
                            format_out = format_out, data_format = data_format)
  if (!read_metadata){
    return(ms)
  }
  meta <- read_agilent_ms_metadata(dirname(xsd))
  meta$time_range <- attr(ms, "time_range")
  purrr::imap(ms, function(x, h){
    meta$polarity <- unique(attr(x, "scan_info")$polarity %||%
                              attr(ms, "polarity"))
    attach_metadata(x, meta,
                    format_in = check_metadata_format(metadata_format,
                                                      "agilent_ms"),
                    format_out = if (h == "TIC") format_out else
                      check_format_out_table(format_out),
                    data_format = if (h == "TIC") data_format else "long",
                    parser = "chromconverter", source_file = source_file,
                    source_file_format = "agilent_ms")
  })
}

#' Read the mass spectra of an 'Agilent' MS acquisition
#'
#' `MS2` holds the product ion and MRM scans, with the scan's `MzOfInterest`
#' as `precursor_mz`. Neutral loss and precursor ion scans are skipped. Each
#' table of spectra has a `scan_info` attribute with one row per scan. Its
#' `collision_energy` is the absolute value of the stored one, which is
#' negative in negative ion mode.
#' @param scan_file,peak_file Paths to `MSScan.bin` and `MSPeak.bin`.
#' @param xsd Path to `MSScan.xsd`.
#' @return A named list of the streams in `what`, with the range of their
#' retention times as its `time_range` attribute.
#' @noRd
read_agilent_msdata <- function(scan_file, peak_file, xsd, what,
                                format_out, data_format){
  scans <- read_msscan_records(scan_file, xsd)
  rt <- scans$ScanTime
  polarity <- rep_len(c("positive", "negative")[
    match(scans$IonPolarity %||% NA, 0:1)], nrow(scans))
  levels <- intersect(c("MS1", "MS2"), what)
  if (length(levels) > 0){
    tandem <- scans$MSLevel == 2L &
      (scans$ScanType %||% NA) %in% c(256L, 512L)
    keep <- list(MS1 = scans$MSLevel == 1L, MS2 = tandem)[levels]
    found <- vapply(keep, any, logical(1))
    if (!any(found)){
      stop(errorCondition(sprintf("The file holds no %s scans that can be read.",
                                  paste(levels, collapse = " or ")),
                          class = "agilent_ms_absent", call = NULL))
    }
    unread <- sum(!(scans$MSLevel == 1L | tandem))
    if (unread > 0 && "MS2" %in% levels){
      warning("Skipped ", unread, " scans that are neither MS1, product ion ",
              "nor MRM, which cannot be read yet.", call. = FALSE)
    }
    what <- setdiff(what, levels[!found])
    spectra <- Map(function(k, lvl){
      sp <- scans[k, , drop = FALSE]
      x <- read_mspeak_spectra(sp, peak_file)
      info <- data.frame(scan = sp$ScanID, rt = sp$ScanTime,
                         ms_level = as.integer(sp$MSLevel),
                         polarity = polarity[k])
      if (lvl == "MS2"){
        x <- cbind(x[, "rt", drop = FALSE],
                   precursor_mz = rep.int(sp$MzOfInterest, sp$PointCount),
                   x[, c("mz", "intensity"), drop = FALSE])
        info$precursor_mz <- sp$MzOfInterest
        info$collision_energy <- abs(sp$CollisionEnergy %||% NA_real_)
      }
      structure(x, scan_info = info)
    }, keep[found], levels[found])
  }
  if (any(what == "BPC")){
    BPC <- cbind(rt = rt, mz = scans$BasePeakMZ, intensity = scans$BasePeakValue)
  }
  if (any(what == "TIC")){
    TIC <- format_2d_chromatogram(rt = rt, int = scans$TIC,
                                  data_format = data_format,
                                  format_out = format_out)
  }
  dat <- purrr::imap(c(if (length(levels) > 0) spectra,
                       mget(setdiff(what, levels)))[what], function(x, h){
    if (h == "TIC") return(x)
    y <- convert_chrom_format(x, format_out = check_format_out_table(format_out),
                              data_format = "long")
    attr(y, "scan_info") <- attr(x, "scan_info")
    y
  })
  structure(dat, time_range = if (length(rt) > 0) range(rt) else NA,
            polarity = unique(polarity))
}

#' Read the spectra of a set of 'Agilent' MS scans
#'
#' The types of a scan's values in `MSPeak.bin` depend on its spectrum format
#' and bytes per point. Scans with spectrum format 2 are profiles, read from
#' the `MSProfile.bin` beside it. Each profile starts with its first m/z and
#' the step between points, as two doubles or two 32-bit floats, followed by
#' the intensities as 32-bit floats or integers respectively. The step is
#' taken from the scan's `SamplingPeriod`, since the stored one can be
#' slightly smaller. At 8 bytes per point, spectrum format 1 holds 32-bit
#' float intensities in MS1 scans but 32-bit integers in MRM scans.
#' @param sp The scan records to read, as returned by `read_msscan_records`.
#' @param peak_file Path to `MSPeak.bin`.
#' @return A matrix with `rt`, `mz` and `intensity` columns.
#' @noRd
read_mspeak_spectra <- function(sp, peak_file){
  k <- sp$PointCount
  profile <- sp$SpectrumFormatID == 2L
  header <- sp$ByteCount - 4 * k
  profile_file <- sub("MSPeak\\.bin$", "MSProfile.bin", peak_file)
  profile_error <- ms_format_error(paste(
    "The spectra are stored as profiles ('MSProfile.bin') in a layout",
    "that cannot be read yet."))
  if (any(profile) && (!file.exists(profile_file) || is.null(sp$SamplingPeriod) ||
                       !all(header[profile & k > 0] %in% c(8, 16)))){
    stop(profile_error)
  }
  if (any(!profile)){
    if (!file.exists(peak_file)){
      stop(sprintf("'%s' could not be found.", basename(peak_file)),
           call. = FALSE)
    }
    key <- paste(sp$SpectrumFormatID, ifelse(k > 0, sp$ByteCount / k, 8),
                 sep = ":")
    sizes <- mspeak_types[key]
    sizes[key == "1:8" & (sp$ScanType %||% NA) %in% 256L] <- list(c(4, -4))
    if (any(vapply(sizes[!profile], is.null, logical(1)))){
      stop(ms_format_error(
        "This 'MSPeak.bin' stores its spectra in a format that cannot be read yet."))
    }
  }
  files <- ifelse(profile, profile_file, peak_file)
  beyond <- sp$SpectrumOffset < 0 |
    sp$SpectrumOffset + sp$ByteCount > file.size(files)
  if (any(beyond)){
    stop(ms_format_error(sprintf("'MSScan.bin' points beyond the end of '%s'.",
                                 basename(files[which(beyond)[1]]))))
  }
  cons <- lapply(setNames(nm = unique(files)), file, open = "rb")
  on.exit(lapply(cons, close))
  mz <- intensity <- vector("list", length(k))
  for (i in seq_along(k)){
    if (k[i] == 0) next
    con <- cons[[files[i]]]
    seek(con, sp$SpectrumOffset[i])
    if (profile[i]){
      axis <- readBin(con, "double", n = 2, size = header[i] / 2,
                      endian = "little")
      if (abs(axis[2] - sp$SamplingPeriod[i]) > 1e-3 * sp$SamplingPeriod[i]){
        stop(profile_error)
      }
      mz[[i]] <- axis[1] + sp$SamplingPeriod[i] * (seq_len(k[i]) - 1)
      intensity[[i]] <- readBin(con, if (header[i] == 8) "integer" else "double",
                                n = k[i], size = 4, endian = "little")
    } else {
      mz[[i]] <- readBin(con, "double", n = k[i], size = sizes[[i]][1],
                         endian = "little")
      intensity[[i]] <- readBin(con, if (sizes[[i]][2] < 0) "integer" else "double",
                                n = k[i], size = abs(sizes[[i]][2]),
                                endian = "little")
    }
  }
  cbind(rt = rep.int(sp$ScanTime, k), mz = unlist(mz),
        intensity = as.numeric(unlist(intensity)))
}

#' Error for an 'Agilent' MS layout that cannot be read
#'
#' Classed so that `read_agilent_d` can point to 'rainbow', which reads the
#' 'MassHunter' layouts these errors come from but not the 'OpenLab' ones.
#' @noRd
ms_format_error <- function(message){
  errorCondition(message, class = "agilent_ms_format_error", call = NULL)
}

#' Sizes in bytes of the m/z and intensity values in 'MSPeak.bin'
#'
#' Keyed by `SpectrumFormatID` and bytes per point. A negative size is an
#' integer; the others are floats.
#' @noRd
mspeak_types <- list("1:8" = c(4, 4), "1:16" = c(8, 8),
                     "3:8" = c(4, -4), "3:12" = c(8, 4))

#' Read the scan records of 'MSScan.bin'
#'
#' The record layout is read from `MSScan.xsd`. The records start at the
#' offset given by the 32-bit integer at byte 88.
#' @param scan_file Path to `MSScan.bin`.
#' @param xsd Path to `MSScan.xsd`.
#' @return A data.frame with one row per scan and one column per field.
#' @noRd
read_msscan_records <- function(scan_file, xsd){
  layout <- read_msscan_layout(xsd)
  fields <- c("ScanID", "ScanTime", "MSLevel", "TIC", "BasePeakMZ",
              "BasePeakValue", "SpectrumFormatID", "SpectrumOffset", "ByteCount",
              "PointCount")
  if (!all(fields %in% layout$name)){
    stop(ms_format_error(paste(
      "'MSScan.xsd' describes scan records without",
      paste0(paste(sQuote(setdiff(fields, layout$name)), collapse = ", "), "."))))
  }
  rec_size <- sum(layout$size)
  f <- file(scan_file, "rb")
  on.exit(close(f))
  seek(f, 88)
  start <- readBin(f, "integer", size = 4, endian = "little")
  n_bytes <- file.size(scan_file) - start
  if (n_bytes %% rec_size != 0){
    stop(ms_format_error(
      "'MSScan.bin' does not match the record layout in 'MSScan.xsd'."))
  }
  seek(f, start)
  rec <- matrix(readBin(f, "raw", n = n_bytes), nrow = rec_size)
  n_scans <- ncol(rec)
  scans <- lapply(seq_len(nrow(layout)), function(i){
    bytes <- as.vector(rec[layout$offset[i] + seq_len(layout$size[i]), ,
                           drop = FALSE])
    switch(layout$type[i],
           "xs:double" = readBin(bytes, "double", size = 8, n = n_scans,
                                 endian = "little"),
           "xs:float" = readBin(bytes, "double", size = 4, n = n_scans,
                                endian = "little"),
           "xs:long" = {
             w <- matrix(readBin(bytes, "integer", size = 2, signed = FALSE,
                                 n = 4 * n_scans, endian = "little"), nrow = 4)
             colSums(w * 65536^(0:3))
           },
           readBin(bytes, "integer", size = layout$size[i], n = n_scans,
                   endian = "little"))
  })
  scans <- as.data.frame(setNames(scans, layout$name))
  if (anyNA(scans$ScanID) || is.unsorted(scans$ScanID, strictly = TRUE) ||
      anyNA(scans$ScanTime) || is.unsorted(scans$ScanTime)){
    stop(ms_format_error(
      "'MSScan.bin' does not match the record layout in 'MSScan.xsd'."))
  }
  scans
}

#' Record layout of 'MSScan.bin'
#' @param xsd Path to `MSScan.xsd`.
#' @return A data.frame of the record's fields in order, with their `name`,
#' `type`, `size` in bytes and `offset` from the start of the record.
#' @noRd
read_msscan_layout <- function(xsd){
  doc <- xml2::read_xml(xsd)
  sizes <- c("xs:byte" = 1, "xs:short" = 2, "xs:int" = 4, "xs:float" = 4,
             "xs:long" = 8, "xs:double" = 8)
  flatten <- function(type){
    els <- xml2::xml_find_all(doc, sprintf(paste0(
      "//*[local-name()='complexType'][@name='%s']",
      "/*[local-name()='sequence']/*[local-name()='element']"), type))
    if (length(els) == 0){
      stop("'MSScan.xsd' uses a type that cannot be read: ", type)
    }
    do.call(rbind, lapply(els, function(el){
      el_type <- xml2::xml_attr(el, "type")
      if (el_type %in% names(sizes)){
        data.frame(name = xml2::xml_attr(el, "name"), type = el_type)
      } else flatten(sub("^[^:]*:", "", el_type))
    }))
  }
  layout <- flatten("ScanRecordType")
  layout$size <- unname(sizes[layout$type])
  layout$offset <- cumsum(c(0, head(layout$size, -1)))
  layout
}

#' Read the metadata files of an 'Agilent' MS acquisition
#' @param dir Directory holding `Contents.xml`, `Devices.xml`, `MSTS.xml` and,
#' for 'MassHunter', `sample_info.xml`.
#' @return A named list of their fields, with the total number of scans from
#' `MSTS.xml` and the sample fields of `sample_info.xml` as `sample_info`.
#' @noRd
read_agilent_ms_metadata <- function(dir){
  read_xml_if <- function(file){
    file <- fs::path(dir, file)
    if (file.exists(file)) xml2::read_xml(file)
  }
  leaves <- function(doc, xpath){
    if (is.null(doc)) return(list())
    nodes <- xml2::xml_children(xml2::xml_find_first(doc, xpath))
    setNames(as.list(xml2::xml_text(nodes)), xml2::xml_name(nodes))
  }
  meta <- c(leaves(read_xml_if("Contents.xml"), "/Contents"),
            leaves(read_xml_if("Devices.xml"), "/Devices/Device"))
  msts <- read_xml_if("MSTS.xml")
  if (!is.null(msts)){
    meta$NumOfScans <- sum(as.numeric(xml2::xml_text(
      xml2::xml_find_all(msts, "//TimeSegment/NumOfScans"))))
  }
  info <- read_xml_if("sample_info.xml")
  if (!is.null(info)){
    fields <- xml2::xml_find_all(info, "//Field")
    meta$sample_info <- setNames(
      as.list(trimws(xml2::xml_text(xml2::xml_find_first(fields, "./Value")))),
      xml2::xml_text(xml2::xml_find_first(fields, "./Name")))
  }
  meta
}
