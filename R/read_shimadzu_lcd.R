#' Read 'Shimadzu' LCD
#'
#' Read PDA, chromatogram, mass spectrometry and peak table streams from
#' 'Shimadzu' `.lcd` files.
#'
#' A parser to read data from 'Shimadzu' `.lcd` files. LCD files are
#' encoded as 'Microsoft' OLE documents. The parser relies on the
#' [olefile](https://pypi.org/project/olefile/) package in Python to unpack the
#' files. Each detector writes its own storage, and this function dispatches
#' over them:
#'
#' * **PDA** (`PDA 3D Raw Data:3D Raw Data`), requested as `DAD`: one
#' delta-encoded segment per retention time, each holding a full spectrum. Read by [read_sz_lcd_3d()],
#' which documents the segment header and the delta encoding.
#' * **Chromatograms** (`LSS Raw Data:Chromatogram Ch<#>`): one stream per
#' channel, delta-encoded in the same way. Read by [read_sz_lcd_2d()].
#' * **Quadrupole time-of-flight mass spectra** (`QTFL RawData`): centroided
#' scans stored as a scan header, a block of flight times and a block of
#' intensities, with the mass axis reconstructed from the calibration table in
#' the file. Read by [read_sz_qtof()], which documents the scan header, the
#' flight-time conversion and the intensity scaling. The total ion current is
#' held separately, in `Centroid SumTIC` ([read_sz_qtof_tic()]).
#' * **Triple quadrupole mass spectra** (`TLM Raw Data`): zlib-compressed scan
#' records located through a spectrum index, covering full scan, product-ion
#' scan, MRM and SIM acquisitions. Read by [read_sz_tlm()], which documents the
#' index, the scan header and the layout of each scan type. The total ion
#' current is again held separately, in `TIC Data`.
#' * **Peak tables** (`Peak Table`): integration results as reported by
#' 'Lab Solutions', one stream per channel. Read by [read_sz_tables()], which
#' documents the two record layouts.
#'
#' The two mass spectrometry containers are mutually exclusive: a file holds
#' one or the other, according to the instrument that wrote it.
#'
#' A mass spectrometry run is divided into **acquisition events**: the scan
#' functions defined by the method, each with its own polarity, MS level and
#' mass range, which the instrument cycles through as the run proceeds. The
#' mass spectrometry streams are reported per event --- `TIC` returns one
#' chromatogram for each, and the `scan_info` attribute of a table of spectra
#' names the event every scan came from.
#'
#' @inheritParams shared_params
#' @param path Path to 'Shimadzu' `.lcd` file.
#' @param what What stream to get: current options are `DAD` (for which `PDA`
#' is accepted as a synonym, since that is what 'Shimadzu' calls the same
#' detector), chromatograms (`chroms`), `TIC`, mass spectra (`MS1`, `MS2`, or
#' `MS` for both), and/or peak lists (`peak_table`). Note that an MRM or SIM acquisition is a single
#' stage of mass selection either way: MRM scans are `MS2`, and SIM scans,
#' whose Q1 (first quadrupole) and Q3 (third quadrupole) are the same, are `MS1`.
#'
#' If a stream is not specified, the richest one the file contains is returned:
#' `PDA` if there is a PDA stream, otherwise `chroms`, and otherwise `MS` for a
#' file whose only detector is the mass spectrometer. The mass spectrometry
#' streams are read from whichever container the file uses: `QTFL RawData`
#' (centroided quadrupole time-of-flight data) or `TLM Raw Data` (triple
#' quadrupole full scan, product-ion scan, MRM and SIM data).
#' @param sparse Logical. Whether to return mass spectra in sparse format
#' (excluding zeros), as [call_rainbow()] does. Defaults to `TRUE`. Applies
#' only to triple quadrupole profile spectra, whose m/z grid is largely
#' empty; ignored for every other stream.
#' @author Ethan Bass
#' @return A chromatogram or list of chromatograms in the format specified by
#' `data_format` and `format_out`. If `data_format` is `wide`, the
#' chromatogram(s) will be returned with retention times as rows and a
#' single column for the intensity. If `long` format is requested, two
#' columns will be returned: one for the retention time and one for the intensity.
#' The `format_out` argument determines whether chromatograms are returned
#' in `matrix`, `data.frame`, or `data.table` format. Metadata will be
#' attached to the chromatogram as [attributes] when `read_metadata` is `TRUE`.
#' @note Times are stored as a 'Windows' `FILETIME`, which is always UTC, so
#' `run_datetime` is reported in UTC. The files also record the offset of the
#' local time zone (e.g. `+01'00'`), but this is the standard offset of the
#' zone rather than the offset that was in force, and it seems that no daylight
#' saving information is stored anywhere in the file. The local times displayed by
#' 'Lab Solutions' therefore cannot be reconstructed from the recorded offset
#' alone: where daylight saving time applied, they are an hour ahead of it.
#' Rendering `run_datetime` in the zone where the data were acquired, e.g.
#' `format(attr(x, "run_datetime"), tz = "Europe/Paris")`, recovers them
#' exactly.
#'
#' As of `v0.10.0`, 2D chromatograms are scaled by the calibration factor and
#' the value factor recorded for each channel, so their intensities match those
#' reported by 'Lab Solutions'. An absorbance axis can be reported in `uAU`,
#' `mAU` or `AU`, and the file records the size of each as a **value factor**:
#' `1`, `1000` and `1e6`, since one `mAU` is a thousand `uAU` and one `AU` a
#' million. The smallest of them is the **base unit**, while 'Lab Solutions'
#' displays the data in whichever unit the method selected, usually `mAU`.
#' Other detectors work the same way: a refractive index axis measures in `nRI`
#' and displays `uRI`.
#'
#' Two factors separate the stored integers from the displayed value. The
#' calibration factor converts an integer into base units, and the value factor
#' converts base units into the displayed unit; an intensity as 'Lab Solutions'
#' reports it is the integer times the one divided by the other. The
#' calibration factor is `1` on some channels and not on others (~42 for an
#' SPD-20A, ~310 for an RID-10A), which suggests the integers are detector
#' counts whose size varies by module, though the file does not say so.
#'
#' For a 2D chromatogram, `scale = TRUE` applies both factors. `scale = FALSE`
#' returns the stored integers, and reports them as `uAU` only where the
#' calibration factor is `1`; where it is not, the integers are in no unit we
#' can name, so the displayed unit is left in place rather than claiming one
#' they are not in. Either way the values and the `detector_y_unit` attribute
#' agree.
#'
#' PDA data is instead returned as it is encoded in the file, matching the
#' `[PDA 3D]` section of a 'Lab Solutions' ASCII export, which declares no
#' intensity unit or multiplier. The `3D Data Item` describes the absorbance
#' axis in `mAU` with a value factor of `1000`, which would imply scaling the
#' values by `0.001`, but the `[PDA Multi Chromatogram]` traces in the ASCII
#' export, which are extracted from the same data, report values on the same
#' scale as the raw data with a multiplier of `1`. Until this can be resolved,
#' PDA data is left unscaled and the `scale` argument is ignored.
#'
#' `Max Plot` is the maximum absorbance over the wavelength range at each point
#' in time, so the value at one time can come from a different wavelength than
#' the value at the next, and no single wavelength describes the trace. It is
#' derived from the PDA data but is read as a 2D chromatogram, so it is scaled
#' and currently differs from the PDA data by a factor of `1000`. The
#' wavelength range it was taken over is not recorded in the `2D Data Item`,
#' whose nanometre axis spans `0` to `0`, so its `wavelength` attribute is `NA`
#' rather than the acquisition range of the PDA stream.
#' @examples \dontrun{
#' read_shimadzu_lcd(path)
#' }
#' @family 'Shimadzu' parsers
#' @export

read_shimadzu_lcd <- function(path, what, format_out = c("matrix", "data.frame",
                                                         "data.table"),
                                data_format = c("wide", "long"),
                                read_metadata = TRUE,
                                metadata_format = c("chromconverter", "raw"),
                                scale = TRUE, sparse = TRUE,
                                collapse = TRUE){
  format_out <- check_format_out(format_out)
  data_format <- check_data_format(data_format, format_out)
  metadata_format <- check_metadata_format(metadata_format, "shimadzu_lcd")

  ms_format <- NULL
  if (missing(what)){
    # Fall back through the streams a file may hold, preferring the richest one
    # it has: a file with both a PDA and 2D chromatograms returns the PDA data,
    # so a file whose only detector is the mass spectrometer returns its
    # spectra. Without this a mass-spectrometry-only file --- an MRM run, or a
    # QTOF acquisition with no PDA --- resolves to `chroms` and fails.
    # `check_streams(what = "tic")` looks for the LC TIC rather than the one in
    # the mass spectrometry container, so that is detected separately.
    what <- if (check_streams(path, "pda", boolean = TRUE)){
      "DAD"
    } else if (check_streams(path, "chroms", boolean = TRUE)){
      "chroms"
    } else {
      ms_format <- get_sz_ms_format(path)
      if (is.na(ms_format)) "chroms" else "MS"
    }
  }
  if (any(tolower(what) == "chromatogram")){
    warning("The `chromatogram` argument to `what` is deprecated. Please use `chroms` instead.")
    what[which(tolower(what) == "chromatogram")] <- "chroms"
  }
  what <- sz_match_streams(what)
  # `MS` is both levels; either can also be asked for on its own
  levels <- if (any(what == "MS")){
    c("MS1", "MS2")
  } else intersect(c("MS1", "MS2"), what)

  check_py_module("olefile")
  if (any(what == "chroms")){
    chroms <- read_sz_lcd_2d(path, format_out = format_out,
                             data_format = data_format,
                             read_metadata = read_metadata,
                             metadata_format = metadata_format,
                             scale = scale)
  }
  if (any(what == "DAD")){
    pda <- read_sz_lcd_3d(path, format_out = format_out,
                             data_format = data_format,
                             read_metadata = read_metadata,
                             metadata_format = metadata_format,
                             scale = scale)
  }
  if ((any(what == "TIC") || length(levels) > 0) && is.null(ms_format)){
    ms_format <- get_sz_ms_format(path)
  }
  if (any(what == "TIC")){
    # both containers are named explicitly, so a file with neither fails here
    # the same way it does when the spectra are asked for
    tic <- if (identical(ms_format, "tlm")){
      read_sz_tlm_tic(path, format_out = format_out,
                      data_format = data_format,
                      read_metadata = read_metadata,
                      metadata_format = metadata_format)
    } else if (identical(ms_format, "qtof")){
      read_sz_qtof_tic(path, format_out = format_out,
                       data_format = data_format,
                       read_metadata = read_metadata,
                       metadata_format = metadata_format)
    } else {
      stop("A mass spectrometry stream could not be detected.")
    }
  }
  if (length(levels) > 0){
    ms <- if (identical(ms_format, "tlm")){
      read_sz_tlm(path, format_out = format_out,
                  data_format = data_format, levels = levels,
                  sparse = sparse,
                  read_metadata = read_metadata,
                  metadata_format = metadata_format)
    } else if (identical(ms_format, "qtof")){
      read_sz_qtof(path, format_out = format_out,
                   data_format = data_format, levels = levels,
                   read_metadata = read_metadata,
                   metadata_format = metadata_format,
                   scale = scale)
    } else {
      stop("A mass spectrometry stream could not be detected.")
    }
    # a level the caller named but the file does not hold, as distinct from
    # `MS`, which is a request for whichever levels are there
    absent <- setdiff(intersect(what, c("MS1", "MS2")), names(ms))
    if (length(absent) > 0){
      warning(sprintf("%s data not found.",
                      paste(absent, collapse = " and ")), call. = FALSE)
    }
  }
  if (any(what == "peak_table")){
    peak_table <- read_sz_tables(path, format_out = format_out)
  }
  dat <- list()
  if (any(what == "DAD")) dat$DAD <- pda
  if (any(what == "chroms")) dat$chroms <- chroms
  if (length(levels) > 0) dat <- c(dat, ms)
  if (any(what == "TIC")) dat$TIC <- tic
  if (any(what == "peak_table")) dat$peak_table <- peak_table
  null <- vapply(dat, is.null, logical(1))
  if (any(null)) dat <- dat[!null]
  if (collapse) dat <- collapse_list(dat)
  dat
}

#' Read 'Shimadzu' LCD 3D data
#'
#' Reads 3D PDA data stream from 'Shimadzu' `.lcd` files.
#'
#' A parser to read PDA data from 'Shimadzu' `.lcd` files. LCD files are
#' encoded as 'Microsoft' OLE documents. The parser relies on the
#' [olefile](https://pypi.org/project/olefile/) package in Python to unpack the
#' files. The PDA data is encoded in a stream called `PDA 3D Raw Data:3D Raw Data`.
#' The PDA data stream contains a segment for each retention time, beginning
#' with a 24-byte header.
#'
#' The 24 byte header consists of the following fields:
#' * 4 bytes: segment label (`17234`).
#' * 4 bytes: Little-endian integer specifying the sampling rate along the
#' spectral axis (?), where the equivalent field of a 2D stream gives the
#' sampling rate along the time axis.
#' * 4 bytes: Little-endian integer specifying the number of wavelength values
#' in the segment.
#' * 4 bytes: Little-endian integer specifying the total number of bytes in the segment.
#' * 8 bytes of `00`s
#'
#' Each segment is divided into two sub-segments, which begin and end with an
#' integer specifying the length of the sub-segment in bytes. All known values
#' in this data stream are little-endian and the data are delta-encoded. The
#' first hexadecimal digit of each value is a sign digit
#' specifying the number of bytes in the delta and whether the value is positive
#' or negative. The sign digit represents the number of hexadecimal digits used
#' to encode each value. Even numbered sign digits correspond to positive deltas,
#' whereas odd numbers indicate negative deltas. Positive values are encoded as
#' little-endian integers, while negative values are encoded as two's
#' complements. The value at each position is derived by subtracting the delta
#' at each position from the previous value.
#'
#' @param path Path to 'Shimadzu' `.lcd` 3D data file.
#' @param format_out Class of output. Either `matrix`, `data.frame`, or
#' `data.table`.
#' @param data_format Either `wide` (default) or `long`.
#' @param read_metadata Logical. Whether to attach metadata.
#' @param metadata_format Format to output metadata. Either `chromconverter`
#' or `raw`.
#' @param scale This argument currently has no effect. PDA data is returned as
#' it is encoded in the file; see the note in [read_shimadzu_lcd()].
#' @examples \dontrun{
#' read_sz_lcd_3d("path/to/file.lcd")
#' }
#' @author Ethan Bass
#' @return A 3D chromatogram from the PDA stream in `matrix`, `data.frame`, or
#' `data.table` format, according to the value of `format_out`.
#' The chromatograms will be returned in `wide` or `long` format according to
#' the value of `data_format`.
#' @family 'Shimadzu' parsers
#' @keywords internal

read_sz_lcd_3d <- function(path, format_out = "matrix",
                            data_format = "wide",
                            read_metadata = TRUE,
                            metadata_format = "shimadzu_lcd",
                            scale = TRUE){
  check_streams(path, what = "pda")

  # read wavelengths from "Wavelength Table" stream
  lambdas <- read_sz_wavelengths(path)
  n_lambdas <- length(lambdas)

  # read data from "3D Raw Data" stream
  dat <- read_sz_pda(path, n_lambdas = n_lambdas)
  colnames(dat) <- lambdas

  data_item_exists <- check_stream(path, c('PDA 3D Raw Data', '3D Data Item'))
  if (data_item_exists){
    DI <- read_sz_3DDI(path)
    times <- seq(DI$DLT, DI$AT, by = DI$Rate)
    if (length(times) != nrow(dat)){
      times <- seq(DI$DLT, DI$AT, length.out = nrow(dat))
      warning("Length of the inferred time axis does not match the number of rows
            in the data.")
    }
    if (inherits(times, "numeric")){
      rownames(dat) <- times
    }
  } else{
    DI <- data.frame(DETN = NA, DSCN = NA, ADN = NA, detector.unit = NA)
  }
  if (data_format == "long"){
    dat <- reshape_chrom(dat, data_format = "long")
  }
  dat <- convert_chrom_format(dat, format_out = format_out,
                              data_format = data_format)
  if (read_metadata){
    meta <- read_sz_file_properties(path)
    meta <- c(meta, DI)
    dat <- attach_metadata(dat, meta, format_in = metadata_format,
                           source_file = path, data_format = data_format,
                           format_out = format_out,
                           source_file_format = "shimadzu_lcd")
  }
  dat
}

#' Read 'Shimadzu' LCD 2D data
#'
#' Reads 2D PDA data stream from 'Shimadzu' `.lcd` files.
#'
#' A parser to read chromatogram data streams from 'Shimadzu' `.lcd` files.
#' LCD files are encoded as 'Microsoft' OLE documents. The parser relies on the
#' [olefile](https://pypi.org/project/olefile/) package in Python to unpack the
#' files. The chromatogram data is encoded in streams titled
#' `LSS Raw Data:Chromatogram Ch<#>`. The chromatogram data streams begin
#' with a 24-byte header.
#'
#' The 24 byte header consists of the following fields:
#' * 4 bytes: segment label (`17234`).
#' * 4 bytes: Little-endian integer specifying the sampling rate (in milliseconds).
#' * 4 bytes: Little-endian integer specifying the number of values
#' in the file.
#' * 4 bytes: Little-endian integer specifying the total number of bytes
#' in the file.
#' * 8 bytes of `00`s
#'
#' Each segment is divided into multiple sub-segments, which begin and end with an
#' integer specifying the length of the sub-segment in bytes. All known values
#' in this data stream are little-endian and the data are delta-encoded. The
#' first hexadecimal digit of each value is a sign digit
#' specifying the number of bytes in the delta and whether the value is positive
#' or negative. The sign digit represents the number of hexadecimal digits used
#' to encode each value. Even numbered sign digits correspond to positive deltas,
#' whereas odd numbers indicate negative deltas. Positive values are encoded as
#' little-endian integers, while negative values are encoded as two's
#' complements. The value at each position is derived by subtracting the delta
#' at each position from the previous value.
#'
#' @param path Path to 'Shimadzu' `.lcd` 2D data file.
#' @param format_out Matrix or data.frame.
#' @param data_format Either `wide` (default) or `long`.
#' @param read_metadata Logical. Whether to attach metadata.
#' @param metadata_format Format to output metadata. Either `chromconverter` or
#' `raw`.
#' @param scale Whether to scale the data by the calibration factor and the
#' value factor, converting the encoded integers into the unit reported by
#' 'Lab Solutions' (e.g. `mV`).
#' @examples \dontrun{
#' read_sz_lcd_2d("path/to/file.lcd")
#' }
#' @author Ethan Bass
#' @return One or more 2D chromatograms from the chromatogram streams in
#' `matrix` or `data.frame` format, according to the value of
#' `format_out. If multiple chromatograms are found, they will be returned
#' as a list of matrices or data.frames. The chromatograms will be returned in
#' `wide or `long format according to the value of `data_format`.
#' @family 'Shimadzu' parsers
#' @keywords internal

read_sz_lcd_2d <- function(path, format_out = "data.frame",
                            data_format = "wide",
                            read_metadata = TRUE,
                            metadata_format = "shimadzu_lcd",
                            scale = TRUE){
  if (data_format == "long" && format_out == "matrix"){
    format_out <- "data.frame"
  }
  existing_streams <- check_streams(path, what = "chroms")
  if (length(existing_streams) == 0){
    stop("Chromatogram streams not detected.")
  }

  if (read_metadata){
    meta <- read_sz_file_properties(path)
  }

  dat <- lapply(existing_streams, function(stream){
    dat <- read_sz_chrom(path, stream = stream)
    idx <- ifelse(stream[2] == "Max Plot", "PDA",
                  as.numeric(gsub("\\D", "", stream[2])))
    status <- read_sz_chrom_status(path, stream)
    cf <- if (is.null(status) || is.na(status$CF)) 1 else status$CF

    data_item_exists <- check_stream(path,c('LSS Data Processing', '2D Data Item'))
    if (data_item_exists){
      DI <- read_sz_2DDI(path, idx = idx)
      times <- seq(DI$DLT, DI$AT, length.out = nrow(dat))
      rownames(dat) <- times
      multiplier <- DI$detector.vf
    } else{
      # files written by 'LCsolution' have no data item, but the status record
      # accompanying the raw data carries the same factors and the same unit
      DI <- data.frame(DETN = stream[[2]], DSCN = NA, ADN = NA,
                       detector.unit = if (is.null(status)){
                         NA_character_
                       } else status$unit)
      times <- as.numeric(rownames(dat))
      multiplier <- if (is.null(status) || is.na(status$VF)){
        NA_real_
      } else 1/status$VF
    }
    scaled <- scale && length(multiplier) == 1 && !is.na(multiplier)
    if (scaled){
      dat <- dat*multiplier*cf
    } else{
      DI$detector.unit <- sz_stored_unit(DI, cf = cf)
    }
    if (data_format == "long"){
      dat <- data.frame(rt = times, intensity = dat$int, detector = DI$DETN,
                   channel = DI$DSCN, lambda = DI$ADN,
                   unit = DI$detector.unit)
    }
    dat <- convert_chrom_format(dat, format_out = format_out,
                                data_format = data_format)
    if (read_metadata){
      dat <- attach_metadata(dat, c(meta, DI), format_in = metadata_format,
                             source_file = path, data_format = data_format,
                             format_out = format_out,
                             scale = scaled,
                             source_file_format = "shimadzu_lcd")
    }
    dat
  })
  if (!is.null(attr(dat[[1]],"detector")) |
      !is.null(attr(dat[[1]], "wavelength"))){
    names(dat) <- vapply(dat, function(x){
      det <- gsub("Detector ", "", attr(x, "detector"))
      wv <- attr(x, "wavelength")
      if (length(det) != 1 || is.na(det)){
        return(NA_character_)
      }
      if (length(wv) != 1 || is.na(wv) || !nzchar(wv)){
        det
      } else{
        paste(det, wv, sep = ", ")
      }
    }, FUN.VALUE = character(1))
  } else{
    names(dat) <-  sapply(existing_streams, "[", 2)
  }

  if (data_format == "long"){
    dat <- do.call(rbind, c(dat, make.row.names = FALSE))
  }
  if (length(dat) == 1){
    dat <- dat[[1]]
  }
  dat
}

#' Read 'Shimadzu' QTOF TIC stream
#'
#' A parser to read total ion chromatogram data streams from the quadrupole
#' time-of-flight (`QTFL RawData`) container of 'Shimadzu' `.lcd` files. Triple
#' quadrupole files store their TIC differently and are read by
#' `read_sz_tlm_tic` instead. LCD files are encoded as 'Microsoft' OLE
#' documents. The parser relies on the
#' [olefile](https://pypi.org/project/olefile/) package in Python to unpack the
#' files. The TIC data is encoded in a stream called `Centroid SumTIC`.
#' The TIC data stream contains a segment for each retention time, beginning
#' with a 8-byte header. After the header, the file consists of a series of
#' 4-byte little-endian integers in blocks of 3 (16-bytes per block), followed by
#' a 4-byte spacer (`00000000`) The first integer is the retention time
#' in milliseconds, the second integer is the scan number, and the third integer
#' is the intensity. Retention times are converted to minutes, as elsewhere in
#' the package.
#'
#' @param path Path to 'Shimadzu' `.lcd` file.
#' @param format_out Matrix or data.frame.
#' @param data_format Either `wide` (default) or `long`.
#' @param read_metadata Logical. Whether to attach metadata.
#' @author Ethan Bass
#' @return A 2D chromatogram from the SumTIC stream in `matrix` or
#' `data.frame` format, according to the value of `format_out`.
#' The chromatograms will be returned in `wide` or `long` format
#' according to the value of `data_format`.
#' @keywords internal

read_sz_qtof_tic <- function(path, format_out = "data.frame",
                             data_format = c("wide", "long"),
                             read_metadata = TRUE,
                             metadata_format = "shimadzu_lcd"){
  data_format <- match.arg(data_format, c("wide", "long"))
  tic_streams <- check_streams(path, what = "tic")
  if (length(tic_streams) == 0){
    return(NULL)
  }
  path_tic <- export_stream(path, tic_streams[[1]])
  on.exit(unlink_stream(path_tic), add = TRUE)
  f <- file(path_tic, "rb")
  on.exit(close(f), add = TRUE)
  tic <- decode_qtof_tic(f)
  dat <- format_2d_chromatogram(rt = tic[, "rt"], int = tic[, "intensity"],
                                data_format = data_format,
                                format_out = format_out)
  if (read_metadata){
    # polarity and the scan window come out of the same protobuf message
    mass_params <- read_qtof_mass_params(path)
    meta <- read_qtof_metadata(path,
                               polarity = read_qtof_polarity(path,
                                                             mass_params),
                               mz_range = read_qtof_mz_range(path,
                                                             mass_params),
                               time_range = range(tic[, "rt"]))
    dat <- attach_metadata(dat, meta, format_in = metadata_format,
                           source_file = path, data_format = data_format,
                           format_out = format_out,
                           source_file_format = "shimadzu_lcd")
  }
  dat
}

#' Decode 'Shimadzu' QTOF total ion chromatogram
#' @noRd
decode_qtof_tic <- function(f){
  seek(f, where = 0, origin = "end")
  bytes <- seek(f, where = 0, origin = "end")

  nval <- (bytes-8)/16
  seek(f, 0, "start")
  seek(f, 0, "start")

  readBin(f, what = "integer", size = 4, n = 2) # skip 2
  mat <- matrix(readBin(f, what = "integer", size = 4, n = nval * 4),
                ncol = 4, byrow = TRUE)[, 1:3, drop = FALSE]
  # retention times are stored in milliseconds; the rest of the package
  # reports minutes
  mat[,1] <- mat[,1]/60000
  colnames(mat) <- c("rt", "index", "intensity")
  mat
}

#' Read Shimadzu chromatogram
#' @noRd
read_sz_chrom <- function(path, stream){
  path_raw <- export_stream(path, stream = stream)
  on.exit(unlink_stream(path_raw), add = TRUE)
  f <- file(path_raw, "rb")
  on.exit(close(f), add = TRUE)
  dat <- data.frame(intensity = decode_sz_block(f))
  seek(f, 4)
  seek(f, 4)
  interval <- readBin(f, "integer", size = 4, endian = "little")
  times <- seq(from = 0, by = interval, length.out = nrow(dat))/60000
  rownames(dat) <- times
  dat
}

#' Read 'Shimadzu' "Method" stream
#' This function is called internally by `read_shimadzu_lcd`.
#' @author Ethan Bass
#' @noRd
read_sz_method <- function(path, stream = c("GUMM_Information", "ShimadzuPDA.1",
                                            "PDA.1.METHOD")){
  method_path <- export_stream(path, stream = stream,
                                   remove_null_bytes = TRUE)
  on.exit(unlink_stream(method_path), add = TRUE)
  if (is.na(method_path)){
    warning("Method stream could not be found --- unable to infer retention times.")
    return(NA)
  } else{
    method_stream <- xml2::read_xml(method_path)

    sz_extract_upd_elements <- function(method_stream, xpath,
                                        data_format = c("list", "data.frame")){
      data_format <- match.arg(data_format, c("list", "data.frame"))
      upd_elements <- xml2::xml_find_all(method_stream, xpath)

      vals <- suppressWarnings(as.numeric(xml2::xml_text(
        xml2::xml_find_first(upd_elements, ".//Val"))))
      data <- as.list(vals)
      names(data) <- xml2::xml_attr(upd_elements, "ID")

      if (data_format == "data.frame"){
        data <- as.data.frame(do.call(rbind, data))
        colnames(data) <- "Val"
      }
      data
    }
    sz_extract_upd_elements(method_stream, xpath = "/GUD/UP/UPD")
  }
}

#' Infer times from 'Shimadzu' Method stream
#' This function is called internally by `read_shimadzu_lcd`.
#' @note This function is no longer needed because the times can be inferred
#' (more reliably?) from the 2D Data Item.
#' @author Ethan Bass
#' @noRd
get_sz_times <- function(sz_method, what = c("pda", "chromatogram"), nval){
  what <- match.arg(what, c("pda", "chromatogram"))
  fields <- switch(what, "pda" = c("StTm", "EdTm"),
                         "chromatogram" = c("ACQ$StartTm#1", "ACQ$EndTm#1"))
  start_time <- try(get_metadata_field(sz_method, fields[1])/60000, silent = TRUE)
  end_time <- try(get_metadata_field(sz_method, fields[2])/60000, silent = TRUE)
  if (inherits(start_time, "numeric") & inherits(end_time, "numeric")){
    seq(from = start_time, to = end_time, length.out = nval)
  } else NA
}

#' Get number of rows and interval from 'PDA 3D Raw Data/Max Plot'
#' @author Ethan Bass
#' @noRd
get_shimadzu_axis <- function(path){
  maxplot_path <- export_stream(path, stream =  c("PDA 3D Raw Data", "Max Plot"))
  on.exit(unlink_stream(maxplot_path), add = TRUE)
  if (is.na(maxplot_path)){
      warning("Unable to infer number of rows in stream.")
      return(NA)
    } else {
      f <- file(maxplot_path, "rb")
      on.exit(close(f), add = TRUE)
      seek(f,4)
      interval <- readBin(f, what = "integer", n = 1, size = 4, endian = "little")
      nrows <- readBin(f, what = "integer", n = 1, size = 4, endian = "little")
      list(interval=interval, nrows=nrows)
    }
}

#' Read 'Shimadzu' LCD 3D Raw Data
#' @author Ethan Bass
#' @noRd
read_sz_pda <- function(path, n_lambdas = NULL){
  path_raw <- export_stream(path, stream =  c("PDA 3D Raw Data", "3D Raw Data"),
                            verbose = TRUE)
  on.exit(unlink_stream(path_raw), add = TRUE)
  f <- file(path_raw, "rb")
  on.exit(close(f), add = TRUE)

  seek(f, 0, 'end')
  fsize <- seek(f, NA, "current")

  # Read data

  seek(f, 0, "start")
  seek(f, 0, "start")

  axis <- get_shimadzu_axis(path)
  nrows <- ifelse(is.na(axis$nrows), fsize/(n_lambdas * 1.5), axis$nrows)

  mat <- matrix(NA, nrow = nrows, ncol = n_lambdas)
  i <- 1
  while (seek(f, NA, "current") < fsize) {
    mat[i,] <- decode_sz_block(f)
    i <- i + 1
  }
  if (any(is.na(mat[,1]))){
    mat <- mat[-which(is.na(mat[,1])),]
  }
  times <- seq(from = 0, by = axis$interval, length.out = nrow(mat))/60000
  rownames(mat) <- times
  mat
}


#' Extract wavelengths from Shimadzu LCD
#' This function is called internally by `read_shimadzu_lcd`.
#' @author Ethan Bass
#' @noRd
read_sz_wavelengths <- function(path){
  path_wavtab <- export_stream(path, stream =  c("PDA 3D Raw Data",
                                                 "Wavelength Table"))
  on.exit(unlink_stream(path_wavtab), add = TRUE)
  f <- file(path_wavtab, "rb")
  on.exit(close(f), add = TRUE)
  n_lambda <- readBin(f, what = "integer", size = 4)
  readBin(f, what = "integer", size = 4, n = n_lambda)/100
}

#' Read 'Shimadzu' LCD data block
#' This function is called internally by `read_shimadzu_lcd`.
#' @author Ethan Bass
#' @noRd
decode_sz_block <- function(f) {
  block_start <- seek(f, NA, "current")

  readBin(f, what = "integer", n = 6, size = 1) #skip
  readBin(f, what = "integer", n = 1, size = 2)

  # the value count is a 4-byte field: read as 2 bytes, a 2D stream with
  # 32768-65535 points yields a negative count and `numeric()` then errors
  n_lambda <- readBin(f, what = "integer", n = 1,
                      size = 4, endian = "little")

  block_length <- readBin(f, what = "integer", n = 1, size = 2)
  readBin(f, what = "integer", n = 5, size = 2)

  signal <- numeric(n_lambda)
  count <- 1L

  while (count < n_lambda) {
    n_bytes <- readBin(f, "integer", n = 1, size = 2)
    if (length(n_bytes) == 0) break

    values <- decode_sz_deltas(readBin(f, "raw", n = n_bytes))
    n_values <- length(values)
    if (n_values > 0){
      signal[count:(count + n_values - 1L)] <- values
      count <- count + n_values
    }

    # Read the end marker
    end <- readBin(f, "integer", n = 1, size = 2)
    # n_bytes == end
  }
  signal
}

#' Decode a delta-encoded 'Shimadzu' sub-block
#'
#' Decodes one sub-block of the delta-encoded data stream found in 'Shimadzu'
#' `.lcd` files. The first hexadecimal digit of each value is a sign digit
#' giving the number of hexadecimal digits used to encode the value; even
#' sign digits denote positive deltas and odd ones negative deltas (encoded as
#' two's complements). Values within a sub-block accumulate, so the decoded
#' signal is the cumulative sum of the deltas.
#'
#' Records are scanned in a single pass to find their start positions, after
#' which the deltas are decoded in bulk, grouped by record length.
#' @param raw A raw vector containing one sub-block.
#' @return A numeric vector of decoded values.
#' @author Ethan Bass
#' @noRd
decode_sz_deltas <- function(raw) {
  n <- length(raw)
  if (n == 0) return(numeric(0))

  # pad so that a truncated record at the end reads as trailing `00`s. Four is
  # enough because records longer than 4 bytes are rejected below.
  bytes <- c(as.integer(raw), 0L, 0L, 0L, 0L)
  sign_digit <- bytes %/% 16L

  # number of bytes in the record beginning at each position
  len <- ifelse(sign_digit > 1L, 1L + sign_digit %/% 2L, 1L)
  len[bytes == 0x82] <- 1L

  # walk the sub-block once to find where each record starts
  starts <- integer(n)
  count <- 0L
  pos <- 1L
  while (pos <= n) {
    count <- count + 1L
    starts[count] <- pos
    pos <- pos + len[pos]
  }
  starts <- starts[seq_len(count)]

  # `0x82` is a marker rather than a value
  starts <- starts[bytes[starts] != 0x82]

  sign_digit <- sign_digit[starts]
  # A sign digit of 8 or more implies a record of 5+ bytes, which has never
  # been observed and which the previous scalar decoder could not represent at
  # all (it assembled values with 32-bit shifts). Refuse rather than return a
  # plausible-looking number from an encoding we cannot validate.
  if (any(sign_digit >= 8L)){
    stop("Unsupported 'Shimadzu' delta record: sign digit ",
         max(sign_digit), " implies a record longer than 4 bytes.")
  }
  n_bytes <- ifelse(sign_digit > 1L, 1L + sign_digit %/% 2L, 1L)
  deltas <- numeric(length(starts))

  # single-byte values
  idx <- which(n_bytes == 1L)
  if (length(idx) > 0){
    value <- bytes[starts[idx]] %% 16L
    deltas[idx] <- ifelse(sign_digit[idx] == 1L, value - 16L, value)
  }

  # multi-byte values, decoded in groups of equal length
  for (size in unique(n_bytes[n_bytes > 1L])){
    idx <- which(n_bytes == size)
    pos <- starts[idx]
    # accumulate the value without the sign digit: including it would push the
    # total past 2^53, where a double can no longer hold every integer
    value <- bytes[pos] %% 16L
    for (i in seq_len(size)[-1]) {
      value <- value * 256 + bytes[pos + i - 1L]
    }
    deltas[idx] <- ifelse(sign_digit[idx] %% 2L == 1L,
                          value - 2^(8L * size - 4L), value)
  }
  cumsum(deltas)
}

#' Convert hexadecimal string to raw format
#' @param x A hexadecimal string
#' @noRd
char_to_raw <- function(x){
  hex <- strsplit(x, "")[[1]]
  hex <- paste(hex[c(TRUE, FALSE)], hex[c(FALSE, TRUE)], sep = "")
  as.raw(strtoi(hex, 16L))
}

#' Read float from 'Shimadzu' metadata
#' @noRd
sz_float <- function(x, size = 8, endian = "little"){
  readBin(char_to_raw(x), "double", n = 1, size = size, endian = endian)
}

#' Decode 'Shimadzu' metadata 'FtoX' floats
#' @noRd
sz_decode_fto <- Vectorize(
  function(x){
    x <- gsub("@FtoX@", "", x)
    if (x == "1"){
      return(1)
    } else{
      return(sz_float(x, size = 4, endian = "big"))
    }
  }
)

#' Decode 'Shimadzu' metadata 'StoX' strings
#'
#' The decoded bytes are in the codepage of the machine that wrote the file, so
#' a file from a Chinese- or Japanese-locale instrument yields a string that is
#' not valid UTF-8 (`method` and `batch` are paths, and their directory names
#' are the usual offenders). Such a string errors out of every regular
#' expression applied to it downstream, so it is repaired before it leaves the
#' parser.
#' @noRd
sz_decode_sto <- Vectorize(
  function(x){
    x <- gsub("@StoX@", "", x)
    tryCatch({raw_bytes <- as.raw(strtoi(substring(x, seq(1, nchar(x), 2),
                                                   seq(2, nchar(x), 2)), 16L))
    to_valid_utf8(rawToChar(raw_bytes))
    }, error = function(err) NA)
  }
)

#' Read 'Shimadzu' LCD file properties
#' @noRd
read_sz_file_properties <- function(path){
  path_prop <- export_stream(path, "File Property")
  on.exit(unlink_stream(path_prop), add = TRUE)
  header <- readBin(path_prop, "raw", n = 9)
  if (readBin(header[5:9],"character") == "<?xml"){
    meta <- read_sz_file_properties_xml(path_prop)
  } else{
    meta <- read_sz_file_properties_raw(path_prop)
  }
  # the instrument the file was acquired on describes the whole file rather
  # than one of its traces, so it is read here, once per read, rather than in
  # the field map, which runs once per trace
  c(meta, read_sz_system_info(path))
}

#' Read Shimadzu File Properties RAW
#' @noRd
read_sz_file_properties_raw <- function(path){
  f <- file(path, "rb")
  on.exit(close(f), add = TRUE)
  offsets <- c(SampleInfo.operator_name = 20,
               DataFileProperty.szVersion = 150,
               SampleInfo.smpl_vial = 196,
               SampleInfo.smpl_type = 210,
               SampleInfo.smpl_name = 242,
               SampleInfo.smpl_id = 306,
               SampleInfoFile.methodfile = 908,
               SampleInfoFile.batchfile = 1677)

  meta <- as.list(sapply(offsets, function(pos){
    seek(f, pos)
    clean_vendor_string(readBin(f, "character"))
  }))

  seek(f, 548)
  acquired1 <- readBin(f, "integer", size = 4, endian = "little")
  acquired2 <- readBin(f, "integer", size = 4, endian = "little")
  meta$time_acq <- sztime_to_unixtime(acquired1, acquired2)
  meta
}

#' Read Shimadzu File Properties XML
#' @noRd
read_sz_file_properties_xml <- function(path){
  raw_xml <- readLines(path, skipNul = TRUE, warn = FALSE)
  raw_xml <- sub("^\037\004|^o\004", "", raw_xml)
  xml_headers <- grep("xml version", raw_xml)

  # Combine lines and parse
  props <- lapply(seq_along(xml_headers[-1]), function(i){
    xml_content <- paste(raw_xml[xml_headers[[i]]:(xml_headers[[i+1]] - 1)],
                         collapse = "\n")
    xml_doc <- xml2::read_xml(xml_content)
  })
  names(props) <- sapply(props, xml2::xml_name)
  meta <- suppressWarnings(unlist(lapply(props, sz_decode_props),
                                  recursive = FALSE))

  meta$time_gen <- sztime_to_unixtime(meta$FileProperty.dwLowGeneratedDateTime,
                                      meta$FileProperty.dwHighGeneratedDateTime,
                                      tz = meta$FileProperty.szLocGMTDiffGenDateTime)
  meta$time_mod <- sztime_to_unixtime(meta$FileProperty.dwLowModifiedDateTime,
                                      meta$FileProperty.dwHighModifiedDateTime,
                                      meta$FileProperty.szLocGMTDiffModDateTime)
  meta$time_acq <- sztime_to_unixtime(meta$SampleInfo.dwLowDateTime,
                                      meta$SampleInfo.dwHighDateTime,
                                      tz = meta$FileProperty.szLocGMTDiffGenDateTime)

  meta
}

#' Streams `read_shimadzu_lcd` can return
#'
#' Acronyms are capitalized and ordinary words are not, as they are in the
#' `what` of every other reader that mixes the two (`read_mzml`,
#' `read_varian_sms`). `MS` is both mass spectrometry levels: the container has
#' to be decoded in full to find out which level each scan is, so asking for
#' one level is no cheaper than asking for both.
#' @noRd
sz_lcd_streams <- function(){
  c("DAD", "chroms", "MS", "MS1", "MS2", "TIC", "peak_table")
}

#' Match `what` against the streams, whatever case it is written in
#'
#' `match.arg` cannot do this for a vocabulary that is not all one case, and
#' the spelling a user reaches for (`pda`, `tic`, `ms2`) should not have to
#' match the canonical one.
#' @noRd
sz_match_streams <- function(what, choices = sz_lcd_streams()){
  # `DAD` is what the `detector` attribute, `write_mzml` and the 'Agilent'
  # readers all call this detector, so it is the canonical name here too.
  # 'Shimadzu' calls the same thing `PDA`, which is accepted as a synonym.
  what[tolower(what) == "pda"] <- "DAD"
  idx <- match(tolower(what), tolower(choices))
  if (anyNA(idx)){
    stop(sprintf("`what` should be one of %s, not %s.",
                 paste(sQuote(choices), collapse = ", "),
                 paste(sQuote(what[is.na(idx)]), collapse = ", ")),
         call. = FALSE)
  }
  unique(choices[idx])
}

#' Split decoded spectra by MS level
#'
#' MS1 and MS2 are returned as tables of their own, as they are by every other
#' mass spectrometry reader in the package, rather than as one table with a
#' level column. An acquisition event is never mixed-level, so the cut falls
#' between events rather than through one, and the columns genuinely differ:
#' only a product-ion, MRM or SIM scan has a precursor to report.
#'
#' @param dat Long `data.table` of spectra, with a `scan` column.
#' @param scan_info One row per spectrum, with `scan`, `ms_level` and
#' `polarity`.
#' @param levels Which levels to return, spelled `MS1` and `MS2`.
#' @param meta Metadata list to attach to each table, or `NULL`.
#' @param level MS level of every row of `dat`. Both readers already know this
#' from the peak counts they built `dat` with, so passing it in avoids a hash
#' probe per peak; it is joined from `scan_info` when it is not supplied.
#' @return A named list holding whichever of the requested levels the file has.
#' @author Ethan Bass
#' @noRd
sz_split_ms_levels <- function(dat, scan_info, levels = c("MS1", "MS2"),
                               meta = NULL, path, format_out,
                               metadata_format = "shimadzu_lcd",
                               level = NULL){
  scan_info <- as.data.frame(scan_info)
  if (is.null(level)){
    level <- scan_info$ms_level[match(dat$scan, scan_info$scan)]
  }
  if (anyNA(level)){
    warning(sprintf(paste("The MS level of %d spectra could not be read.",
                          "Their peaks are reported in neither table."),
                    length(unique(dat$scan[is.na(level)]))), call. = FALSE)
  }
  out <- list()
  for (label in levels){
    lvl <- as.integer(sub("MS", "", label))
    info <- scan_info[!is.na(scan_info$ms_level) & scan_info$ms_level == lvl, ,
                      drop = FALSE]
    if (nrow(info) == 0){
      next
    }
    row.names(info) <- NULL
    x <- dat[which(!is.na(level) & level == lvl), ]
    # a precursor column with nothing in it for any scan of this level is
    # describing the other level's scans
    if ("precursor_mz" %in% colnames(x) && all(is.na(x$precursor_mz))){
      data.table::set(x, j = "precursor_mz", value = NULL)
    }
    x <- convert_chrom_format(x, format_out = format_out,
                              data_format = "long")
    if (!is.null(meta)){
      meta$ms_level <- lvl
      # the levels of a run can differ in polarity -- a DDA method alternates
      # it -- so the field is only set where this level used one
      polarity <- unique(info$polarity)
      if (length(polarity) == 1){
        meta$polarity <- polarity
      }
      x <- attach_metadata(x, meta, format_in = metadata_format,
                           source_file = path, data_format = "long",
                           format_out = format_out,
                           source_file_format = "shimadzu_lcd")
    }
    attr(x, "scan_info") <- info
    out[[label]] <- x
  }
  out
}

#' Read 'Shimadzu' system information
#'
#' The `SystemInformation` stream names the system a file was acquired on, as a
#' `GUD` block of type `SI`. `IN` is the name the system was given when it was
#' configured in 'LabSolutions' --- the same string the ascii exports report as
#' `Instrument Name` --- and each `SGLI` group holds the unit assigned to one of
#' its slots (`LC`, `PDA`, `GC`, `LCMS-QP`, ...) in a `U` element.
#'
#' Neither field is reliably a model number. `IN` is free text, so it is a model
#' on one system (`LCMS-8030`) and a nickname on the next (`Instrument2`, `Full
#' LC-MS`), while the unit is a model only where the software records one
#' (`LCMS-9030`, `LCMS-8050`): older versions register every triple quadrupole
#' under the platform name `LCMS-3030`. `IN` is nonetheless the vendor's own
#' answer to which instrument wrote the file, and the answer its ascii exports
#' give, which is why it is the one the parser reports.
#'
#' The stream is absent from some files (`.qgd`, for one), in which case this
#' returns an empty list and the fields it would have filled stay `NA`.
#'
#' Read by `read_sz_file_properties`, once per file, so that the field map
#' stays a mapping of names and does no I/O of its own.
#'
#' @param path Path to a 'Shimadzu' OLE file (`.lcd` or `.gcd`).
#' @return A list with the name of the system (`SI.IN`) and the units assigned
#' to its slots (`SI.units`), or an empty list.
#' @author Ethan Bass
#' @noRd
read_sz_system_info <- function(path){
  if (length(path) != 1 || is.na(path) || !fs::is_file(path)){
    return(list())
  }
  path_si <- export_stream(path, c("GUMM_Information", "GUMMSubStg",
                                   "SystemInformation"),
                           remove_null_bytes = TRUE)
  if (length(path_si) != 1 || is.na(path_si)){
    return(list())
  }
  on.exit(unlink_stream(path_si), add = TRUE)
  raw <- readBin(path_si, what = "raw", n = file.info(path_si)$size)
  txt <- iconv(rawToChar(raw), from = "ISO-8859-1", to = "UTF-8")
  # the stream is a bare `GUD` block rather than a document, and the units carry
  # escaped `GUD` blocks of their own, so it is wrapped before it is parsed
  doc <- try(xml2::read_xml(paste0("<root>", txt, "</root>")), silent = TRUE)
  if (inherits(doc, "try-error")){
    return(list())
  }
  si <- xml2::xml_find_first(doc, ".//GUD[@Type='SI']")
  if (inherits(si, "xml_missing")){
    return(list())
  }
  # `IN` is taken as a direct child of the block: the same element name labels
  # every entry in the catalog of supported units further down the stream
  name <- xml2::xml_text(xml2::xml_find_first(si, "./IN"))
  groups <- xml2::xml_find_all(si, "./SGLI")
  units <- xml2::xml_text(xml2::xml_find_first(groups, "./U"))
  names(units) <- xml2::xml_attr(groups, "Name")
  units <- units[!is.na(units) & nzchar(units)]

  list(SI.IN = if (length(name) != 1 || is.na(name) || !nzchar(name)){
         NULL
       } else name,
       SI.units = if (length(units) == 0) NULL else units)
}

#' Decode 'Shimadzu' file properties
#' @noRd
sz_decode_props <- function(x){
  nodes <- xml2::xml_children(x)
  meta <- xml2::xml_text(nodes)
  names(meta) <- xml2::xml_name(nodes)
  fto.idx <- grep("@FtoX@", meta)
  meta[fto.idx] <- sz_decode_fto(meta[fto.idx])
  sto.idx <- grep("@StoX@", meta)
  meta[sto.idx] <- sz_decode_sto(meta[sto.idx])
  as.list(meta)
}

#' Read 'Shimadzu' 3D Data Item
#' @noRd
read_sz_3DDI <- function(path){
  path_meta <- export_stream(path, c('PDA 3D Raw Data', '3D Data Item'))
  on.exit(unlink_stream(path_meta), add = TRUE)

  raw <- readBin(path_meta, what = "raw", n = file.info(path_meta)$size)
  txt <- iconv(rawToChar(raw), from = "ISO-8859-1", to = "UTF-8")
  doc <- xml2::read_xml(paste0("<root>", txt, "</root>"))
  doc <- xml2::xml_find_first(doc, ".//GUD[@Type='3DD']")

  nodes <- xml2::xml_children(doc)
  rm <- which(xml2::xml_name(nodes) %in% c("ELE", "GUD", "DataItem", "SPR"))
  meta <- as.list(xml2::xml_text(nodes[-rm]))
  names(meta) <- xml2::xml_name(nodes[-rm])

  meta[c("WVB", "WVE", "WLS")] <-
    lapply(meta[c("WVB", "WVE", "WLS")], function(x){
      sz_float(x)/100
  })
  meta <- c(meta, read_sz_2DDI(xml2::xml_find_all(doc,
                                                  ".//GUD[@Type='2DDataItem']"),
                               read_file = FALSE))
  meta
}

#' Read 'Shimadzu' chromatogram status record
#'
#' Returns the scaling factors for a 2D chromatogram stream. The calibration
#' factor (`CF`) converts the delta-encoded integers in the stream into the base
#' unit of the detector (e.g. `uV` or `nRI`), and must be applied before the
#' value factor (`VF`), which gives the number of base units in the unit
#' selected for display (e.g. `mV` or `uRI`). So an intensity in the unit
#' reported by 'Lab Solutions' is the encoded integer times `CF` divided by
#' `VF`. `1/VF` is what 'Lab Solutions' calls the `Intensity Multiplier` in its
#' ASCII exports, while the calibration factor is already folded into the
#' intensities of those exports.
#'
#' The gain factor (`GF`) is `1` in all of the files I have seen, so it is not
#' applied anywhere.
#'
#' The file gives only the tags. `CF`, `VF` and `GF` are expanded here as
#' calibration, value and gain factor, and the unit whose `VF` is `1` is called
#' the base unit, but these names are this package's reading of the format
#' rather than the vendor's own. What is established is the arithmetic: the
#' formula above reproduces the intensities 'Lab Solutions' reports, across
#' detectors whose calibration factors range from `1` to ~310.
#'
#' The factors are stored alongside the raw data in a `Chromatogram Status`
#' stream (or `Max Plot Status`, for the PDA max plot), which consists of a
#' 64-byte record for each channel, indexed by the same channel number as the
#' corresponding `Chromatogram Ch<#>` stream. Each record consists of the
#' following fields:
#'
#' * 4 bytes: Little-endian integer flagging whether the channel contains data.
#' * 4 bytes of `00`s.
#' * 4 bytes: Little-endian integer specifying the duration of the run (in
#' milliseconds).
#' * 4 bytes of `00`s.
#' * 8 bytes: Little-endian double specifying the calibration factor (`CF`).
#' * 8 bytes: Little-endian double specifying the gain factor (`GF`).
#' * 8 bytes: Little-endian double specifying the value factor (`VF`).
#' * 24 bytes: Null-terminated string naming the unit selected for display
#' (e.g. `mV`, `mAU` or `kgf/cm2`), padded with `00`s.
#'
#' The same factors are also encoded in the `2D Data Item` stream accompanying
#' the raw data, where they agree with the status record for every channel I
#' have checked. They must be matched to the channel by `DSID` there, though,
#' since that stream also describes the status log channels, which are numbered
#' separately. The `CF` in the `LSS Data Processing` copy of the
#' `2D Data Item` is always `1`. Older files, written by 'LCsolution' rather
#' than 'Lab Solutions', have no `2D Data Item` at all, so the status record is
#' the only source of these factors.
#' @param path Path to 'Shimadzu' `.lcd` file.
#' @param stream Name of the chromatogram stream.
#' @return A list with the calibration factor (`CF`), gain factor (`GF`),
#' value factor (`VF`) and the name of the display `unit`, each `NA` where the
#' record does not supply it, or `NULL` if the record could not be read.
#' @author Ethan Bass
#' @noRd

read_sz_chrom_status <- function(path, stream){
  idx <- suppressWarnings(as.numeric(gsub("\\D", "", stream[length(stream)])))
  if (is.na(idx)){
    idx <- 1
  }
  stream[length(stream)] <- paste(gsub(" Ch\\d+$", "",
                                       stream[length(stream)]), "Status")
  path_status <- export_stream(path, stream)
  if (length(path_status) == 0 || is.na(path_status[1])){
    return(NULL)
  }
  on.exit(unlink_stream(path_status), add = TRUE)

  offset <- (idx - 1)*64
  size <- file.info(path_status)$size
  if (size < (offset + 40)){
    return(NULL)
  }
  f <- file(path_status, "rb")
  on.exit(close(f), add = TRUE)
  seek(f, where = offset)
  record <- readBin(f, what = "raw", n = min(64, size - offset))

  factors <- readBin(record[17:40], what = "double", n = 3, size = 8,
                     endian = "little")
  if (length(factors) < 3){
    return(NULL)
  }
  factors <- as.list(factors)
  names(factors) <- c("CF", "GF", "VF")

  # an unpopulated record is all `00`s
  factors[] <- lapply(factors, function(x){
    if (!is.finite(x) || x <= 0) NA_real_ else x
  })
  unit <- read_null_terminated(record, offset = 40)
  factors$unit <- if (is.null(unit)) NA_character_ else unit
  factors
}

#' Read 'Shimadzu' calibration factor
#'
#' Returns the calibration factor (`CF`) from the `Chromatogram Status` record
#' for a stream, or `1` where it could not be found. See
#' `read_sz_chrom_status()` for the layout of the record.
#' @param path Path to 'Shimadzu' `.lcd` file.
#' @param stream Name of the chromatogram stream.
#' @author Ethan Bass
#' @noRd

read_sz_calibration_factor <- function(path, stream){
  status <- read_sz_chrom_status(path, stream)
  if (is.null(status) || is.na(status$CF)){
    return(1)
  }
  status$CF
}

#' Read 'Shimadzu' 2D Data Item
#' @noRd
read_sz_2DDI <- function(path, read_file = TRUE, idx = 1){
  if(read_file){
    path_meta <- export_stream(path, c('LSS Data Processing', '2D Data Item'))
    on.exit(unlink_stream(path_meta), add = TRUE)
    raw <- readBin(path_meta, what = "raw", n = file.info(path_meta)$size)
    txt <- iconv(rawToChar(raw), from = "ISO-8859-1", to = "UTF-8")
    doc <- xml2::read_xml(paste0("<root>", txt, "</root>"))
    doc <- xml2::xml_find_first(doc, ".//GUD[@Type='2DDataItem']")
  } else{
    doc <- path
  }
  if (idx == "PDA"){
    det <- xml2::xml_text(xml2::xml_find_all(doc, "//DN"))
    idx <- which(det == "PDA")
  }

  nodes <- xml2::xml_child(doc, search = idx) |> xml2::xml_children()
  ddi_idx <- which(xml2::xml_name(nodes) == "DDI")

  meta <- xml2::xml_text(nodes[-ddi_idx])
  names(meta) <- xml2::xml_name(nodes[-ddi_idx])

  meta[c("CF", "GF", "AT", "DLT")] <-
    lapply(meta[c("CF", "GF", "AT", "DLT")], function(x) sz_float(x))

  meta <- c(meta, extract_axis_metadata(nodes))

  meta$time.vf <- ifelse(is.na(meta$time.vf), 60000, meta$time.vf)
  meta$detector.vf <- 1/meta$detector.vf

  meta[c("AT", "DLT", "Rate")] <-
    lapply(meta[c("AT", "DLT", "Rate")], function(x) as.numeric(x)/meta$time.vf)

  meta
}

#' Extract axis metadata
#' @noRd
extract_axis_metadata <- function(x){
  idx <- c(0, 1)
  ax <- lapply(idx, function(i){
    ax <- xml2::xml_find_all(x, paste0(".//Axis[@ID='", i, "']"))
    dus <- as.numeric(xml2::xml_attr(ax, "DUS"))
    us <- xml2::xml_find_all(ax, "US")
    if (length(dus) == 1 && !is.na(dus) && dus != 0 && length(us) >= dus){
      list(selected = us[[dus]], available = us)
    } else NA

  })
  names(ax) <- c("detector", "time")
  unlist(lapply(ax, function(x){
    if (is.list(x)){
      list(vf = xml2::xml_find_all(x$selected, "VF") |> xml2::xml_text() |>
             sz_float(),
           unit = xml2::xml_find_all(x$selected, "Unit") |> xml2::xml_text(),
           base_unit = extract_sz_base_unit(x$selected, x$available)
           )
    } else list(vf = NA, unit = NA, base_unit = NA)
  }), recursive = FALSE)
}

#' Find the base unit of an axis
#'
#' Each axis offers a choice of units, grouped by unit type (`UT`), one of which
#' is selected for display by the `DUS` attribute of the axis. Within a group,
#' the value factor (`VF`) of each unit gives the number of base units it
#' represents, so the base unit is the one with a factor of `1`. For example, an
#' absorbance axis offers `µAU` (`1`), `mAU` (`1000`) and `AU` (`1e6`), so the
#' values are encoded in `µAU`.
#' @param selected The `US` node selected for display.
#' @param us All of the `US` nodes belonging to the axis.
#' @return The name of the base unit, or `NA` if it could not be identified.
#' @author Ethan Bass
#' @noRd

extract_sz_base_unit <- function(selected, us){
  ut <- xml2::xml_text(xml2::xml_find_first(selected, "UT"))
  uts <- vapply(us, function(x){
    xml2::xml_text(xml2::xml_find_first(x, "UT"))
  }, FUN.VALUE = character(1))
  vfs <- vapply(us, function(x){
    sz_float(xml2::xml_text(xml2::xml_find_first(x, "VF")))
  }, FUN.VALUE = numeric(1))
  idx <- which(uts == ut & vfs == 1)
  if (length(idx) != 1){
    return(NA_character_)
  }
  xml2::xml_text(xml2::xml_find_first(us[[idx]], "Unit"))
}

#' Unit of the values as they are encoded in the file
#'
#' The integers in a data stream are encoded in the base unit of the detector,
#' but only once the calibration factor has been applied to them. Where the
#' calibration factor is not `1`, the unscaled values are raw converter counts,
#' so the unit selected for display is left in place rather than claiming a
#' unit the unscaled values do not have.
#' @param DI Data item metadata.
#' @param cf The calibration factor applied to the stream.
#' @author Ethan Bass
#' @noRd

sz_stored_unit <- function(DI, cf = 1){
  base_unit <- DI$detector.base_unit
  if (length(cf) == 1 && !is.na(cf) && cf == 1 &&
      length(base_unit) == 1 && !is.na(base_unit)){
    base_unit
  } else{
    DI$detector.unit
  }
}
