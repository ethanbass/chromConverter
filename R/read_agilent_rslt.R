#' Read files from 'Agilent' .rslt directories
#'
#' Reads a sequence of injections from an 'Agilent' `.rslt` directory.
#' Combines raw chromatogram data from `.dx` files with injection-level
#' metadata parsed from the accompanying `.acaml` file.
#'
#' Currently this function only reads `.dx` chromatogram files. To read the
#' peak tables stored in `.rx` files, use [read_peaklist] with
#' `format_in = "openlab"`.
#'
#' @inheritParams read_agilent_dx
#' @param path Path to 'Agilent' `.rslt` directory.
#' @param sample_names How to name the chromatograms that are returned. Either
#' `basename` (default), to use the name of the source `.dx` file, or
#' `sample_name`, to use the sample name field from the metadata.
#' @param progress_bar Logical. Whether to show a progress bar while reading
#' the sequence's `.dx` files. Defaults to `FALSE`, since [read_chroms] already
#' reports progress over the directories it found and a second bar inside each
#' one would be redrawn per directory. Set to `TRUE` when calling this function
#' directly on a sequence with many injections.
#' @param cl Argument to [pbapply][pbapply::pbapply] specifying the number
#' of clusters to use or a cluster object created by
#' [makeCluster][parallel::makeCluster]. Defaults to `1`.
#' @return A list of chromatograms (one `read_agilent_dx`-style result per
#' injection in the sequence), in the format specified by `data_format` and
#' `format_out`. If `read_metadata` is `TRUE`, injection-level metadata parsed
#' from the `.acaml` file is attached to each chromatogram as attributes.
#' @examples \dontrun{
#' read_agilent_rslt("path/to/sequence.rslt")
#' }
#' @author Ethan Bass
#' @family 'Agilent' parsers
#' @export
read_agilent_rslt <- function(path, what = c("chroms","dad"), path_out = NULL,
                              format_out = c("matrix", "data.frame", "data.table"),
                              data_format = c("wide", "long"),
                              read_metadata = TRUE,
                              metadata_format = c("chromconverter", "raw"),
                              collapse = TRUE,
                              sample_names = c("basename", "sample_name"),
                              progress_bar = FALSE, cl = 1){
  format_out <- match.arg(format_out, c("matrix", "data.frame", "data.table"))
  data_format <- match.arg(data_format, c("wide", "long"))
  metadata_format <- match.arg(metadata_format, c("chromconverter", "raw"))
  sample_names <- match.arg(sample_names, c("basename", "sample_name"))
  dx_files <- list.files(path, pattern = "\\.dx$", ignore.case = TRUE,
                         full.names = TRUE)
  if (length(dx_files) == 0){
    stop("No .dx files found in ", path)
  }

  acaml_file <- list.files(path, pattern = "\\.acaml$", ignore.case = TRUE,
                           full.names = TRUE)
  acaml <- if (read_metadata && length(acaml_file) > 0){
    read_acaml(acaml_file[1])
  } else NULL

  laplee <- choose_apply_fnc(progress_bar, cl = cl)
  data <- laplee(dx_files, read_agilent_dx, what = what, path_out = path_out,
                 format_out = format_out, data_format = data_format,
                 read_metadata = read_metadata,
                 metadata_format = metadata_format, collapse = collapse)
  if (!is.null(acaml)){
    data <- mapply(function(chrom, fname){
      meta <- acaml[acaml$RawDataFileName == fname, , drop = FALSE]
      if (nrow(meta) == 0){
        warning("No acaml metadata found for ", fname)
        return(chrom)
      }
      add_acaml_metadata(chrom, meta)
    }, data, basename(dx_files), SIMPLIFY = FALSE)
  }
  file_names <- gsub("\\.dx$", "", basename(dx_files), ignore.case = TRUE)
  names(data) <- if (sample_names == "sample_name"){
    name_by_sample_name(data, file_names)
  } else file_names
  structure(data, class = c("chrom_list", "list"))
}

#' Attach a row of `read_acaml` output to a chromatogram or peak table
#' @noRd
add_acaml_metadata <- function(x, meta){
  acaml_field_map <- c(
    SampleName           = "sample_name",
    VialNumber           = "sample_position",
    InstrumentName       = "instrument",
    Software             = "software",
    SoftwareVersion      = "software_version",
    AcqMethodName        = "method",
    SequenceName         = "batch",
    InjectionAcqDateTime = "run_datetime",
    InjectionVolume      = "sample_injection_volume",
    SampleAmount         = "sample_amount",
    SampleType           = "sample_type"
  )
  for (col in names(acaml_field_map)){
    if (col %in% names(meta) && !is.na(meta[[col]]) && nzchar(meta[[col]])){
      val <- meta[[col]]
      if (acaml_field_map[[col]] == "run_datetime"){
        val <- as.POSIXct(val, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
      }
      attr(x, acaml_field_map[[col]]) <- val
    }
  }
  attr(x, "acaml_metadata") <- meta
  x
}

#' Read the peak tables in an 'OpenLab' `.rx` file
#'
#' The `.rx` archive holds the integration results as XML, keyed to signals
#' by ID. The signal descriptors (e.g. `DAD1A,Sig=218.0,8.0  Ref=off`) and
#' the injection metadata come from the `.acaml` file in the same directory;
#' without one, tables are named by signal ID.
#' @param path Path to an `.rx` file.
#' @param acaml_cache An environment in which to keep each `.acaml` file once
#' read, so that the `.rx` files of a sequence share one reading of it.
#' @return A list with one peak table per integrated signal, sorted by
#' retention time and named by signal descriptor.
#' @noRd
read_agilent_rx <- function(path,
                            peaktable_format = c("chromatographr", "original"),
                            read_metadata = TRUE, acaml_cache = new.env()){
  peaktable_format <- match.arg(tolower(peaktable_format),
                                c("chromatographr", "original"))
  xpath <- function(tag) sprintf("./*[local-name()='%s']", tag)
  doc <- xml2::read_xml(unz(path, "Base/InjectionACAML"))
  acaml_file <- list.files(dirname(path), pattern = "\\.acaml$",
                           ignore.case = TRUE, full.names = TRUE)
  acaml <- list(descs = character())
  if (length(acaml_file) > 0){
    acaml_file <- acaml_file[1]
    if (is.null(acaml_cache[[acaml_file]])){
      acaml_cache[[acaml_file]] <- read_rx_acaml(acaml_file, read_metadata)
    }
    acaml <- acaml_cache[[acaml_file]]
  }
  descs <- acaml$descs
  compounds <- xml2::xml_find_all(doc, "//*[local-name()='InjectionCompound']")
  compound_peaks <- lapply(compounds, function(cmp){
    xml2::xml_attr(xml2::xml_find_all(cmp, ".//*[local-name()='Peak_ID']"),
                   "id")
  })
  compound_names <- rep(
    xml2::xml_text(xml2::xml_find_first(compounds, xpath("CompoundName"))),
    lengths(compound_peaks))
  names(compound_names) <- unlist(compound_peaks)

  results <- xml2::xml_find_all(doc, "//*[local-name()='SignalResult']")
  peaks <- lapply(results, xml2::xml_find_all, xpath("Peak"))
  results <- results[lengths(peaks) > 0]
  peaks <- peaks[lengths(peaks) > 0]
  ids <- xml2::xml_attr(xml2::xml_find_first(results, xpath("Signal_ID")), "id")
  peak_lists <- lapply(peaks, function(pk){
    fields <- setdiff(unique(xml2::xml_name(xml2::xml_children(pk))),
                      "BaselineParameters")
    tab <- lapply(fields, function(field){
      node <- xml2::xml_find_first(pk, xpath(field))
      val <- xml2::xml_attr(node, "val")
      if (all(is.na(val))) trimws(xml2::xml_text(node)) else as.numeric(val)
    })
    names(tab) <- fields
    tab <- as.data.frame(tab)
    tab$CompoundName <- unname(compound_names[xml2::xml_attr(pk, "id")])
    tab <- tab[order(tab$RetentionTime), , drop = FALSE]
    rownames(tab) <- NULL
    if (peaktable_format == "chromatographr"){
      tab <- chromatographr_peak_table(tab, list(
        rt = "RetentionTime", start = "BeginTime", end = "EndTime",
        area = "Area", height = "Height"))
    }
    tab
  })
  names(peak_lists) <- ifelse(ids %in% names(descs), descs[ids], ids)
  peak_lists[] <- lapply(seq_along(peak_lists), function(i){
    chemstation_signal_attrs(peak_lists[[i]], names(peak_lists)[i])
  })
  if (read_metadata && !is.null(acaml$meta)){
    inj <- xml2::xml_attr(xml2::xml_find_first(
      doc, "//*[local-name()='InjectionMeasData_ID']"), "id")
    meta <- acaml$meta[acaml$meta$InjectionId %in% inj, , drop = FALSE]
    if (nrow(meta) > 0){
      peak_lists <- add_acaml_metadata(peak_lists, meta)
      peak_lists[] <- lapply(peak_lists, add_acaml_metadata, meta = meta)
    }
  }
  peak_lists
}

#' Read what `read_agilent_rx` needs from an `.acaml` file
#' @return A list of the signal descriptors, named by signal ID, and, if
#' `read_metadata`, the injection table from `read_acaml`.
#' @noRd
read_rx_acaml <- function(path, read_metadata = TRUE){
  sigs <- xml2::xml_find_all(xml2::read_xml(path),
                             "//*[local-name()='Signal'][@id]",
                             ns = character())
  descs <- stats::setNames(
    xml2::xml_text(xml2::xml_find_first(
      sigs, "./*[local-name()='Description']", ns = character())),
    xml2::xml_attr(sigs, "id"))
  list(descs = descs, meta = if (read_metadata) read_acaml(path))
}
