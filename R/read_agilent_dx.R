#' Read 'Agilent' DX files
#'
#' Reads 'Agilent' `.dx` files.
#'
#' The archive is extracted to `path_out`, or to a temporary directory that is
#' deleted afterwards, and its `.ch`, `.uv` and `.it` files are read with
#' [read_chemstation_ch], [read_chemstation_uv] and an internal reader for
#' `.it` files.
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
#' (`dad`) and/or auxiliary instrumental data (`instrument`), such as
#' temperature, pressure or solvent composition. Accepts multiple arguments,
#' and defaults to `chroms` and `dad`. When more than one is requested, any the archive does not contain
#' are left out; a single one that is missing is an error.
#' @param path_out A directory to export unzipped files. If a path is not
#' specified, a temporary directory is used. The files are extracted into a
#' folder named for `path`, overwriting any files of the same name already
#' there.
#' @return A list with one element per type in `what`, each a chromatogram or
#' a list of chromatograms named by signal, in the format specified by
#' `format_out` and `data_format`. With `collapse = TRUE`, a list of one
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
  what <- match.arg(what, c("chroms", "dad", "instrument"), several.ok = TRUE)
  files <- unzip(path, list = TRUE)
  acmd <- grep("^injection\\.acmd$", files$Name, ignore.case = TRUE,
               value = TRUE)
  exts <- c(chroms = "\\.ch$", dad = "\\.uv$", instrument = "\\.it$")
  files <- lapply(exts[what], function(ext){
    grep(ext, files$Name, ignore.case = TRUE, value = TRUE)
  })
  if (length(files) > 1){
    what <- what[vapply(files, length, FUN.VALUE = numeric(1)) > 0]
  }
  if (is.null(path_out)) {
    tmp <- temp_directory(path)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  } else {
    tmp <- fs::path_expand(path_out)
  }
  path_out <- fs::path(tmp, basename(path))
  fs::dir_create(path_out, force = TRUE)
  unzip(path, files = unlist(files), exdir = path_out)
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
  dat <- mget(what)
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
  info <- xml2::xml_find_first(xml2::read_xml(path),
                               "./*[local-name()='InjectionInfo']")
  field <- function(name){
    node <- xml2::xml_find_first(info, sprintf("./*[local-name()='%s']", name))
    val <- if (is.na(node)) "" else trimws(xml2::xml_text(node))
    if (nzchar(val)) val else NA_character_
  }
  volume <- suppressWarnings(as.numeric(field("InjectionVolume")))
  if (!isTRUE(field("InjectionVolumeUnits") %in% c("\u00b5L", "uL"))){
    volume <- NA_real_
  }
  list(run_datetime = parse_iso8601(field("RunDateTime")),
       sample_injection_volume = volume)
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
