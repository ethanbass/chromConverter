# Metadata vocabulary -----------------------------------------------------
#
# The names chromConverter uses for the metadata it attaches to a chromatogram.
# Each format's field map chooses which of these it can fill in;
# nothing outside this file should invent a name.
#
# `extract_metadata` reads back exactly this list, so the names the readers
# attach and the names reported to the user cannot disagree.

#' Metadata fields chromConverter attaches to a chromatogram
#'
#' The canonical vocabulary, and the default set of elements
#' [extract_metadata] reports. A format's field map may set fields outside this
#' list -- `wavelength`, `ms_params` and `n_scans` describe some detectors and
#' not others -- but anything in it means the same thing for every format.
#'
#' @return A character vector of attribute names.
#' @noRd
chrom_metadata_fields <- function(){
  c(# instrument and detector
    "instrument", "detector", "detector_model", "detector_range",
    "detector_y_unit", "detector_x_unit",
    # acquisition software and method
    "software", "method", "batch", "operator", "run_datetime",
    # sample
    "sample_name", "sample_id", "sample_position",
    "sample_injection_volume", "sample_amount",
    # time and intensity axes
    "time_range", "time_interval", "time_unit", "intensity_multiplier",
    "scaled",
    # provenance: where the data came from and how it was read
    "source_file", "source_file_format", "source_sha1", "data_format",
    "parser", "format_out")
}

#' Superseded metadata field names
#'
#' Names that earlier versions attached, mapped to the name now used. Accepted
#' by [extract_metadata]'s `what`, so code written against the old spelling
#' keeps returning a column.
#' @noRd
.metadata_field_aliases <- c(detector_id = "detector_model",
                             injection_volume = "sample_injection_volume",
                             run_date = "run_datetime",
                             software_name = "software",
                             no_scans = "n_scans",
                             detector_unit = "detector_y_unit",
                             time_start = "time_range",
                             time_end = "time_range",
                             end_time = "time_range")

#' Resolve superseded metadata field names
#' @noRd
resolve_metadata_fields <- function(what){
  hit <- what %in% names(.metadata_field_aliases)
  what[hit] <- .metadata_field_aliases[what[hit]]
  unique(what)
}

#' Attributes that describe the object rather than the data
#'
#' The structural attributes: how the object is put together, rather than what
#' was measured or where it came from. Anything that copies a chromatogram's
#' metadata onto a derived object has to leave these behind, since the derived
#' object has its own.
#'
#' Note this is deliberately only the structural set. `list_metadata_attrs`
#' skips `comment` and `acaml_metadata` as well, because those describe a list
#' as a whole and should not be copied down onto each of its traces, whereas
#' `transfer_metadata` has to keep them: a reshaped chromatogram is the same
#' chromatogram and should not lose its acaml table.
#' @noRd
bookkeeping_attrs <- function(){
  c("names", "class", "dim", "dimnames", "row.names")
}

#' An empty metadata record
#'
#' Every canonical field as `NA`, for a format that carries no metadata at all.
#' `NA` rather than absent: an attribute that is never set drops the column from
#' [extract_metadata] entirely, where `NA` reports it as empty.
#'
#' The provenance fields are left out, since `finalize_metadata` fills those in
#' with real values.
#' @noRd
empty_metadata <- function(){
  fields <- setdiff(chrom_metadata_fields(),
                    c("source_file", "source_file_format", "source_sha1",
                      "data_format", "parser", "format_out"))
  stats::setNames(rep(list(NA), length(fields)), fields)
}

#' Attach the provenance block and build the chromatogram
#'
#' A field map returns only the fields it can read from the vendor metadata.
#' This adds the rest -- where the file came from, how it was read, and what
#' shape the result is in -- so that every format reports them identically.
#'
#' @param x The chromatogram.
#' @param attrs Named list returned by the format's field map.
#' @param ctx The arguments to `attach_metadata` that are not vendor metadata:
#' `source_file`, `source_file_format`, `format_out`, `data_format`, `parser`
#' and `scale`.
#' @noRd
finalize_metadata <- function(x, attrs, ctx){
  # drop null values to avoid deleting attributes that have already been set
  attrs <- attrs[!vapply(attrs, is.null, logical(1))]

  attrs$source_file <- ctx$source_file
  attrs$source_file_format <- ctx$source_file_format
  attrs$source_sha1 <- source_sha1(ctx$source_file)
  attrs$format_out <- ctx$format_out
  attrs$data_format <- attrs$data_format %||% ctx$data_format
  attrs$parser <- ctx$parser %||% attrs$parser %||% NA
  if (!is.null(ctx$scale)) attrs$scaled <- ctx$scale

  validate_metadata_names(attrs)
  do.call(structure, c(list(x), attrs))
}

#' Warn about metadata names outside the vocabulary
#'
#' A field map that invents a name puts it beyond the reach of
#' [`extract_metadata`], which asks for the vocabulary by name. Enabled for
#' `devtools::test()` and `R CMD check`, and a no-op otherwise, so a user never
#' sees it.
#' @noRd
validate_metadata_names <- function(attrs){
  if (!isTRUE(getOption("chromConverter.validate_metadata",
                        !identical(Sys.getenv("NOT_CRAN"), "")))) return(invisible(NULL))
  known <- c(chrom_metadata_fields(), .metadata_extra_fields)
  unknown <- setdiff(names(attrs), known)
  if (length(unknown) > 0){
    warning(sprintf("Unrecognized metadata field(s): %s.",
                    paste(sQuote(unknown), collapse = ", ")), call. = FALSE)
  }
  invisible(NULL)
}

#' Metadata fields that only some detectors or formats have
#'
#' Real fields, but not part of the vocabulary every format shares, so
#' [extract_metadata] does not ask for them by default. Listed here so
#' `validate_metadata_names` can tell them from a typo.
#' @noRd
.metadata_extra_fields <- c(
  # file identity
  "file_version", "file_type",
  # the control platform a 'Shimadzu' triple quadrupole reports for itself,
  # which names a line of instruments rather than one of them
  "instrument_config",
  # the channel a 'Shimadzu' OLE trace was read from, which is also what names
  # the peak table that goes with it
  "channel_id",
  # detector specifics
  "wavelength", "bandwidth", "detector_reference", "signal_descriptor",
  "polarity",
  # software detail
  "software_version", "software_revision",
  # sample detail
  "sample_type", "sample_dilution",
  # axes
  "time_interval_unit", "time_multiplier", "mz_multiplier",
  "intensity_offset",
  # mass spectrometry
  "n_scans", "ms_params",
  # `metadata_format = "raw"` passes the vendor list through untouched
  "metadata")

#' Field map for a metadata format
#'
#' Keyed by metadata format tag, which is a different key space from the format
#' registry in `R/parser_registry.R`: several 'ChemStation' formats share one
#' map, and `chemstation_peaklist` is a tag `read_peaklist` produces rather
#' than a format `read_chroms` reads.
#' @noRd
.metadata_maps <- list(
  andi_chrom = "meta_andi_chrom",
  andi_ms = "meta_andi_ms",
  asm = "meta_asm",
  chemstation = "meta_chemstation",
  chemstation_csv = "meta_chemstation_csv",
  chemstation_peaklist = "meta_chemstation_peaklist",
  chromatotec = "meta_chromatotec",
  chromeleon = "meta_chromeleon",
  default = "meta_default",
  masshunter_dad = "meta_masshunter_dad",
  mdf = "meta_mdf",
  mzml = "meta_mzml",
  rainbow = "meta_rainbow",
  shimadzu_chrom = "meta_shimadzu_chrom",
  shimadzu_dad = "meta_shimadzu_dad",
  shimadzu_lcd = "meta_shimadzu_lcd",
  thermoraw = "meta_thermoraw",
  varian_sms = "meta_varian_sms",
  waters_arw = "meta_waters_arw",
  waters_raw = "meta_waters_raw"
)

#' Look up a format's field map
#' @noRd
metadata_map <- function(format_in){
  fn <- .metadata_maps[[format_in]]
  if (is.null(fn)) return(NULL)
  get(fn, envir = asNamespace("chromConverter"))
}
