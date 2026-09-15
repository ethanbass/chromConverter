# Metadata vocabulary -----------------------------------------------------
#
# The names chromConverter uses for the metadata it attaches to a chromatogram,
# in one place. Each format's field map chooses which of these it can fill in;
# nothing outside this file should invent a name.
#
# Before this existed the vocabulary was implied by 20 hand-written field maps
# and read back by a separately hand-written list in `extract_metadata`, so the
# two drifted: the reader list asked for `injection_volume`, which one format
# set, and not `sample_injection_volume`, which seventeen did.

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
    "instrument", "detector", "detector_id", "detector_range",
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
.metadata_field_aliases <- c(injection_volume = "sample_injection_volume",
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
