#' Attaches metadata to chromatogram
#'
#' @name attach_metadata
#' @param x chromatogram
#' @param meta List object containing metadata.
#' @param format_in Chromatogram format
#' @param format_out R format. Either `matrix` or `data.frame`.
#' @param data_format Whether data is in wide or long format.
#' @param parser What parser was used to decode the data.
#' @param source_file The path to the source file.
#' @param scale Whether the data has been scaled.
#' @return A chromatogram with attached metadata.
#' @author Ethan Bass
#' @noRd

attach_metadata <- function(x, meta, format_in, format_out, data_format,
                            parser = NULL, source_file,
                            source_file_format = format_in,
                            scale = NULL){
  ctx <- list(source_file = source_file,
              source_file_format = source_file_format,
              format_out = format_out, data_format = data_format,
              parser = parser, scale = scale)
  if (identical(format_in, "raw")){
    return(finalize_metadata(x, list(metadata = meta), ctx))
  }
  field_map <- metadata_map(format_in)
  if (is.null(field_map)){
    warning(sprintf(paste("Metadata for the %s format could not be",
                          "interpreted. Returning the data with its source",
                          "file and parser recorded, but without the",
                          "instrument metadata."),
                    sQuote(format_in)), call. = FALSE)
    return(finalize_metadata(x, empty_metadata(), ctx))
  }
  finalize_metadata(x, field_map(meta, ctx), ctx)
}

#' Clean a string decoded from a vendor file
#'
#' Some vendor metadata strings are Latin-1 (the `method` field of a 'Shimadzu'
#' `.qgd` file) and some carry embedded control bytes (the same field of an
#' 'OpenLab' 131 `.uv` file contains `\032`). Left alone they produce strings
#' for which `validUTF8()` is `FALSE`, so `nchar()` and `toupper()` error and
#' `grepl()` warns and fails to match.
#'
#' Only strings that are not already valid UTF-8 are re-encoded: some fields
#' (e.g. the `units` of an 'Agilent' `.dx` instrument channel, `"\u00b0C"`)
#' are UTF-8 as read, and treating those as Latin-1 would mojibake them. The
#' re-encoding has to come first either way, because `gsub` errors on a string
#' that is not yet valid in the current encoding.
#' @noRd
clean_vendor_string <- function(x){
  if (!is.character(x)) return(x)
  broken <- !is.na(x) & !validUTF8(x)
  x[broken] <- iconv(x[broken], from = "ISO-8859-1", to = "UTF-8")
  gsub("[[:cntrl:]]", "", x)
}

#' Parse a date-time from a 'Shimadzu' ASCII export
#'
#' 'Lab Solutions' writes the timestamps in its ASCII exports using the date and
#' time format of the machine that produced the export, so no single format
#' string can read them. The variants seen so far are a 12-hour month-first
#' format (`4/26/2021 11:01:11 PM`) and 24-hour day-first formats separated by
#' either slashes (`02/08/2023 17:08:21`) or hyphens (`29-03-2022 10:12:19`).
#'
#' A leading component greater than `12` can only be a day, which settles the
#' order on its own. Otherwise the 12-hour clock is taken as the signal:
#' 'Windows' pairs it with the month-first format, so a timestamp written on a
#' 24-hour clock is read as day-first. Both orders are tried in either case, so
#' an unrecognized combination still parses if it is unambiguous.
#'
#' Note that the wall clock time is local to the machine that wrote the export,
#' which does not record its time zone, so the result is a local time labelled
#' as UTC rather than a true UTC instant.
#' @param x Character vector of date-times.
#' @return A `POSIXct` vector, with `NA` wherever the value could not be read.
#' @author Ethan Bass
#' @noRd

parse_shimadzu_ascii_datetime <- function(x){
  out <- .POSIXct(rep(NA_real_, length(x)), tz = "UTC")
  if (length(x) == 0){
    return(.POSIXct(NA_real_, tz = "UTC"))
  }
  x <- trimws(as.character(x))

  month_first <- c("%m/%d/%Y %I:%M:%S %p", "%m-%d-%Y %I:%M:%S %p",
                   "%m/%d/%Y %H:%M:%S", "%m-%d-%Y %H:%M:%S")
  day_first <- c("%d/%m/%Y %H:%M:%S", "%d-%m-%Y %H:%M:%S",
                 "%d/%m/%Y %I:%M:%S %p", "%d-%m-%Y %I:%M:%S %p")
  iso <- c("%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S")

  leading <- suppressWarnings(as.numeric(sub("^(\\d+)\\D.*$", "\\1", x)))
  twelve_hour <- grepl("[AP]M[[:space:]]*$", x, ignore.case = TRUE)
  prefer_day <- !twelve_hour | (!is.na(leading) & leading > 12)

  for (i in seq_along(x)){
    if (is.na(x[i]) || !nzchar(x[i])) next
    formats <- if (prefer_day[i]){
      c(day_first, iso, month_first)
    } else{
      c(month_first, iso, day_first)
    }
    for (format in formats){
      parsed <- as.POSIXct(x[i], format = format, tz = "UTC")
      if (!is.na(parsed)){
        out[i] <- parsed
        break
      }
    }
  }
  out
}

#' Sample name recorded by the file, or the file's own name
#'
#' Most formats record a sample name, but not all files fill it in, so the
#' basename of the source file is the fallback. Note this deliberately avoids
#' `ifelse()`, for the reason given under `get_metadata_field`: its result is
#' shaped like the *test*, which truncates a name that is not a length-1 vector.
#' @noRd
sample_name_or_file <- function(meta, field, source_file){
  val <- meta[[field]]
  if (is.null(val) || length(val) == 0){
    return(fs::path_ext_remove(basename(source_file)))
  }
  val
}

#' SHA-1 of the source file
#'
#' `NA` for anything that is not a file, rather than an error. Some formats are
#' directories -- a Waters `.raw`, an 'Agilent' `.D` -- and `digest` errors when
#' handed one, so the check belongs here rather than in each field map.
#'
#' Memoized on size and mtime as well as path, because a single read can attach
#' metadata many times over: a 'Shimadzu' TLM file with 72 acquisition events
#' hashes the same source once per event, and hashing is ~85 ms for 34 MB.
#' @noRd
source_sha1 <- function(path){
  if (length(path) != 1 || is.na(path) || !fs::is_file(path)) return(NA)
  info <- file.info(path)
  key <- paste(normalizePath(path, winslash = "/"), info$size,
               format(info$mtime, "%Y-%m-%d %H:%M:%OS6"), sep = "\r")
  hit <- sha1_cache[[key]]
  if (!is.null(hit)) return(hit)
  val <- digest::digest(path, algo = "sha1", file = TRUE)
  assign(key, val, envir = sha1_cache)
  val
}

sha1_cache <- new.env(parent = emptyenv())

#' Get a metadata field
#'
#' Returns `null_val` when the field is absent or empty, and otherwise the
#' value unchanged. Note: this deliberately avoids `ifelse()`, which returns a
#' value shaped and typed like its *test* -- that silently truncated
#' multi-element fields (e.g. a `c(250, 600)` detector range) to their first
#' element and dropped attributes such as the class of a `POSIXct`.
#' @noRd
get_metadata_field <- function(x, field, num = FALSE, null_val = NA){
  val <- x[[field]]
  if (is.null(val) || length(val) == 0){
    return(null_val)
  }
  if (num) as.numeric(val) else val
}

#' @noRd
get_metadata_field2 <- function(x, field, class = c("char","float","int"),
                                null_val = NA){
  class <- match.arg(class, c("char","float","int"))
  conv <- switch(class, char = as.character, float = as.numeric,
         int = as.integer)
  ifelse(!is.null(attr(x, field)) && !is.na(attr(x, field)),
         conv(attr(x, field)), null_val)
}

#' @noRd
get_time_unit <- function(string, format_in){
  if (length(string) == 0 || is.na(string)){
    NA
  } else{
    if (format_in %in% c("chromeleon", "shimadzu")){
      pattern <- "\\((.*?)\\)"
      unit <- gsub("\\(|\\)", "", regmatches(string, regexpr(pattern, string))[[1]])
      switch(unit, "min" = "Minutes", "sec" = "Seconds")
    } else NA
  }
}

#' @name read_chemstation_metadata
#' @param file file
#' @param what Whether to return `metadata` or `peaktable`.
#' @importFrom readxl read_xls
#' @return A list containing extracted metadata.
#' @author Ethan Bass
#' @noRd
read_chemstation_metadata <- function(file, what = c("metadata", "peaktable")){
  what <- match.arg(what, c("metadata", "peaktable"))
  folder <- gsub(basename(file), "", file)
  if (grepl("\\.D/$", folder, ignore.case = TRUE)){
    reps <- list.files(folder, pattern = '.xls',
                      ignore.case = TRUE, full.names = TRUE)
    if (length(reps) > 0){
      if (what == "metadata"){
        meta <- as.data.frame(readxl::read_xls(reps[1], sheet = 1, skip = 1))
        meta2<-as.list(meta$Results)
        names(meta2) <- meta$Title
        meta2
      } else if (what == "peaktable"){
        pktab <- as.data.frame(readxl::read_xls(reps[1], sheet = "Peak"))
        pktab <- pktab[, -c(1:2)]
        pktab
      }
    }
  }
}

#' @name read_masshunter_metadata
#' @param file file
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @return A list containing extracted metadata.
#' @author Ethan Bass
#' @noRd
read_masshunter_metadata <- function(file){
  # Both are left `NULL` when the XML file they come from is absent, so that a
  # path without a `sample_info.xml` returns `NULL` rather than raising
  # "object 'meta_sample' not found". The masshunter field map reads `meta$x`
  # throughout, which is `NULL` either way.
  meta_sample <- NULL
  meta_devices <- NULL
  folder <- gsub(basename(file), "", file)
  if (grepl("\\.D/|\\.d/$", folder, ignore.case = TRUE)){
    rep <- list.files(folder, pattern = '.xml',
                      ignore.case = TRUE, full.names = TRUE)
    if (length(rep) > 0){
      path_devices <- rep[basename(rep) == "Devices.xml"]
      path_sample <- rep[basename(rep) == "sample_info.xml"]
      if (length(path_sample) == 1){
        meta_sample <- xml2::read_xml(path_sample)
        name <- xml2::xml_text(xml2::xml_find_all(meta_sample, xpath = "//Name"))
        meta_sample <- as.list(xml2::xml_text(
          xml2::xml_find_all(meta_sample, xpath = "//Value")
        ))
        names(meta_sample) <- name
      }
      if (length(path_devices) == 1){
        meta_devices <- xml2::read_xml(path_devices)
        name <- xml_text(xml_find_all(meta_devices, xpath = "//Name"))
        meta_devices <- as.character(xml_text(
          xml_find_all(meta_devices, xpath = "//ModelNumber")
        ))
        names(meta_devices) <- name
      }
      if (!is.null(meta_sample) && !is.null(meta_devices)){
        meta_sample$Instrument <- meta_devices
      }
    }
  }
  meta_sample
}

#' @name read_waters_metadata
#' @param file file
#' @return A list containing extracted metadata.
#' @author Ethan Bass
#' @noRd
read_waters_metadata <- function(file){
  ll <- readLines(file, n = 2)
  ll <- iconv(ll,from = "ISO-8859-1", to = "UTF-8")
  meta <- gsub("\\\"", "", do.call(cbind, strsplit(ll, "\t")))
  rownames(meta) <- meta[, 1]
  meta <- as.list(meta[, -1])
}


#' Extract metadata
#'
#' Extract metadata as a `data.frame`, `data.table` or `tibble` from a list of
#' chromatograms.
#'
#' @param chrom_list A list of chromatograms with attached metadata (as returned
#' by `read_chroms` with `read_metadata = TRUE`).
#' @param what A character vector specifying the metadata elements to
#' extract. Defaults to every field chromConverter attaches; no format
#' records all of them, so the elements a format does not provide are
#' simply absent from the result. Superseded names (`injection_volume`,
#' `software_name`, `time_start`) are accepted and mapped to the names
#' that replaced them.
#' @param detector A character vector of detectors to include (e.g. `"UV"` or
#' `c("UV", "MS")`), matched case-insensitively against each chromatogram's
#' `detector` attribute. Defaults to `NULL`, in which case all chromatograms
#' are included. Useful for lists containing more than one detector per
#' sample.
#' @param format_out Format of object. Either `data.frame`, `data.table` or
#' `tibble`.
#' @return A `data.frame`, `tibble`, or `data.table` (according to the value of
#' `format_out`), with samples as rows and the specified metadata elements as
#' columns.
#' @export
extract_metadata <- function(chrom_list,
                             what = chrom_metadata_fields(),
                             detector = NULL,
                             format_out = c("data.frame", "data.table", "tibble")
){
  defaulted <- identical(what, chrom_metadata_fields())
  what <- resolve_metadata_fields(what)
  if (inherits(chrom_list, c("matrix", "data.table", "data.frame"))){
    chrom_list <- list(chrom_list)
    use_names <- FALSE
  } else use_names <- TRUE
  chrom_list <- flatten_chrom_list(chrom_list, inherit = TRUE)
  if (!is.null(detector)){
    chrom_list <- filter_by_detector(chrom_list, detector)
  }
  format_out <- match.arg(format_out, c("data.frame", "data.table", "tibble"))
  metadata <- purrr::imap_dfr(chrom_list, function(chrom, name){
    c(name = name, unlist(sapply(what, function(w){
      val <- attr(chrom, which = w, exact = TRUE)
      if (w == "run_datetime" && length(val) > 1) val <- val[1]
      val
    }, simplify = FALSE)))
  })
  missing <- what[which(!(what %in% colnames(metadata)))]
  if (nrow(metadata) == 0){
    stop("The specified metadata elements were not found")
  }
  if (!defaulted && length(missing) > 0){
    warning(sprintf("The following metadata elements were not found: %s.",
                    paste(sQuote(missing),collapse = ", ")),immediate. = TRUE)
  }
  if (use_names && ncol(metadata) == 1) {
    return(NA)
  }
  if (any(colnames(metadata) == "run_datetime")){
    metadata$run_datetime <- as.POSIXct(as.numeric(metadata$run_datetime),
                                        tz = "UTC")
  }
  if (!use_names){
    metadata <- metadata[,-1]
  }
  if (format_out == "data.frame"){
    metadata <- as.data.frame(metadata, row.names = NULL)
  } else if (format_out == "data.table"){
    data.table::setDT(metadata)
  }
  metadata
}

#' Enumerate the individual chromatograms in a (possibly nested) list
#'
#' A `chrom_list` element can itself be a list of chromatograms, e.g. one entry
#' per detector for a multichannel file, or one entry per data file within each
#' detector when `read_chroms` is called with `collapse = FALSE`. This function
#' walks the list to whatever depth is needed and returns one entry per actual
#' chromatogram, together with the path taken to reach it.
#'
#' Both `extract_metadata` and `print.chrom_list` are built on this, so that
#' they cannot disagree about how many chromatograms a list contains.
#'
#' Each leaf also carries the attributes of the lists it was reached through
#' (`inherited`), since a parser may attach sample-level metadata to the list
#' holding the traces rather than to the traces themselves --
#' `read_agilent_rslt` writes the acaml fields this way.
#'
#' @return A list of
#' `list(path = <character vector>, chrom = <object>, inherited = <list>)`.
#' @noRd
chrom_list_leaves <- function(x, path = character()){
  if (inherits(x, "chromconverter_metadata")) return(list())
  if (!is.list(x) || inherits(x, c("matrix", "data.table", "data.frame"))){
    return(list(list(path = path, chrom = x, inherited = list())))
  }
  if (length(x) == 0) return(list())
  nms <- names(x)
  if (is.null(nms)) nms <- rep("", length(x))
  nms[!nzchar(nms)] <- seq_along(x)[!nzchar(nms)]
  leaves <- unlist(lapply(seq_along(x), function(i){
    chrom_list_leaves(x[[i]], c(path, nms[i]))
  }), recursive = FALSE)
  for (nm in names(list_metadata_attrs(x))){
    val <- attr(x, nm, exact = TRUE)
    if (is.null(usable_attr(val))) next
    vals <- unique(Filter(Negate(is.null), lapply(leaves, function(l){
      usable_attr(attr(l$chrom, nm, exact = TRUE))
    })))
    if (length(vals) > 1) next
    for (i in seq_along(leaves)){
      if (is.null(leaves[[i]]$inherited[[nm]])){
        leaves[[i]]$inherited[[nm]] <- val
      }
    }
  }
  leaves
}

#' Attributes of a list of chromatograms that describe the data
#'
#' Everything except the bookkeeping attributes that say how the list itself is
#' put together.
#' @noRd
list_metadata_attrs <- function(x){
  a <- attributes(x)
  a[!(names(a) %in% c(bookkeeping_attrs(), "comment", "acaml_metadata"))]
}

#' Flatten a (possibly nested) list of chromatograms
#'
#' Nested chromatograms are named for the path taken to reach them, so a
#' multichannel sample `blue` with a `UV` channel becomes `blue.UV`.
#'
#' @param inherit Whether to apply the attributes of the enclosing lists to
#' each chromatogram. `chrom_list_leaves` decides which may be applied: a list
#' describes the sample, so where its traces agree about a field it is the
#' better source (`read_agilent_rslt` has the method's name from the `acaml`
#' file, while every trace has the path to the method file), but where they
#' disagree the field belongs to the trace and is left alone.
#' @noRd
flatten_chrom_list <- function(x, inherit = FALSE){
  leaves <- chrom_list_leaves(x)
  chroms <- lapply(leaves, function(l){
    chrom <- l$chrom
    if (inherit){
      # `chrom_list_leaves` has already dropped the values that must not be
      # applied, so what is left describes the sample better than the trace does
      for (nm in names(l$inherited)) attr(chrom, nm) <- l$inherited[[nm]]
    }
    chrom
  })
  stats::setNames(chroms,
                  vapply(leaves, function(l) paste(l$path, collapse = "."),
                         character(1)))
}

#' Resolve a sample-level attribute from a chromatogram or a list of them
#'
#' `read_chroms` needs per-sample values like `sample_name` and `run_datetime`,
#' but a sample may be a single chromatogram or a (possibly nested) list of
#' them, and parsers only ever attach metadata to the individual traces. Look on
#' the element itself first -- `read_agilent_rslt` writes acaml attributes
#' directly onto whatever `read_agilent_dx` returned, which may be a list --
#' then fall back to the first leaf that carries a usable value.
#' @noRd
get_sample_attr <- function(x, which){
  vals <- sample_attr_values(x, which, first_only = TRUE)
  if (length(vals) == 0) return(NULL)
  vals[[1]]
}

#' Collect the usable values of a sample-level attribute
#'
#' An attribute on the element itself describes the whole sample, so it is used
#' on its own. Otherwise every leaf is a candidate: with `first_only = TRUE` the
#' search stops at the first usable value (what `get_sample_attr` wants), and
#' otherwise all of them are returned so the caller can check that the traces
#' making up a sample agree.
#'
#' @return A list of length-1 values, empty if the attribute is nowhere to be
#' found.
#' @noRd
sample_attr_values <- function(x, which, first_only = FALSE){
  val <- usable_attr(attr(x, which, exact = TRUE))
  if (!is.null(val)) return(list(val))
  vals <- list()
  for (leaf in chrom_list_leaves(x)){
    val <- usable_attr(attr(leaf$chrom, which, exact = TRUE))
    if (!is.null(val)){
      vals <- c(vals, list(val))
      if (first_only) break
    }
  }
  vals
}

#' Reduce an attribute to a single usable value, or `NULL` if it has none
#' @noRd
usable_attr <- function(val){
  if (length(val) == 0) return(NULL)
  val <- val[[1]]
  if (!is.atomic(val) || length(val) != 1 || is.na(val)) return(NULL)
  if (is.character(val) && !nzchar(trimws(val))) return(NULL)
  val
}

#' Subset a list of chromatograms by detector
#'
#' Matches against the `detector` attribute rather than the name of the list
#' element, since the two do not always agree: the `rainbow` parser happens to
#' name its sub-lists after the detector (`MS`, `UV`, `CAD`), but other parsers
#' name them after the kind of data (e.g. `pda`, `tic`, `chroms`).
#'
#' Chromatograms with no `detector` attribute cannot match and are dropped.
#' Called by `extract_metadata`.
#' @noRd
filter_by_detector <- function(chrom_list, detector){
  detectors <- lapply(chrom_list, attr, "detector", exact = TRUE)
  keep <- vapply(detectors, function(x){
    !is.null(x) && any(tolower(x) %in% tolower(detector))
  }, logical(1))
  if (!any(keep)){
    found <- unique(unlist(detectors))
    stop(sprintf(paste0("No chromatograms were found for the requested ",
                        "detector(s): %s.\n%s"),
                 paste(sQuote(detector), collapse = ", "),
                 if (length(found) == 0)
                   "The chromatograms do not have a 'detector' attribute." else
                   sprintf("The following detector(s) are present: %s.",
                           paste(sQuote(found), collapse = ", "))),
         call. = FALSE)
  }
  chrom_list[keep]
}

#' Transfer metadata
#'@noRd
transfer_metadata <- function (new_object, old_object,
                               exclude = bookkeeping_attrs()){
  a <- attributes(old_object)
  a[exclude] <- NULL
  attributes(new_object) <- c(attributes(new_object), a)
  new_object
}

#' Get 'Shimadzu' Wavelength
#' @noRd
get_sz_wv <- function(meta){
  if ("WVB" %in% names(meta)){
    c(get_metadata_field(meta, "WVB"),
      get_metadata_field(meta, "WVE"))
  } else{
    get_metadata_field(meta, "ADN")
  }
}

#' Extract ASM wavelength from metadata list
#' @author Ethan Bass
#' @noRd
get_asm_wavelength <- function(meta, lab = "absorbance_wavelength_setting.value"){
  wv_idx <- grep(lab, names(meta$`device control aggregate document`))
  unique(unlist(meta$`device control aggregate document`[wv_idx]))
}

#' Date-time formats used by 'MassHunter' (`sample_info.xml`)
#'
#' Note the literal `Z` rather than `%z`, which does not accept the military
#' `Z` designator for UTC.
#' @noRd
masshunter_datetime_formats <- c("%Y-%m-%dT%H:%M:%SZ", "%Y-%m-%dT%H:%M:%OS",
                                 "%m/%d/%Y %I:%M:%S %p")

#' Convert date-time string to POSIXct
#' @author Ethan Bass
#' @noRd
convert_timestamp <- function(string, datetime_formats){
  if (length(string) == 0 || all(is.na(string))){
    return(.POSIXct(NA_real_, tz = "UTC"))
  }
  tryCatch({
    as.POSIXct(string, tz = "UTC", tryFormats = datetime_formats)
  }, error = function(cond){
    warning("Run date-time could not be converted to POSIXct format, returning string instead.")
    string
  })
}

