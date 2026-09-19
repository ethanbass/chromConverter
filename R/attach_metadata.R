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
#' @param collapse Logical. Whether to collapse a field holding more than one
#' value (`time_range`, or the `product_mz` of an MRM event monitoring several
#' transitions) into a single comma-separated string. Defaults to `FALSE`, in
#' which case such a field is spread across numbered columns
#' (`time_range1`, `time_range2`).
#' @param expand Whether to include the nested metadata fields, whose value is
#' itself a list or table rather than a single value per chromatogram: the
#' `ms_params` instrument settings, the `acaml_metadata` injection record that
#' `read_agilent_rslt` reads from the `.acaml` file, or the whole vendor list
#' that `metadata_format = "raw"` passes through. Either `TRUE`, to include
#' every nested field the chromatograms carry, a character vector naming the
#' ones to include, or `FALSE` (the default) to include none. Each element
#' becomes a column of its own, named for itself (`SampleName`) unless that
#' name is already taken, in which case it carries the field it came from
#' (`ms_params.polarity`, since `polarity` is a metadata field in its own
#' right).
#' @return A `data.frame`, `tibble`, or `data.table` (according to the value of
#' `format_out`), with samples as rows and the specified metadata elements as
#' columns, or `NA` if none of the specified elements could be found.
#' @examples
#' path <- system.file("extdata/ladder.txt", package = "chromConverter")
#' chroms <- read_chroms(path, format_in = "shimadzu_ascii",
#'                       find_files = FALSE, progress_bar = FALSE)
#' extract_metadata(chroms, what = c("sample_name", "instrument", "run_datetime"))
#' @export
extract_metadata <- function(chrom_list,
                             what = chrom_metadata_fields(),
                             detector = NULL,
                             format_out = c("data.frame", "data.table",
                                            "tibble"),
                             collapse = FALSE,
                             expand = FALSE
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
  # named explicitly, so a field that is nowhere to be found is worth a warning
  requested <- if (defaulted) character() else what
  all_nested <- unique(unlist(lapply(chrom_list, nested_metadata_attrs)))
  if (isTRUE(expand)){
    expand <- all_nested
  } else if (isFALSE(expand) || is.null(expand)){
    expand <- character()
  } else if (is.character(expand)){
    expand <- resolve_metadata_fields(expand)
    requested <- c(requested, expand)
  } else {
    stop("`expand` must be TRUE, FALSE, or a character vector of field names.",
         call. = FALSE)
  }
  what <- union(what, expand)
  # a nested field reached through `what` is expanded and named just as one
  # reached through `expand` is, so the two ways of asking for it agree. Which
  # is why this is derived from `what` rather than from `expand`, whose members
  # need not be nested at all.
  nested <- intersect(what, all_nested)
  taken <- expand_taken_names(chrom_list, nested, all_nested)
  metadata <- purrr::imap_dfr(chrom_list, function(chrom, name){
    c(name = name, unlist(lapply(what, function(w){
      val <- attr(chrom, which = w, exact = TRUE)
      if (w == "run_datetime"){
        if (length(val) > 1) val <- val[1]
        # `flatten_metadata_field` would format it, but the column is converted
        # back to `POSIXct` below, which keeps the full precision
        if (inherits(val, "POSIXt")) val <- as.numeric(val)
      }
      if (!w %in% nested) return(flatten_metadata_field(val, w, collapse))
      out <- flatten_metadata_field(val, "", collapse)
      # a nested field is nested across the whole list, but an individual
      # chromatogram need not carry it: a UV trace has no `ms_params`
      if (length(out) == 0) return(NULL)
      hit <- names(out) %in% taken[[w]]
      names(out)[hit] <- paste(w, names(out)[hit], sep = ".")
      out
    })))
  })
  # a field is present if any chromatogram carries it, which is not the same as
  # its name appearing in `metadata`: a multi-valued field is spread over
  # `product_mz1`, `product_mz2`, ... and would otherwise be reported missing
  missing <- what[!vapply(what, function(w){
    any(vapply(chrom_list, function(chrom)
      !is.null(attr(chrom, which = w, exact = TRUE)), logical(1)))
  }, logical(1))]
  if (nrow(metadata) == 0){
    stop("The specified metadata elements were not found")
  }
  missing <- intersect(missing, requested)
  if (length(missing) > 0){
    warning(sprintf("The following metadata elements were not found: %s.",
                    paste(sQuote(missing),collapse = ", ")),immediate. = TRUE)
  }
  # only the `name` column, so nothing was found -- answered the same way
  # whether the input was a list or a single chromatogram
  if (ncol(metadata) == 1) {
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

#' Reduce a metadata field to one named value per column
#'
#' A field may hold a single value (`sample_name`), several (`time_range`, or
#' the `product_mz` of an MRM event monitoring several transitions), or a
#' nested list or table (`ms_params`, `acaml_metadata`). Walk it to whatever
#' depth is needed and name each value for the path taken to reach it, so that
#' `extract_metadata` can `unlist` the result into a row.
#'
#' Several values under one name are left as they are for `unlist` to spread
#' over numbered columns, unless `collapse` is `TRUE`. Note this happens per
#' name: collapsing `ms_params` pastes together the values of each of its
#' elements, not the elements themselves.
#'
#' A timestamp is formatted here, since `unlist` would otherwise drop its class
#' and leave a bare number in the column. `extract_metadata` keeps `run_datetime`
#' out of this, converting the whole column back to `POSIXct` once it is built.
#'
#' @param name What to call the field. An empty string names the values of a
#' nested field for their own elements alone (`SampleName`), rather than for
#' the field they came from (`acaml_metadata.SampleName`); `extract_metadata`
#' decides between the two with its `prefix` argument.
#' @return A named list of atomic values, empty if the field holds nothing.
#' @noRd
flatten_metadata_field <- function(val, name, collapse = FALSE){
  if (length(val) == 0) return(NULL)
  if (inherits(val, "POSIXt")){
    val <- format(val, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  } else if (inherits(val, "Date")) val <- format(val)
  if (!is.list(val)){
    if (collapse && length(val) > 1) val <- paste(val, collapse = ", ")
    return(stats::setNames(list(val), name))
  }
  nms <- names(val)
  if (is.null(nms)) nms <- rep("", length(val))
  nms[!nzchar(nms)] <- seq_along(val)[!nzchar(nms)]
  if (nzchar(name)) nms <- paste(name, nms, sep = ".")
  unlist(lapply(seq_along(val), function(i){
    flatten_metadata_field(val[[i]], nms[i], collapse)
  }), recursive = FALSE)
}

#' Which elements of an expanded field cannot be named for themselves?
#'
#' An expanded field's elements are better read as themselves -- `SampleName`
#' says as much as `acaml_metadata.SampleName` and is easier to type -- but
#' only where the shorter name is unambiguous. An element may share a name with
#' a metadata field of chromConverter's own (`ms_params` has a `polarity`, and
#' the vendor list that `metadata_format = "raw"` passes through shares most of
#' its names), with an element of another nested field, or with the `name`
#' column naming the chromatogram itself. Those elements are prefixed and the
#' rest are not, so the prefix marks exactly the ambiguous columns.
#'
#' The comparison is against the whole vocabulary and every nested field the
#' chromatograms carry, rather than the fields `what` happens to ask for, so
#' that the columns of a field are named the same way however it was requested.
#' `extract_metadata` has scanned for those already and passes them in as
#' `all_nested`.
#'
#' @return A list of character vectors, named by `fields`.
#' @noRd
expand_taken_names <- function(chrom_list, fields, all_nested){
  element_names <- function(field){
    unique(unlist(lapply(chrom_list, function(chrom){
      # `collapse` cannot change these names, only how many values sit under
      # each of them, so it does not matter which way it is set here
      names(flatten_metadata_field(attr(chrom, field, exact = TRUE), ""))
    })))
  }
  elements <- lapply(stats::setNames(nm = all_nested), element_names)
  vocabulary <- c("name", chrom_metadata_fields(), .metadata_extra_fields)
  lapply(stats::setNames(nm = fields), function(field){
    intersect(elements[[field]], c(vocabulary, unlist(elements[-match(field,
                                                              all_nested)])))
  })
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
    # a list-valued field belongs to the loop below, which checks whether a
    # leaf has one of its own before handing down the container's
    if (is.list(val) || is.null(usable_attr(val))) next
    vals <- unique(Filter(Negate(is.null), lapply(leaves, function(l){
      lv <- attr(l$chrom, nm, exact = TRUE)
      # `usable_attr` says whether the leaf has a value of its own, but whether
      # the leaves agree is decided on the whole value: a `time_range` of
      # `c(0, 10)` and one of `c(0, 20)` are not the same range
      if (is.null(usable_attr(lv))) NULL else lv
    })))
    if (length(vals) > 1) next
    for (i in seq_along(leaves)){
      if (is.null(leaves[[i]]$inherited[[nm]])){
        leaves[[i]]$inherited[[nm]] <- val
      }
    }
  }
  for (nm in nested_metadata_attrs(x)){
    # `usable_attr` keeps only single atomic values, so the loop above cannot
    # see a nested field like the `acaml_metadata` table `read_agilent_rslt`
    # attaches to the list holding the traces. One row describes the whole
    # injection, so carry it down unless a trace has a nested field of its own.
    if (any(vapply(leaves, function(l)
      !is.null(attr(l$chrom, nm, exact = TRUE)), logical(1)))) next
    val <- attr(x, nm, exact = TRUE)
    for (i in seq_along(leaves)){
      if (is.null(leaves[[i]]$inherited[[nm]])){
        leaves[[i]]$inherited[[nm]] <- val
      }
    }
  }
  leaves
}

#' Nested metadata attributes of a chromatogram or a list of them
#'
#' A field whose value is itself a list or table, rather than a single value
#' per chromatogram: the `ms_params` instrument settings, the `acaml_metadata`
#' injection record, or the whole vendor list that `metadata_format = "raw"`
#' passes through. [extract_metadata] reaches these through its `expand`
#' argument, which spreads each of their elements over a column of its own.
#' @noRd
nested_metadata_attrs <- function(x){
  a <- attributes(x)
  a <- a[!(names(a) %in% c(bookkeeping_attrs(), "comment"))]
  names(a)[vapply(a, function(v) is.list(v) && length(v) > 0, logical(1))]
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
    # a detector with no wavelength to report -- a refractive index or
    # conductivity channel -- leaves the channel description empty, and an
    # empty string prints as a blank cell rather than as a missing value
    wv <- get_metadata_field(meta, "ADN")
    if (is.character(wv) && !nzchar(trimws(wv))) NA_character_ else wv
  }
}

#' Get 'Shimadzu' instrument name
#'
#' `SI.IN`, which `read_sz_file_properties` reads from the `SystemInformation`
#' stream: the name the system was given in 'LabSolutions', and the string its ascii exports report as `Instrument Name`
#' (see `read_sz_system_info`). It is the one instrument field that describes
#' the whole file rather than one of its detectors, so every trace read from a
#' file reports the same instrument, and it is the only field that names the
#' model of a triple quadrupole or a Q-TOF.
#'
#' `instrument_config` is the fallback, for a mass spectrometry file with no
#' `SystemInformation` stream. It names the control platform rather than the
#' instrument, which is why it is not preferred.
#' @noRd
sz_instrument <- function(meta){
  name <- get_metadata_field(meta, "SI.IN", null_val = NA)
  if (sz_has_instrument(name)){
    return(name)
  }
  config <- get_metadata_field(meta, "instrument_config", null_val = NA)
  if (sz_has_instrument(config)) config else NA
}

#' Get 'Shimadzu' detector type
#'
#' `DETN`, the detector a trace was read from. 'Shimadzu' calls a photodiode
#' array `PDA` where the rest of the package calls the same device `DAD`, as
#' 'Agilent' does and as the `shimadzu_dad` ascii map already did, so the one
#' acquisition read from a `.lcd` and from its ascii export no longer reports
#' two different detectors. `read_shimadzu_lcd` names the stream `DAD` as well,
#' and accepts `PDA` as a synonym for it.
#'
#' Everything else is passed through as the file gives it. Note that `DETN` is
#' not always a device type: a multichannel LC reports slot labels
#' (`Detector A`, `DET#1`) rather than `UV` or `RID`, and those are left alone
#' for want of anything reliable to map them to --- `detector_model` names the
#' module in those cases.
#' @noRd
sz_detector <- function(meta){
  detector <- get_metadata_field(meta, "DETN")
  if (identical(detector, "PDA")) "DAD" else detector
}

#' Get 'Shimadzu' detector model
#'
#' `DSN`, the unit in the detector slot the trace was read from: `SPD-M20A` for
#' a PDA, `RID-10A` for a refractive index channel, `SFID1` for a GC detector.
#' A mass spectrometry stream leaves it empty, so the unit `SystemInformation`
#' records in the mass spectrometer slot stands in --- a model on newer
#' software (`LCMS-9030`, `LCMS-8050`), and the platform name `LCMS-3030` on
#' older versions, which register every triple quadrupole under it.
#' @noRd
sz_detector_model <- function(meta){
  dsn <- get_metadata_field(meta, "DSN", null_val = NA)
  if (sz_has_instrument(dsn)){
    return(dsn)
  }
  unit <- unname(meta[["SI.units"]]["LCMS-QP"])
  if (sz_has_instrument(unit)) unit else NA
}

#' Is this a usable 'Shimadzu' instrument name?
#' @noRd
sz_has_instrument <- function(x){
  length(x) == 1 && !is.na(x) && is.character(x) && nzchar(trimws(x))
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

