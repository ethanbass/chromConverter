#' Read 'Allotrope Simple Model' (ASM) files
#'
#' Reads chromatograms, mass spectra and peak lists from
#' ['Allotrope Simple Model'](https://allotropefoundation.org/our-technology/)
#' chromatography files into R.
#'
#' Retention times are returned in the unit the file declares, which is
#' recorded in the `time_unit` attribute.
#'
#' A file can hold several injections, such as a sequence exported from a
#' chromatography data system. Each injection is then returned as a separate
#' sample, named by its sample name, and [read_chroms] adds each one to the
#' list of samples it returns.
#'
#' Mass spectra have so far only been tested against the example files
#' published by the Allotrope Foundation.
#'
#' @inheritParams shared_params
#' @param path Path to ASM `.json` file.
#' @param what What to read: 2D chromatograms (`chroms`), mass spectra
#' (`MS1`), peak lists (`peak_table`) and/or instrument traces (`instrument`),
#' such as pump pressure, flow rate or temperature. Defaults to `chroms` and
#' `MS1`, dropping whichever the file does not contain.
#' @param peaktable_format Whether to return peak tables in `chromatographr`
#' format (`rt`, `start`, `end`, `area` and `height`) or `original` format,
#' with every field the file records under its ASM name.
#' @return A 2D chromatogram in the format specified by `format_out` and
#' `data_format`, or a list of them named by detection type if the file holds
#' more than one (or `collapse = FALSE`). Mass spectra are returned as a
#' `data.frame` (or `data.table`) with columns `rt`, `mz` and `intensity`, and
#' peak tables as a `data.frame` (or `data.table`) per measurement. Instrument
#' traces are returned as 2D chromatograms named by trace. Where more
#' than one of these is returned, they are combined in a list named by `what`.
#' A file with several injections returns a `chrom_list` with one such
#' element per injection. Metadata are attached as [attributes] if
#' `read_metadata` is `TRUE`.
#' @examples \dontrun{
#' read_asm("path/to/file.json")
#' read_asm("path/to/file.json", what = "peak_table")
#' }
#' @author Ethan Bass
#' @export

read_asm <- function(path, what = c("chroms", "MS1"),
                     data_format = c("wide", "long"),
                     format_out = c("matrix", "data.frame", "data.table"),
                     peaktable_format = c("chromatographr", "original"),
                     read_metadata = TRUE,
                     metadata_format = c("chromconverter", "raw"),
                     collapse = TRUE){
  what_given <- !missing(what)
  what <- match.arg(what, c("chroms", "MS1", "peak_table", "instrument"),
                    several.ok = TRUE)
  format_out <- match.arg(format_out, c("matrix", "data.frame", "data.table"))
  data_format <- check_data_format(data_format, format_out)
  peaktable_format <- match.arg(peaktable_format, c("chromatographr", "original"))
  metadata_format <- check_metadata_format(metadata_format, "asm")

  xx <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  agg <- xx[[grep("aggregate document$", names(xx))[1]]]
  docs <- agg[[grep("chromatography document$", names(agg))[1]]]
  meas <- get_asm_measurements(docs)
  if (length(meas) == 0){
    stop("No measurement documents could be found.")
  }
  file_meta <- list(file_version = xx$`$asm.manifest`,
                    device = agg[["device system document"]])
  injections <- vapply(meas, `[[`, character(1), "injection")
  groups <- split(meas, factor(injections, levels = unique(injections)))
  samples <- lapply(groups, read_asm_injection, what = what,
                    data_format = data_format, format_out = format_out,
                    peaktable_format = peaktable_format,
                    read_metadata = read_metadata,
                    metadata_format = metadata_format, collapse = collapse,
                    file_meta = file_meta, path = path)
  names(samples) <- name_asm_samples(groups)
  found <- unique(unlist(lapply(samples, names)))
  if (length(found) == 0){
    stop("None of the requested data (", paste(sQuote(what), collapse = ", "),
         ") could be found.")
  }
  missing_what <- setdiff(what, found)
  if (what_given && length(missing_what) > 0){
    warning("The following could not be found: ",
            paste(sQuote(missing_what), collapse = ", "), ".", call. = FALSE)
  }
  samples <- Filter(length, samples)
  samples <- lapply(samples, function(out){
    dat <- if (length(out) == 1) out[[1]] else out
    if (collapse) dat <- collapse_list(dat)
    dat
  })
  if (length(samples) == 1) return(samples[[1]])
  structure(samples, class = "chrom_list")
}

#' List the measurement documents of an ASM file
#'
#' Each measurement is returned with the scalar fields and documents of its
#' parent chromatography document, which some releases use for the sample and
#' injection, and the identifier of its injection.
#' @noRd
get_asm_measurements <- function(docs){
  unlist(lapply(seq_along(docs), function(i){
    doc <- docs[[i]]
    mad <- doc[["measurement aggregate document"]]
    mds <- mad[["measurement document"]]
    doc <- doc[names(doc) != "measurement aggregate document"]
    doc[["diagnostic trace aggregate document"]] <-
      mad[["diagnostic trace aggregate document"]]
    lapply(mds, function(md){
      injection <- md[["injection document"]][["injection identifier"]] %||%
        doc[["injection document"]][["injection identifier"]] %||%
        paste("document", i)
      list(md = md, doc = doc, injection = as.character(injection))
    })
  }), recursive = FALSE)
}

#' Read the measurements of one ASM injection
#' @return A list holding whichever of `chroms`, `MS1` and `peak_table` were
#' found.
#' @noRd
read_asm_injection <- function(meas, what, data_format, format_out,
                               peaktable_format, read_metadata,
                               metadata_format, collapse, file_meta, path){
  table_format <- check_format_out_table(format_out)
  cubes_2d <- vapply(meas, function(m) find_asm_cube(m$md, n_dim = 1),
                     character(1))
  cubes_3d <- vapply(meas, function(m) find_asm_cube(m$md, n_dim = 2),
                     character(1))
  cubes <- ifelse(is.na(cubes_2d), cubes_3d, cubes_2d)
  rows <- which(!is.na(cubes))
  meas_names <- rep(NA_character_, length(meas))
  meas_names[rows] <- name_asm_chromatograms(meas[rows], cubes[rows])

  add_meta <- function(x, m, cube, format_out, data_format, units = NULL){
    if (!read_metadata) return(x)
    meta <- get_asm_metadata(m, file_meta, cube = cube, units = units)
    attach_metadata(x, meta, format_in = metadata_format,
                    format_out = format_out, data_format = data_format,
                    parser = "chromconverter", source_file = path,
                    source_file_format = "allotrope_simple_model",
                    scale = FALSE)
  }

  out <- list()
  if (any(what == "chroms")){
    rows_2d <- which(!is.na(cubes_2d))
    chroms <- lapply(rows_2d, function(i){
      x <- read_asm_2d_data(meas[[i]]$md[[cubes_2d[i]]],
                            data_format = data_format, format_out = format_out)
      if (length(rows_2d) > 1 && format_out %in% c("data.frame", "data.table"))
        x$detector <- meas_names[i]
      add_meta(x, meas[[i]], cubes_2d[i], format_out, data_format)
    })
    names(chroms) <- meas_names[rows_2d]
    if (length(chroms) > 0) out$chroms <- chroms
  }
  if (any(what == "MS1")){
    rows_3d <- which(!is.na(cubes_3d))
    ms <- lapply(rows_3d, function(i){
      x <- read_asm_ms_data(meas[[i]]$md[[cubes_3d[i]]],
                            format_out = table_format)
      if (!is.null(x)){
        x <- add_meta(x, meas[[i]], cubes_3d[i], table_format, "long")
      }
      x
    })
    names(ms) <- meas_names[rows_3d]
    ms <- Filter(Negate(is.null), ms)
    if (length(ms) > 0) out$MS1 <- if (collapse) collapse_list(ms) else ms
  }
  if (any(what == "peak_table")){
    peaks <- lapply(rows, function(i){
      peaks <- unlist(lapply(find_asm_peak_lists(meas[[i]]$md), `[[`, "peak"),
                      recursive = FALSE)
      if (length(peaks) == 0) return(NULL)
      tab <- asm_peak_table(peaks, peaktable_format)
      units <- attr(tab, "units")
      attr(tab, "units") <- NULL
      if (table_format == "data.table") tab <- data.table::as.data.table(tab)
      add_meta(tab, meas[[i]], NULL, table_format, "long", units = units)
    })
    names(peaks) <- meas_names[rows]
    peaks <- Filter(Negate(is.null), peaks)
    if (length(peaks) > 0){
      out$peak_table <- if (collapse) collapse_list(peaks) else peaks
    }
  }
  if (any(what == "instrument")){
    traces <- find_asm_traces(meas)
    instrument <- lapply(traces, function(tr){
      x <- read_asm_2d_data(tr$cube, data_format = data_format,
                            format_out = format_out)
      cs <- tr$cube[["cube-structure"]]
      units <- list(time = cs[["dimensions"]][[1]][["unit"]],
                    y = cs[["measures"]][[1]][["unit"]])
      add_meta(x, tr$m, NULL, format_out, data_format, units = units)
    })
    if (length(instrument) > 0){
      out$instrument <- if (collapse) collapse_list(instrument) else instrument
    }
  }
  out
}

#' Find the instrument traces of an ASM injection
#'
#' Traces such as pressure, flow rate and temperature are stored as data cubes
#' in diagnostic trace documents, named by their description, or in device
#' control documents, named by their label. A trace repeated in each
#' measurement of an injection is returned once.
#' @return A list of traces named by trace, each holding the measurement it
#' belongs to (`m`) and its data cube (`cube`).
#' @noRd
find_asm_traces <- function(meas){
  traces <- unlist(lapply(meas, function(m){
    diagnostic <- c(
      m$md[["diagnostic trace aggregate document"]][["diagnostic trace document"]],
      m$doc[["diagnostic trace aggregate document"]][["diagnostic trace document"]])
    device_control <- m$md[["device control aggregate document"]][["device control document"]]
    lapply(c(lapply(diagnostic, function(d) list(d = d, name = d[["description"]])),
             lapply(device_control, function(d) list(d = d, name = NULL))),
           function(src){
             cubes <- names(src$d)[grepl("data cube$", names(src$d))]
             lapply(src$d[cubes], function(cube){
               concepts <- vapply(cube[["cube-structure"]][["dimensions"]],
                                  function(x) x[["concept"]] %||% NA_character_,
                                  character(1))
               if (length(concepts) != 1 || !concepts %in% asm_time_concepts){
                 return(NULL)
               }
               list(m = m, cube = cube,
                    name = src$name %||% cube[["label"]] %||% "trace")
             })
           })
  }), recursive = FALSE)
  traces <- Filter(Negate(is.null), unlist(traces, recursive = FALSE))
  repeated <- duplicated(lapply(traces, function(tr) tr[c("name", "cube")]))
  traces <- traces[!repeated]
  names(traces) <- make.unique(vapply(traces, `[[`, character(1), "name"),
                               sep = " ")
  traces
}

#' Concepts of the first dimension of an ASM chromatogram
#' @noRd
asm_time_concepts <- c("retention time", "acquisition time", "retention volume")

#' Find a data cube in an ASM measurement document
#'
#' Chromatograms may be stored as `mass chromatogram data cube` or
#' `three-dimensional mass spectrum data cube` rather than
#' `chromatogram data cube`, and other cubes hold spectra rather than
#' chromatograms, so cubes are told apart by their dimensions rather than by
#' name.
#' @param n_dim Number of dimensions: `1` for a chromatogram, `2` for mass
#' spectra.
#' @return The name of the cube, or `NA` if the measurement has none.
#' @noRd
find_asm_cube <- function(md, n_dim = 1){
  for (cube in grep("data cube$", names(md), value = TRUE)){
    dims <- md[[cube]][["cube-structure"]][["dimensions"]]
    concepts <- vapply(dims, function(d) d[["concept"]] %||% NA_character_,
                       character(1))
    if (length(concepts) == n_dim && concepts[1] %in% asm_time_concepts &&
        (n_dim == 1 || identical(concepts[2], "m/z"))){
      return(cube)
    }
  }
  NA_character_
}

#' Read ASM 2D data
#' @author Ethan Bass
#' @noRd
read_asm_2d_data <- function(cube, data_format, format_out){
  dat <- cube[["data"]]
  format_2d_chromatogram(rt = asm_dimension_values(dat[["dimensions"]][[1]]),
                         int = asm_numeric(dat[["measures"]][[1]]),
                         data_format = data_format, format_out = format_out)
}

#' Read ASM mass spectra
#'
#' Mass spectra are stored as `points`, a list of `(rt, m/z, intensity)`
#' tuples. Some files group the tuples by scan instead, in which case the
#' retention time is taken from the scan's position along the first dimension.
#' @return A `data.frame` (or `data.table`) with columns `rt`, `mz` and
#' `intensity`, or `NULL` if the cube holds no points.
#' @noRd
read_asm_ms_data <- function(cube, format_out = "data.frame"){
  dat <- cube[["data"]]
  points <- dat[["points"]]
  by_scan <- any(vapply(points, function(p) length(p) > 0 && is.list(p[[1]]),
                        logical(1)))
  tuples <- if (by_scan) unlist(points, recursive = FALSE) else points
  if (length(tuples) == 0 || any(lengths(tuples) != 3)) return(NULL)
  x <- matrix(asm_numeric(unlist(tuples, recursive = FALSE)), ncol = 3,
              byrow = TRUE)
  if (by_scan){
    rt <- asm_dimension_values(dat[["dimensions"]][[1]])
    n <- lengths(points)
    if (length(rt) == length(n)) x[, 1] <- rep(rt, n)
  }
  x <- data.frame(rt = x[, 1], mz = x[, 2], intensity = x[, 3])
  if (format_out == "data.table") data.table::setDT(x)
  x
}

#' Convert an ASM array to numeric, keeping `null` as `NA`
#' @noRd
asm_numeric <- function(x){
  x[vapply(x, is.null, logical(1))] <- NA
  as.numeric(unlist(x))
}

#' Values along an ASM data cube dimension
#'
#' A dimension is either an explicit array of values or a function giving its
#' `start`, increment (`incr`) and `length`.
#' @noRd
asm_dimension_values <- function(d){
  if ("length" %in% names(d)){
    start <- d[["start"]] %||% 1
    incr <- d[["incr"]] %||% 1
    return(start + incr * (seq_len(d[["length"]]) - 1))
  }
  asm_numeric(d)
}

#' Find peak lists in an ASM measurement document
#'
#' Peak lists have moved between releases (under `processed data aggregate
#' document`, `processed data document`, or directly in the measurement
#' document), so they are found by name.
#' @noRd
find_asm_peak_lists <- function(x){
  if (!is.list(x)) return(list())
  nms <- names(x)
  if (is.null(nms)) nms <- rep("", length(x))
  out <- x[nms == "peak list"]
  rest <- x[nms != "peak list" & !grepl("data cube$", nms)]
  c(out, unlist(lapply(rest, find_asm_peak_lists), recursive = FALSE))
}

#' Convert ASM peaks to a peak table
#'
#' Each peak is a list of fields, holding either a plain value or a
#' `value`/`unit` pair. Peaks need not share the same fields.
#' @noRd
asm_peak_table <- function(peaks, peaktable_format = "chromatographr"){
  fields <- unique(unlist(lapply(peaks, names)))
  vals <- lapply(fields, function(field) lapply(peaks, `[[`, field))
  names(vals) <- fields
  units <- vapply(vals, function(v){
    unit <- unlist(lapply(Filter(is.list, v), `[[`, "unit"))
    if (length(unit) > 0) unit[1] else NA_character_
  }, character(1))
  cols <- lapply(vals, function(v){
    quantity <- vapply(v, is.list, logical(1))
    v[quantity] <- lapply(v[quantity], `[[`, "value")
    v[vapply(v, is.null, logical(1))] <- NA
    v <- unlist(v)
    if (any(quantity)) suppressWarnings(as.numeric(v)) else v
  })
  tab <- as.data.frame(cols, check.names = FALSE)
  rt_field <- intersect(c("retention time", "retention volume"), fields)[1]
  if (peaktable_format == "chromatographr"){
    get_col <- function(field){
      if (!is.na(field) && field %in% fields) tab[[field]] else NA_real_
    }
    tab <- data.frame(rt = get_col(rt_field),
                      start = get_col("peak start"),
                      end = get_col("peak end"),
                      area = get_col("peak area"),
                      height = get_col("peak height"))
  }
  attr(tab, "units") <- list(time = unname(units[rt_field]),
                             y = unname(units["peak height"]))
  tab
}

#' Name ASM chromatograms
#'
#' Chromatograms are named by detection type, falling back to the cube label
#' where more than one shares a detection type or the file records none.
#' @noRd
name_asm_chromatograms <- function(meas, cubes){
  detectors <- vapply(meas, function(m){
    m$md[["detection type"]] %||% NA_character_
  }, character(1))
  labels <- trimws(vapply(seq_along(meas), function(i){
    meas[[i]]$md[[cubes[i]]][["label"]] %||% NA_character_
  }, character(1)))
  dup <- duplicated(detectors) | duplicated(detectors, fromLast = TRUE)
  use_label <- (dup | is.na(detectors)) & !is.na(labels) & labels != ""
  detectors[is.na(detectors) & !use_label] <- "chromatogram"
  detectors[use_label] <- labels[use_label]
  make.unique(detectors, sep = " ")
}

#' Name the injections of an ASM file by their sample names
#' @noRd
name_asm_samples <- function(groups){
  nms <- vapply(groups, function(meas){
    m <- meas[[1]]
    smp <- m$md[["sample document"]] %||% m$doc[["sample document"]]
    smp[["written name"]] %||% smp[["sample identifier"]] %||% m$injection
  }, character(1))
  make.unique(unname(nms), sep = "_")
}

#' Collect the metadata of an ASM measurement
#'
#' The sample, injection, column and device control documents are taken from
#' the measurement, or from its chromatography document where an older release
#' keeps them there, and flattened into named lists.
#' @param units Time and intensity units, read from `cube` if not supplied.
#' @noRd
get_asm_metadata <- function(m, file_meta, cube = NULL, units = NULL){
  md <- m$md
  doc <- m$doc
  get_doc <- function(name) md[[name]] %||% doc[[name]]
  device_control <- (get_doc("device control aggregate document") %||%
                       doc[["detector control aggregate document"]])
  device_control <- device_control[[1]]
  if (is.null(units)){
    cs <- md[[cube]][["cube-structure"]]
    units <- list(time = cs[["dimensions"]][[1]][["unit"]],
                  y = cs[["measures"]][[1]][["unit"]])
  }
  c(list(file_version = file_meta$file_version),
    doc[!vapply(doc, is.list, logical(1))],
    list(`device system document` = flatten_asm(file_meta$device),
         `sample document` = flatten_asm(get_doc("sample document")),
         `injection document` = flatten_asm(get_doc("injection document")),
         `chromatography column document` =
           flatten_asm(get_doc("chromatography column document")),
         `device control document` = lapply(device_control, flatten_asm),
         detection_type = md[["detection type"]],
         label = if (!is.null(cube)) md[[cube]][["label"]],
         time_unit = units$time,
         detector_unit = units$y))
}

#' Flatten an ASM document into a named list
#'
#' Nested documents are prefixed with their parent's name, and each
#' `value`/`unit` pair becomes the value and a field suffixed with `unit`.
#' Data cubes are dropped.
#' @noRd
flatten_asm <- function(x, prefix = NULL){
  out <- list()
  for (nm in names(x)){
    if (grepl("data cube$", nm)) next
    v <- x[[nm]]
    key <- if (is.null(prefix)) nm else paste(prefix, nm, sep = ".")
    if (is_asm_quantity(v)){
      out[[key]] <- v[["value"]]
      if (!is.null(v[["unit"]])) out[[paste(key, "unit")]] <- v[["unit"]]
    } else if (is.list(v) && !is.null(names(v))){
      out <- c(out, flatten_asm(v, key))
    } else {
      out[[key]] <- v
    }
  }
  out
}

#' Is an ASM field a `value`/`unit` pair?
#' @noRd
is_asm_quantity <- function(x){
  is.list(x) && "value" %in% names(x) &&
    all(names(x) %in% c("value", "unit", "@type"))
}

#' Values of a field across ASM device control documents
#' @param pattern Regular expression matching the field name.
#' @param exclude Regular expression for field names to leave out.
#' @noRd
asm_device_field <- function(device_control, pattern, exclude = NULL){
  vals <- unlist(lapply(device_control, function(d){
    keep <- grepl(pattern, names(d))
    if (!is.null(exclude)) keep <- keep & !grepl(exclude, names(d))
    d[keep]
  }), use.names = FALSE)
  if (length(vals) > 0) unique(vals)
}

#' Search ASM metadata list for string
#' @noRd
search_metadata <- function(x, str){
  x[grep(str, names(x))]
}

#' Minify an ASM file
#'
#' Strips the whitespace from an ASM `.json` file, which is often most of its
#' size, without parsing its contents, so numbers keep their exact text.
#' @param path Path to ASM `.json` file.
#' @param path_out Path to write the minified file to. Defaults to `path`.
#' @return `path_out`, invisibly.
#' @noRd
minify_asm <- function(path, path_out = path){
  txt <- paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  writeLines(jsonlite::minify(txt), path_out, useBytes = TRUE)
  invisible(path_out)
}
