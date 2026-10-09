#' Read 'Agilent ChemStation' log files
#'
#' Reads the event log 'Agilent ChemStation' writes to the folder of each
#' sequence, or the `RUN.LOG` it writes to each `.D` folder when that file is
#' named in `paths`. These record instrument errors, aborted runs and routine
#' readings such as pump pressure and column temperature.
#'
#' Directories in `paths` are searched, including their subdirectories, for
#' sequence logs: every `.LOG` file outside a `.D` folder. `RUN.LOG` files are
#' left out, since the sequence log holds their events. A file found this way
#' that is not a 'ChemStation' log is skipped with a warning; a file named in
#' `paths` that is not one is an error.
#'
#' Tested on logs from 'ChemStation' revisions A.10.02, B.01.03 and B.04.02.
#'
#' @param paths Paths to 'ChemStation' `.LOG` files, or to directories to
#' search for sequence logs.
#' @param what What to return: one row per injection (`injections`, the
#' default), only the events marked as a `problem` (`problems`), or every event
#' (`events`).
#' @param format_out Class of the returned object: `data.frame` (the default)
#' or `data.table`.
#' @return A `data.frame` or `data.table`. For `injections`, one row per
#' injection, with columns:
#'
#' * `sequence`: the name of the 'ChemStation' sequence, taken from the log's
#'   own sequence events or, failing those, from the name of the file. `NA` for
#'   a `RUN.LOG`, which has neither.
#' * `folder`: the folder the sequence's data are in, which can differ from
#'   `sequence`. This is the folder holding the log, or for a `RUN.LOG` the
#'   folder holding its `.D` folder.
#' * `injection`: numbers the injections in the order the log starts them.
#' * `data_file`: the `.D` folder the log names for the injection. The log
#'   records it only once the run's data are analyzed, so an aborted injection
#'   has none, and its case can differ from the folder on disk.
#' * `sample`: the sample, as the log names it.
#' * `start`: `POSIXct`, UTC. The `run_datetime` of the traces and reports in a
#'   `.D` folder is instead the instrument's local time labeled as UTC, so match
#'   injections to them by `data_file` rather than by time.
#' * `minutes`: from the injection's first event to its last.
#' * `status`: `completed`, `aborted`, `stopped by user` or `incomplete`.
#' * `pressure_start`, `pressure_end`: the first and last reading of pump 1, in
#'   bar.
#' * `problems`: the injection's problem messages, separated by `"; "`.
#'
#' For `events`, one row per event, with `sequence`, `folder`, `injection` and
#' `data_file` as above, and:
#'
#' * `time`: `POSIXct`, UTC, as recorded in the event's header. The local time
#'   printed in the log's text is not returned.
#' * `problem`: whether the event reports an alarm from an instrument module
#'   (any event from a module with a non-zero `event_code`, such as a leak or a
#'   shutdown), a method that was aborted, timed out, stopped by an instrument
#'   error or stopped by the user, or a sequence that was terminated or
#'   stopped.
#' * `event_code`, `module_id`: hexadecimal strings, since 'Agilent' does not
#'   document them. The same module always carries the same `module_id` (for
#'   example, `1da6` for the pump in 'Agilent 1100' logs).
#' * `source`: e.g. `"1100 PMP   1"`, `"Method"` or `"Sequence"`.
#' * `message`: the event's text. The log splits a message longer than 45
#'   characters over several events, which are joined, but stores a message of
#'   exactly 45 characters without its last character.
#'
#' Events of the sequence itself, events between injections such as loading
#' the next method, and method runs that acquire no data, such as a
#' data-analysis-only re-run of the sequence, have no `injection` or
#' `data_file`.
#'
#' For `problems`, the events marked as a `problem`, without that column and
#' with `incident`, which numbers the runs of problem events in the same log
#' that are less than a minute apart, such as a leak and the shutdowns that
#' follow it.
#' @author Ethan Bass
#' @examplesIf interactive()
#' read_chemstation_logs("tests/testthat/testdata/chemstation_sequence.LOG")
#' read_chemstation_logs("path/to/sequences", what = "problems")
#' @family 'Agilent' parsers
#' @export

read_chemstation_logs <- function(paths,
                                  what = c("injections", "problems", "events"),
                                  format_out = "data.frame"){
  what <- match.arg(what)
  format_out <- check_format_out_table(format_out)
  found <- lapply(paths, function(p){
    if (!dir.exists(p)) return(NULL)
    f <- list.files(p, pattern = "\\.log$", recursive = TRUE,
                    ignore.case = TRUE)
    f <- f[!grepl("\\.d/", f, ignore.case = TRUE) &
             toupper(basename(f)) != "RUN.LOG"]
    if (!length(f)) stop("No sequence logs found in `", p, "`.", call. = FALSE)
    file.path(p, f)
  })
  searched <- !vapply(found, is.null, TRUE)
  skipped <- character()
  logs <- lapply(seq_along(paths), function(i){
    if (!searched[i]) return(list(read_chemstation_log(paths[i])))
    lapply(found[[i]], function(f){
      tryCatch(read_chemstation_log(f), log_unrecognised = function(e){
        skipped <<- c(skipped, f)
        NULL
      })
    })
  })
  if (length(skipped)){
    warning("Skipped files that are not 'ChemStation' logs: ",
            paste(skipped, collapse = ", "), call. = FALSE)
  }
  logs <- Filter(Negate(is.null), unlist(logs, recursive = FALSE))
  if (!length(logs)) stop("No 'ChemStation' logs found.", call. = FALSE)
  dat <- do.call(rbind, c(logs, make.row.names = FALSE))
  dat <- switch(what, events = dat,
                problems = number_log_incidents(
                  dat[dat$problem, names(dat) != "problem", drop = FALSE]),
                injections = summarize_log_injections(dat))
  dat$file <- NULL
  rownames(dat) <- NULL
  if (format_out == "data.table") data.table::as.data.table(dat) else dat
}

#' Number the incidents among the problem events of 'ChemStation' logs
#'
#' An incident is a run of problem events from the same log, each less than a
#' minute after the one before.
#' @noRd
number_log_incidents <- function(x){
  new <- if (nrow(x)){
    c(TRUE, x$file[-1] != x$file[-nrow(x)] | diff(as.numeric(x$time)) >= 60)
  } else logical()
  x$incident <- cumsum(new)
  cols <- setdiff(names(x), "incident")
  x[, append(cols, "incident", after = match("folder", cols)), drop = FALSE]
}

#' Summarize the events of each injection in a 'ChemStation' log
#' @noRd
summarize_log_injections <- function(x){
  x <- x[!is.na(x$injection), , drop = FALSE]
  groups <- split(seq_len(nrow(x)), paste(x$file, x$injection), drop = TRUE)
  groups <- groups[order(match(names(groups), paste(x$file, x$injection)))]
  if (!length(groups)){
    return(data.frame(file = character(), sequence = character(),
                      folder = character(), injection = integer(),
                      data_file = character(), sample = character(),
                      start = .POSIXct(numeric(), tz = "UTC"),
                      minutes = numeric(), status = character(),
                      pressure_start = numeric(), pressure_end = numeric(),
                      problems = character()))
  }
  do.call(rbind, c(lapply(groups, function(i){
    z <- x[i, , drop = FALSE]
    pump <- grepl("^(1100 PMP +1|PUMP +1)$", z$source) &
      grepl("^Pressure = [0-9.]+ bar$", z$message)
    p <- as.numeric(sub("^Pressure = ([0-9.]+) bar$", "\\1", z$message[pump]))
    sample <- z$message[startsWith(z$message, "Instrument running sample ")]
    data.frame(
      file = z$file[1], sequence = z$sequence[1], folder = z$folder[1],
      injection = z$injection[1], data_file = z$data_file[1],
      sample = if (length(sample)){
        sub("^Instrument running sample ", "", sample[1])
      } else NA_character_,
      start = min(z$time),
      minutes = as.numeric(difftime(max(z$time), min(z$time), units = "mins")),
      status = if (any(z$message == "Method aborted")) "aborted" else
        if (any(z$message == "Method stopped by user")) "stopped by user" else
          if (any(z$message == "Method completed")) "completed" else "incomplete",
      pressure_start = if (length(p)) p[1] else NA_real_,
      pressure_end = if (length(p) > 1) p[length(p)] else NA_real_,
      problems = paste(unique(z$message[z$problem]), collapse = "; "))
  }), make.row.names = FALSE))
}

#' Read a 'ChemStation' text file
#'
#' Decodes UTF-16 where the file opens with a byte order mark, and Latin-1
#' otherwise.
#' @noRd
read_chemstation_text <- function(path){
  bom <- readBin(path, "raw", 2)
  utf16 <- identical(bom, as.raw(c(0xff, 0xfe))) ||
    identical(bom, as.raw(c(0xfe, 0xff)))
  con <- file(path, encoding = if (utf16) "UTF-16" else "latin1")
  on.exit(close(con))
  readLines(con, warn = FALSE)
}

#' Read one 'ChemStation' log file
#' @noRd
read_chemstation_log <- function(path){
  x <- read_chemstation_text(path)
  x <- x[nzchar(trimws(x))]
  header <- grep("^ *[0-9a-f]+ +[0-9a-f]+ +[0-9a-f]{8} +[0-9a-f]+$", x)
  if (length(header) && header[length(header)] == length(x)){
    x <- x[-length(x)]
    header <- header[-length(header)]
  }
  if (length(header) == 0 ||
      !identical(header, seq(1L, length(x), by = 2L))){
    stop(structure(class = c("log_unrecognised", "error", "condition"),
                   list(message = paste0("`", path, "` is not a recognised ",
                                         "'ChemStation' log file."),
                        call = NULL)))
  }
  msg <- x[header + 1L]
  body <- sub("\\s*\\d{2}:\\d{2}:\\d{2} \\d{2}/\\d{2}/\\d{2}$", "",
              substring(msg, 14))
  # a message longer than its 45-character field ends in `>` and continues in
  # the next event
  wrap <- which(nchar(body) == 45 & endsWith(body, ">"))
  wrap <- wrap[wrap < length(body)]
  for (i in rev(wrap)) body[i] <- paste0(substr(body[i], 1, 44), body[i + 1])
  keep <- !seq_along(body) %in% (wrap + 1)
  fields <- strsplit(trimws(x[header[keep]]), " +")
  source <- trimws(substr(msg[keep], 1, 13))
  message <- trimws(body[keep])
  method <- source == "Method"
  started <- method & startsWith(message, "Method started")
  ended <- (method & startsWith(message, "Loading Method")) |
    c(FALSE, head(method & message == "Method completed", -1))
  last_start <- cummax(seq_along(started) * started)
  open <- last_start > 0 & last_start >= cummax(seq_along(ended) * ended)
  injection <- cumsum(started)
  injection[!open | source == "Sequence"] <- NA
  acquired <- startsWith(message, "Instrument running sample ")
  if (any(acquired)){
    injection[!injection %in% injection[acquired]] <- NA
    injection <- match(injection, unique(injection[!is.na(injection)]))
  }
  rawdata <- sub("^Analyzing rawdata ", "", message)
  rawdata[source != "CP Macro" | rawdata == message] <- NA
  named <- which(!is.na(injection) & !is.na(rawdata))
  named <- named[!duplicated(injection[named])]
  data_file <- rawdata[named][match(injection, injection[named])]
  event_code <- vapply(fields, `[`, "", 1)
  module <- !source %in% c("Method", "Sequence", "CP Macro")
  problem <- (module & event_code != "0") |
    (source == "Method" & (message %in% c("Method aborted",
                                          "Method stopped by user") |
                             grepl("^Instrument Error|Timeout occurred$",
                                   message))) |
    (source == "Sequence" &
       grepl("terminated due to an error?$|stopped by user?$", message))
  seq_name <- sub("^(Acquisition for )?(.+)\\.S .*$", "\\2",
                  message[source == "Sequence" &
                            grepl("\\.S ", message)][1])
  if (is.na(seq_name) && toupper(basename(path)) != "RUN.LOG"){
    seq_name <- fs::path_ext_remove(basename(path))
  }
  folder <- dirname(normalizePath(path, mustWork = FALSE))
  if (grepl("\\.d$", folder, ignore.case = TRUE)) folder <- dirname(folder)
  data.frame(
    file = path,
    sequence = seq_name,
    folder = basename(folder),
    time = .POSIXct(strtoi(vapply(fields, `[`, "", 3), 16L), tz = "UTC"),
    injection = injection,
    data_file = data_file,
    problem = problem,
    event_code = event_code,
    module_id = vapply(fields, `[`, "", 2),
    source = source,
    message = message)
}
