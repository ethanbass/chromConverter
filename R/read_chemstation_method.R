#' Read an 'Agilent ChemStation' method
#'
#' Reads the pump, diode array detector, autosampler and column thermostat
#' settings from the module register files (`LPMP1.REG`, `LDAD1.REG`,
#' `LALS1.REG` or `LWLS1.REG`, and `LTHM1.REG`) of a 'ChemStation' `.M` method
#' directory. Revisions that store a module's settings as 'RapidControl' XML
#' (e.g. `AgilentPumpDriver1.RapidControl.MethodXML.xml`) instead are read as
#' 'OpenLab CDS' methods are by [read_agilent_amx].
#'
#' Depending on how 'ChemStation' is configured, a `.D` directory may hold a
#' copy of the method it was acquired with, in `ACQ.M` or, in older revisions,
#' `RUN.M`. `path` can then be the `.D` directory itself. Its `DA.M` holds the
#' data analysis method instead, whose pump settings may not be those the run
#' was acquired with; reading it gives a warning.
#'
#' The gradient follows the pump's timetable, with each parameter changing
#' linearly between the times it is set. Channel A is the pump's primary
#' channel and delivers whatever the other channels leave, so it is always
#' included and its share is computed rather than read.
#'
#' @param path Path to a `.M` directory, or to a `.D` directory holding a copy
#'   of its method.
#' @param what One or more instrument modules to read: any combination of
#'   `"pump"`, `"dad"`, `"sampler"` and `"column"`. Defaults to all four.
#' @param format_out Class of the `solvents`, `gradient`, `signals` and
#'   `temp_controls` tables: `"data.frame"`, `"tibble"` or `"data.table"`.
#' @param gradient_format Whether to return the gradient in `"wide"` (default)
#'   or `"long"` format.
#' @return A named list with one element per module read. A module the method
#'   has no settings for is left out with a warning, and it is an error if
#'   none of the requested modules are present. The `sampler` module is
#'   returned as `autosampler`. The elements are those of [read_agilent_amx],
#'   with `NA` for a setting the method does not record:
#'
#'   **`pump`**: a list with scalar elements `flow_mL_min`, `stop_time_min`,
#'   `post_time_min`, `pressure_low_bar` and `pressure_high_bar`, plus:
#'   * `solvents`: a table of the channels in use, with columns `channel`,
#'     `percentage` (at the start of the run) and `solvent` (the name entered
#'     for it, or `NA`).
#'   * `gradient`: the timetable. Wide format: `time_min` and a `pct_<channel>`
#'     column per channel in use, plus `flow_mL_min` if the timetable sets the
#'     flow. Long format: `time_min`, `channel` and `percent`, where a `flow`
#'     channel holds the flow in mL/min. An empty timetable gives the starting
#'     composition at time 0.
#'
#'   **`dad`**: a list with scalar elements `peakwidth_nm`, `slitwidth_nm`,
#'   `uv_lamp_required`, `vis_lamp_required`, `spectra_from_nm`,
#'   `spectra_to_nm` and `spectra_step_nm`, plus
#'   `signals`, a table of the stored signals with columns `id`,
#'   `wavelength_nm`, `bandwidth_nm`, `reference_nm` and
#'   `reference_bandwidth_nm` (`NA` without a reference).
#'
#'   **`autosampler`**: a list with scalar elements `thermostat_installed`,
#'   `draw_speed_uL_min`, `eject_speed_uL_min`, `wait_after_draw_min`,
#'   `injection_volume_uL` and `wash_time_s`. Register files record only the
#'   speeds and the injection volume.
#'
#'   **`column`**: a list with scalar element `post_time_min` (`NA` when off),
#'   plus `temp_controls`, a two-row table (`Left`, `Right`) with columns
#'   `side`, `temperature_C`, `not_ready_limit_C` and `equilibration_time_min`.
#'   A temperature stored below absolute zero (e.g. -274) is `NA`; this may
#'   mean no temperature was set.
#' @seealso [read_agilent_amx] for 'OpenLab CDS' methods, and [read_agilent_d]
#'   with `what = "instrument"` for the pump's record of what it delivered.
#' @examples \dontrun{
#' method <- read_chemstation_method("path/to/METHOD.M")
#' method$pump$gradient
#' }
#' @author Ethan Bass
#' @family 'Agilent' parsers
#' @importFrom stats approx
#' @export
read_chemstation_method <- function(path,
                                    what = c("pump", "dad", "sampler",
                                             "column"),
                                    format_out = c("data.frame", "tibble",
                                                   "data.table"),
                                    gradient_format = c("wide", "long")){
  what <- match.arg(what, c("pump", "dad", "sampler", "column"),
                    several.ok = TRUE)
  format_out <- match.arg(format_out, c("data.frame", "tibble", "data.table"))
  gradient_format <- match.arg(gradient_format, c("wide", "long"))
  if (!dir.exists(path)){
    stop("`", path, "` is not a directory.", call. = FALSE)
  }
  if (!grepl("\\.M$", path, ignore.case = TRUE)){
    m <- list.files(path, "^(ACQ|RUN)\\.M$", ignore.case = TRUE,
                    full.names = TRUE)
    if (!length(m)){
      stop("`", path, "` holds no `ACQ.M` or `RUN.M` method directory.",
           call. = FALSE)
    }
    path <- m[order(!grepl("^ACQ", basename(m), ignore.case = TRUE))][1]
  }
  if (grepl("^DA\\.M$", basename(path), ignore.case = TRUE)){
    warning("`DA.M` holds the data analysis method; its pump settings may not ",
            "be those the run was acquired with. Read `ACQ.M` instead if the ",
            "`.D` directory has one.", call. = FALSE)
  }
  files <- list(pump = "LPMP1\\.REG", dad = "LDAD1\\.REG",
                sampler = "L(ALS|WLS)1\\.REG", column = "LTHM1\\.REG")[what]
  xml <- c(pump = "Pump", dad = "Dad", sampler = "Sampler",
           column = "ColumnComp")[what]
  contents <- list.files(path)
  find <- function(pattern){
    hit <- grep(pattern, contents, ignore.case = TRUE, value = TRUE)
    if (length(hit)) file.path(path, hit[1]) else NA_character_
  }
  found <- vapply(names(files), function(module){
    find(paste0("^", files[[module]], "$"))
  }, "")
  rapid <- vapply(names(files), function(module){
    if (!is.na(found[[module]])) return(NA_character_)
    find(paste0("^Agilent.*", xml[[module]],
                "Driver.*\\.RapidControl\\.MethodXML\\.xml$"))
  }, "")
  missing <- is.na(found) & is.na(rapid)
  if (all(missing)){
    stop("`", path, "` holds none of the requested modules (",
         paste0("`", names(files), "`", collapse = ", "), ").", call. = FALSE)
  }
  if (any(missing)){
    warning("No method settings for ", paste0("`", names(files)[missing],
                                              "`", collapse = ", "),
            " in `", path, "`.", call. = FALSE)
  }
  modules <- names(files)[!missing]
  out <- lapply(modules, function(module){
    if (!is.na(rapid[[module]])){
      f <- rapid[[module]]
      return(switch(module,
                    pump = parse_pump_method(f, format_out = format_out,
                                             gradient_format = gradient_format),
                    dad = parse_dad_method(f, format_out = format_out),
                    sampler = parse_multisampler_method(f),
                    column = parse_column_method(f, format_out = format_out)))
    }
    reg <- read_chemstation_reg(found[[module]])
    switch(module,
           pump = method_pump(reg, format_out, gradient_format),
           dad = method_dad(reg, format_out),
           sampler = method_sampler(reg),
           column = method_column(reg, format_out))
  })
  names(out) <- c(pump = "pump", dad = "dad", sampler = "autosampler",
                  column = "column")[modules]
  out
}

#' Look up numeric settings in a register file's conditions
#'
#' Settings stored as single-precision floats are rounded to the 7 significant
#' digits they hold.
#' @noRd
method_num <- function(reg, keys){
  cond <- reg$conditions
  signif(suppressWarnings(as.numeric(cond$value[match(keys, cond$key)])), 7)
}

#' @noRd
method_str <- function(reg, keys){
  cond <- reg$conditions
  x <- cond$value[match(keys, cond$key)]
  replace(x, !is.na(x) & !nzchar(x), NA)
}

#' @noRd
method_pump <- function(reg, format_out, gradient_format){
  ch <- c("A", "B", "C", "D")
  on <- c(TRUE, method_num(reg, paste0("SOLV_ON_", ch[-1])) %in% 1)
  start <- method_num(reg, paste0("SOLV_RATIO_", ch))
  flow <- method_num(reg, "FLOW")
  tt <- reg$tables[["TIMETABLE"]]
  gradient <- NULL
  if (is.null(tt)){
    warning("The pump timetable could not be read.", call. = FALSE)
  } else {
    solv <- paste0("solv_", ch[-1])
    if (!nrow(tt) || all(tt$time != 0)){
      init <- tt[NA_integer_, ]
      init$time <- 0
      init[intersect(solv, names(tt))] <- as.list(
        start[match(intersect(solv, names(tt)), solv) + 1])
      if ("flow" %in% names(tt)) init$flow <- flow
      tt <- rbind(init, tt)
    }
    tt <- tt[order(tt$time), ]
    fill <- function(x, initial){
      t0 <- tt$time[!is.na(x)]
      x0 <- x[!is.na(x)]
      if (!any(t0 == 0)){
        t0 <- c(0, t0)
        x0 <- c(initial, x0)
      }
      if (length(t0) == 1) return(rep(x0, nrow(tt)))
      approx(t0, x0, xout = tt$time, rule = 2, ties = "ordered")$y
    }
    pct <- vapply(ch[-1], function(x){
      col <- paste0("solv_", x)
      if (col %in% names(tt)) fill(tt[[col]], start[match(x, ch)]) else
        rep(0, nrow(tt))
    }, numeric(nrow(tt)))
    pct <- cbind(A = 100 - rowSums(matrix(pct, nrow(tt))),
                 matrix(pct, nrow(tt), dimnames = list(NULL, ch[-1])))
    keep <- on | colSums(abs(pct) > 0) > 0
    gradient <- data.frame(time_min = tt$time,
                           matrix(signif(pct[, keep, drop = FALSE], 7),
                                  nrow(tt),
                                  dimnames = list(NULL,
                                                  paste0("pct_", ch[keep]))))
    if ("flow" %in% names(tt) && any(!is.na(tt$flow))){
      gradient$flow_mL_min <- fill(tt$flow, flow)
    }
    if (gradient_format == "long"){
      vals <- gradient[-1]
      gradient <- data.frame(
        time_min = rep(gradient$time_min, ncol(vals)),
        channel = rep(sub("^pct_", "", sub("_mL_min$", "", names(vals))),
                      each = nrow(vals)),
        percent = unlist(vals, use.names = FALSE))
    }
    rownames(gradient) <- NULL
    gradient <- convert_format_out(gradient, format_out = format_out)
  }
  solvents <- data.frame(channel = ch, percentage = start,
                         solvent = method_str(reg, paste0("SOLV_NAME_", ch)))
  list(flow_mL_min = flow,
       stop_time_min = method_num(reg, "STOPTIME"),
       post_time_min = method_num(reg, "POSTTIME"),
       pressure_low_bar = method_num(reg, "PRESSURE_MIN"),
       pressure_high_bar = method_num(reg, "PRESSURE_MAX"),
       solvents = convert_format_out(solvents[on, , drop = FALSE],
                                     format_out = format_out),
       gradient = gradient)
}

#' @noRd
method_dad <- function(reg, format_out){
  ids <- sub("^Sample(.)Wl$", "\\1",
             grep("^Sample.Wl$", reg$conditions$key, value = TRUE))
  ids <- ids[method_num(reg, sprintf("StoreSignal%s", ids)) %in% 1]
  ref <- method_num(reg, sprintf("Reference%sOn", ids)) %in% 1
  signals <- data.frame(
    id = ids,
    wavelength_nm = method_num(reg, sprintf("Sample%sWl", ids)),
    bandwidth_nm = method_num(reg, sprintf("Sample%sBw", ids)),
    reference_nm = ifelse(ref, method_num(reg, sprintf("Reference%sWl", ids)),
                          NA_real_),
    reference_bandwidth_nm = ifelse(ref, method_num(reg, sprintf("Reference%sBw",
                                                                 ids)),
                                    NA_real_))
  list(peakwidth_nm = method_num(reg, "Peakwidth"),
       slitwidth_nm = method_num(reg, "Slit"),
       uv_lamp_required = method_num(reg, "UVLamp") %in% 1,
       vis_lamp_required = method_num(reg, "VisLamp") %in% 1,
       spectra_from_nm = method_num(reg, "RangeFrom"),
       spectra_to_nm = method_num(reg, "RangeTo"),
       spectra_step_nm = method_num(reg, "RangeStep"),
       signals = convert_format_out(signals, format_out = format_out))
}

#' @noRd
method_sampler <- function(reg){
  list(thermostat_installed = NA,
       draw_speed_uL_min = method_num(reg, "DRAWSPEED"),
       eject_speed_uL_min = method_num(reg, "EJECTSPEED"),
       wait_after_draw_min = NA_real_,
       injection_volume_uL = method_num(reg, "INJVOLUME"),
       wash_time_s = NA_real_)
}

#' @noRd
method_column <- function(reg, format_out){
  left <- method_num(reg, "LeftTemp")
  right <- if (method_num(reg, "TwoTemperatures") %in% 1){
    method_num(reg, "RightTemp")
  } else left
  temps <- c(left, right)
  post <- method_num(reg, "Posttime")
  list(post_time_min = if (isTRUE(post > 0)) post else NA_real_,
       temp_controls = convert_format_out(
         data.frame(side = c("Left", "Right"),
                    temperature_C = replace(temps, temps < -273.15, NA),
                    not_ready_limit_C = method_num(reg, c("LeftReady",
                                                          "RightReady")),
                    equilibration_time_min = NA_real_),
         format_out = format_out))
}
