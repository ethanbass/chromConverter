#' Read a 'Shimadzu' method
#'
#' Reads the instrument settings stored with a 'Shimadzu' GC run (`.gcd`), LC
#' run (`.lcd`) or GC-MS run (`.qgd`), such as the oven program, gradient,
#' flows and column.
#'
#' Method parameters are also attached automatically to every chromatogram and
#' peak table read from these files, in the `method_params` metadata field.
#' This includes all scalar values but leaves out the oven program and gradient
#' tables, which are only returned by this function. The `ms` module of a
#' GC-MS run is attached as `ms_params`.
#'
#' @section File format:
#' `.gcd` and `.lcd` files store the method and instrument configuration as
#' labelled XML, in the `GUC.1.METHOD` and `GUC.1.CONFIG` streams, and the PDA
#' settings in `PDA.1.METHOD`.
#'
#' `.qgd` files store the method as unlabelled binary records, one stream per
#' module, at fixed byte offsets. Values are little-endian float32 unless marked
#' otherwise, and bytes not listed are not read. The same streams are found in
#' files from 'GCMS-QP2010' and 'QP2020 NX' instruments:
#' * `GC-2010 Instrument Parameters/Column Oven Parameter`, 260 bytes:
#'   * 8: the number of ramps, n (int32).
#'   * 12, 16: the initial temperature and hold.
#'   * 20, 100, 180: the rates, target temperatures and holds, each an array of
#'     20 slots, of which those past n are zero.
#' * `GC-2010 Instrument Parameters/Injection Parameter-1`, 512 bytes:
#'   * 12, 16, 20: the column flow, linear velocity and split ratio.
#'   * 24, 120, 216, 312: the temperature, total flow, pressure and purge flow
#'     programs, 96 bytes each: the number of ramps (int32), the initial value
#'     and hold, then arrays of 7 rates, targets and holds.
#' * `GCMS Configuration/Column-1`, 832 bytes:
#'   * 0, 64: the column name and serial number, null-terminated strings of up
#'     to 64 bytes.
#'   * 132: the length in tenths of a metre (int32).
#'   * 140: the maximum temperature (int32).
#' * `QP5K Instrument Parameters/MS Parameter`, 600 or 664 bytes:
#'   * 44, 48: the start and end of acquisition in milliseconds (int32).
#'
#' @param path Path to a `.gcd`, `.lcd` or `.qgd` file.
#' @param what One or more modules to read. For GC runs, any of `"oven"`,
#'   `"injector"`, `"detector"`, `"autosampler"` and `"column"`; for GC-MS runs,
#'   `"oven"`, `"injector"`, `"column"` and `"ms"`; for LC runs, `"pump"`,
#'   `"column"` and, with a PDA detector, `"dad"`. Defaults to all the modules
#'   the run has.
#' @param format_out Class of the tables returned: `"data.frame"`, `"tibble"`
#'   or `"data.table"`.
#' @return A named list with one element per module.
#'
#'   For GC runs:
#'
#'   **`oven`**: a list with scalar elements `equilibration_time_min`,
#'   `initial_temperature_C` and `initial_hold_min`, plus `program`, a table of
#'   the temperature ramps with columns `rate_C_min`, `temperature_C` and
#'   `hold_min`.
#'
#'   **`injector`**: a list with scalar elements `name`, `temperature_C`,
#'   `split_ratio`, `column_flow_mL_min`, `linear_velocity_cm_s`,
#'   `pressure_kPa`, `total_flow_mL_min` and `purge_flow_mL_min`.
#'
#'   **`detector`**: a list with scalar elements `name`, `temperature_C`,
#'   `h2_flow_mL_min`, `air_flow_mL_min`, `makeup_flow_mL_min` and
#'   `sampling_rate_ms`.
#'
#'   **`autosampler`**: a list with scalar element `injection_volume_uL`.
#'
#'   **`column`**: a list with scalar elements `name`, `length_m`,
#'   `diameter_mm`, `film_thickness_um`, `max_temperature_C` and
#'   `serial_number`.
#'
#'   For GC-MS runs, `oven` and `column` as for GC runs but without
#'   `equilibration_time_min`, `diameter_mm` and `film_thickness_um`; `injector`
#'   with `temperature_C`, `split_ratio`, `column_flow_mL_min`,
#'   `linear_velocity_cm_s`, `total_flow_mL_min` and `purge_flow_mL_min`; and
#'   **`ms`**, a list with scalar elements `start_time_min` and `end_time_min`,
#'   the window in which the mass spectrometer acquires. A split ratio of -1
#'   appears to mean the split is off. The injector settings are the values at
#'   the start of the run; a method may program the pressure or purge flow to
#'   change later, so a purge flow of 0 can be the start of a purge program.
#'   Unlike the other settings, `temperature_C` could not be checked against
#'   other values: it is identified by its position, which matches the order of
#'   the labelled settings in `.gcd` files.
#'
#'   For LC runs:
#'
#'   **`pump`**: a list with scalar elements `mode`, the abbreviation
#'   'LabSolutions' stores for the pump mode (e.g. `ISO`, `BGE` or `LPGE`),
#'   `flow_mL_min` and `stop_time_min`, plus `gradient`, a table with columns
#'   `time_min` and `pct_B`. The gradient starts from the pump's initial
#'   concentration of B at time 0 and changes linearly between the times
#'   listed. It is `NULL` for an isocratic run whose time program sets no
#'   concentration, as is `flow_mL_min`.
#'
#'   **`column`**: a list with scalar element `temperature_C`, the temperature
#'   of the column oven, which is `NA` if the oven is not in use.
#'
#'   **`dad`**: the PDA detector, a list with scalar elements
#'   `start_wavelength_nm`, `end_wavelength_nm`, `sampling_interval_ms`,
#'   `end_time_min` and `cell_temperature_C` (`NA` if the cell is not
#'   thermostatted), plus
#'   `channels`, a table of the channels the acquisition method extracts, with
#'   columns `channel`, `wavelength_nm` and `bandwidth_nm`. Data processing can
#'   change these channels; the wavelengths of the peak tables are those of the
#'   processed channels.
#' @examples \dontrun{
#' method <- read_shimadzu_method("path/to/file.gcd")
#' method$oven$program
#' }
#' @seealso [read_chemstation_method] and [read_agilent_amx] for 'Agilent'
#'   methods.
#' @author Ethan Bass
#' @family 'Shimadzu' parsers
#' @export
read_shimadzu_method <- function(path, what = NULL,
                                 format_out = c("data.frame", "tibble",
                                                "data.table")){
  format_out <- match.arg(format_out, c("data.frame", "tibble", "data.table"))
  check_py_module("olefile")
  magic <- as.raw(c(0xD0, 0xCF, 0x11, 0xE0, 0xA1, 0xB1, 0x1A, 0xE1))
  if (!identical(readBin(path, "raw", 8), magic)){
    stop("`", path, "` is not a 'Shimadzu' data file.", call. = FALSE)
  }
  for (type in c("GC", "LC")){
    stream <- c("GUMM_Information", paste0("Shimadzu", type, ".1"))
    method <- sz_method_xml(path, c(stream, "GUC.1.METHOD"))
    if (!is.null(method)) break
  }
  if (is.null(method)){
    type <- "GCMS"
    if (!check_stream(path, c("GC-2010 Instrument Parameters",
                              "Column Oven Parameter"), min_size = 0)){
      stop("`", path, "` holds no 'Shimadzu' GC, LC or GC-MS method.",
           call. = FALSE)
    }
  }
  modules <- switch(type,
                    GC = c("oven", "injector", "detector", "autosampler",
                           "column"),
                    GCMS = c("oven", "injector", "column", "ms"),
                    LC = c("pump", "column",
                           if (check_stream(path, c("GUMM_Information",
                                                    "ShimadzuPDA.1",
                                                    "PDA.1.METHOD"),
                                            min_size = 0)) "dad"))
  what <- if (is.null(what)) modules else
    match.arg(what, modules, several.ok = TRUE)
  out <- switch(type,
                GC = sz_gc_method(
                  method, sz_method_xml(path, c(stream, "GUC.1.CONFIG")),
                  what, format_out),
                GCMS = sz_qgd_method(path, what, format_out),
                LC = sz_lc_method(method, what, format_out, path))
  out[what]
}

#' @noRd
sz_gc_method <- function(method, config, what, format_out){
  slot <- function(unit, id, keep, li = NULL){
    x <- sz_upd(config, unit, id, li = li)
    ui <- xml2::xml_attr(xml2::xml_find_first(x, "parent::UP"), "UI")
    ui <- ui[keep(xml2::xml_text(xml2::xml_find_first(x, "Val")))]
    if (length(ui)) ui[1] else "1"
  }
  inj <- slot("INJ", "Name", nzchar)
  det <- slot("DET", "Name", nzchar)
  col <- slot("COLUMN", "UseF", function(x) x == "1", li = "1")
  m <- function(unit, id, ui = "1") sz_param(method, unit, id, ui)
  out <- lapply(what, function(module){
    switch(module,
      oven = {
        n <- m("OVEN", "OvTempPgCnt")
        ramps <- sz_floats(m("OVEN", "OvTempPg"))
        ramps <- matrix(ramps[seq_len(3 * n)], ncol = 3, byrow = TRUE)
        list(equilibration_time_min = m("OVEN", "EqTim"),
             initial_temperature_C = m("OVEN", "OvTempPgIniTemp"),
             initial_hold_min = m("OVEN", "OvTempPgIniTim"),
             program = convert_format_out(
               data.frame(rate_C_min = ramps[, 1], temperature_C = ramps[, 2],
                          hold_min = ramps[, 3]),
               format_out = format_out))
      },
      injector = list(
        name = sz_param(config, "INJ", "Name", inj, "0"),
        temperature_C = m("INJ", "InjTempPgIniTemp", inj),
        split_ratio = m("INJ", "SplRatio", inj),
        column_flow_mL_min = m("INJ", "ColFl", inj),
        linear_velocity_cm_s = m("INJ", "LinearV", inj),
        pressure_kPa = m("INJ", "InjPrsPgIniPrs", inj),
        total_flow_mL_min = m("INJ", "InjFlPgIniFl", inj),
        purge_flow_mL_min = m("INJ", "PurFlPgIniFl", inj)),
      detector = list(
        name = sz_param(config, "DET", "Name", det, "0"),
        temperature_C = m("DET", "DetTemp", det),
        h2_flow_mL_min = m("DET", "H2FlPgIniFl", det),
        air_flow_mL_min = m("DET", "AirFlPgIniFl", det),
        makeup_flow_mL_min = m("DET", "MkupFlPgIniFl", det),
        sampling_rate_ms = m("DET", "SampRate", det)),
      autosampler = list(injection_volume_uL = m("AOC", "InjVol")),
      column = list(
        name = sz_param(config, "COLUMN", "ColN", col),
        length_m = sz_param(config, "COLUMN", "ColLen", col),
        diameter_mm = sz_param(config, "COLUMN", "ColDiam", col),
        film_thickness_um = sz_param(config, "COLUMN", "film_thick", col),
        max_temperature_C = sz_param(config, "COLUMN", "MaxUTmp", col),
        serial_number = sz_param(config, "COLUMN", "ColID", col)))
  })
  names(out) <- what
  out
}

#' Read the PDA detector method of a 'Shimadzu' LC file
#'
#' From the `PDA.1.METHOD` stream, where wavelengths are stored x 100 and times
#' in milliseconds. The extracted channels are numbered `AN$Ch#k`, with
#' wavelength `AN$Wave#k` and bandwidth `AN$BndWid#k`.
#' @noRd
sz_pda_method <- function(path, format_out){
  doc <- sz_method_xml(path, c("GUMM_Information", "ShimadzuPDA.1",
                               "PDA.1.METHOD"))
  p <- function(id) sz_param(doc, "PDA", id)
  ch <- xml2::xml_attr(xml2::xml_find_all(
    doc, "/GUD/UP[@Name='PDA']/UPD[starts-with(@ID, 'AN$Ch#')]"), "ID")
  k <- sub("^AN\\$Ch#", "", ch)
  list(start_wavelength_nm = p("StWav") / 100,
       end_wavelength_nm = p("EdWav") / 100,
       sampling_interval_ms = p("SmplRt"),
       end_time_min = p("EdTm") / 60000,
       cell_temperature_C = if (p("UseCTmp") %in% 1) p("CTmp") else NA,
       channels = convert_format_out(
         data.frame(channel = vapply(paste0("AN$Ch#", k), p, numeric(1),
                                     USE.NAMES = FALSE),
                    wavelength_nm = vapply(paste0("AN$Wave#", k), p,
                                           numeric(1), USE.NAMES = FALSE) / 100,
                    bandwidth_nm = vapply(paste0("AN$BndWid#", k), p,
                                          numeric(1), USE.NAMES = FALSE)),
         format_out = format_out))
}

#' Read a 'Shimadzu' file's method as metadata
#'
#' The scalar settings `read_shimadzu_method` returns, as `method_params`, with
#' the acquisition window of a GC-MS run as `ms_params`. Tables are left out,
#' and a file whose method cannot be read gets neither.
#' @noRd
sz_method_metadata <- function(path){
  m <- tryCatch(suppressWarnings(read_shimadzu_method(path)),
                error = function(e) NULL)
  if (is.null(m)) return(list())
  ms <- m$ms
  m$ms <- NULL
  list(method_params = lapply(m, function(module){
         module[!vapply(module, function(x) is.data.frame(x) || is.null(x),
                        logical(1))]
       }),
       ms_params = ms)
}

#' Read the GC method of a 'Shimadzu' GC-MS file
#'
#' Fields are little-endian float32 or int32 at fixed offsets in the
#' `GC-2010 Instrument Parameters`, `GCMS Configuration` and `QP5K Instrument
#' Parameters` streams; the oven ramps are three arrays of 20 float32 (rates,
#' targets and holds). Injector and column slot 1 are the ones in use in every
#' file seen.
#' @noRd
sz_qgd_method <- function(path, what, format_out){
  olefile <- py_import("olefile")
  ole <- olefile$OleFileIO(path)
  on.exit(ole$close(), add = TRUE)
  read <- function(stream){
    if (!ole$exists(stream)) return(NULL)
    as.raw(reticulate::import_builtins()$list(ole$openstream(stream)$read()))
  }
  f32 <- function(b, o, n = 1){
    if (is.null(b)) return(rep(NA_real_, n))
    signif(readBin(b[o + seq_len(4 * n)], "double", n = n, size = 4,
                   endian = "little"), 7)
  }
  i32 <- function(b, o){
    if (is.null(b)) return(NA_real_)
    as.numeric(readBin(b[o + 1:4], "integer", size = 4, endian = "little"))
  }
  str <- function(b, o){
    if (is.null(b)) return(NA_character_)
    x <- b[o + 1:64]
    rawToChar(x[cumsum(x == as.raw(0)) == 0])
  }
  gc <- "GC-2010 Instrument Parameters/"
  oven <- read(paste0(gc, "Column Oven Parameter"))
  inj <- read(paste0(gc, "Injection Parameter-1"))
  col <- read("GCMS Configuration/Column-1")
  ms <- read("QP5K Instrument Parameters/MS Parameter")
  out <- lapply(what, function(module){
    switch(module,
      oven = {
        n <- i32(oven, 8)
        list(initial_temperature_C = f32(oven, 12),
             initial_hold_min = f32(oven, 16),
             program = convert_format_out(
               data.frame(rate_C_min = f32(oven, 20, n),
                          temperature_C = f32(oven, 100, n),
                          hold_min = f32(oven, 180, n)),
               format_out = format_out))
      },
      injector = list(temperature_C = f32(inj, 28),
                      split_ratio = f32(inj, 20),
                      column_flow_mL_min = f32(inj, 12),
                      linear_velocity_cm_s = f32(inj, 16),
                      total_flow_mL_min = f32(inj, 124),
                      purge_flow_mL_min = f32(inj, 316)),
      column = list(name = str(col, 0),
                    length_m = i32(col, 132) / 10,
                    max_temperature_C = i32(col, 140),
                    serial_number = str(col, 64)),
      ms = list(start_time_min = i32(ms, 44) / 60000,
                end_time_min = i32(ms, 48) / 60000))
  })
  names(out) <- what
  out
}

#' @noRd
sz_lc_method <- function(method, what, format_out, path){
  out <- lapply(what, function(module){
    switch(module,
      pump = {
        x <- sz_upd(method, "PUMP", "PUMPpumpMode")
        modes <- data.frame(
          ui = xml2::xml_attr(xml2::xml_find_first(x, "parent::UP"), "UI"),
          val = xml2::xml_text(xml2::xml_find_first(x, "Val")))
        modes <- modes[order(modes$val == "0", modes$ui), ]
        ui <- modes$ui[1]
        mode <- c("ISO", "BGE", "TGE", "LPGE", "BGEx2")[as.numeric(modes$val[1]) + 1]
        p <- function(id) sz_param(method, "PUMP", paste0(mode, "$", id), ui)
        prog <- sz_timeprog(method, "SCL$TIMEPROG")
        unknown <- setdiff(prog$code, c(5, 94, 96, 97))
        if (length(unknown)){
          warning("Time program commands with codes ",
                  paste(unknown, collapse = ", "), " were not read.",
                  call. = FALSE)
        }
        conc <- prog[prog$code == 5, ]
        flow <- p("tflow")
        gradient <- if (nrow(conc) || mode != "ISO"){
          if (!any(conc$time <= 0.01)){
            conc <- rbind(data.frame(time = 0, code = 5, data = p("bconc")),
                          conc)
          }
          convert_format_out(data.frame(time_min = conc$time,
                                        pct_B = conc$data),
                             format_out = format_out)
        }
        list(mode = mode,
             flow_mL_min = if (!is.na(flow) && flow > 0) flow else NA,
             stop_time_min = if (any(prog$code == 97)){
               max(prog$time[prog$code == 97])
             } else NA,
             gradient = gradient)
      },
      column = list(temperature_C = if (sz_param(method, "OVEN", "OVEN$use") %in% 1){
        sz_param(method, "OVEN", "OVEN$ovent")
      } else NA),
      dad = sz_pda_method(path, format_out))
  })
  names(out) <- what
  out
}

#' Read a 'Shimadzu' method or configuration stream as XML
#'
#' Returns `NULL` if the stream is absent.
#' @noRd
sz_method_xml <- function(path, stream){
  xml_path <- export_stream(path, stream = stream, remove_null_bytes = TRUE)
  if (is.na(xml_path)) return(NULL)
  on.exit(unlink_stream(xml_path), add = TRUE)
  xml2::read_xml(xml_path)
}

#' Find parameters in a 'Shimadzu' method or configuration
#'
#' Selects the parameters (`UPD`) with ID `id` in the units (`UP`) named `unit`,
#' optionally only those in slot `ui` and on analytical line `li`.
#' @noRd
sz_upd <- function(doc, unit, id, ui = NULL, li = NULL){
  up <- sprintf("@Name='%s'", unit)
  if (!is.null(ui)) up <- sprintf("%s and @UI='%s'", up, ui)
  if (!is.null(li)) up <- sprintf("%s and @LI='%s'", up, li)
  xml2::xml_find_all(doc, sprintf("/GUD/UP[%s]/UPD[@ID='%s']", up, id))
}

#' @noRd
sz_param <- function(doc, unit, id, ui = "1", li = "1"){
  x <- sz_upd(doc, unit, id, ui, li)
  if (!length(x)) return(NA)
  sz_value(xml2::xml_text(xml2::xml_find_first(x[[1]], "Val")),
           xml2::xml_text(xml2::xml_find_first(x[[1]], "Type")))
}

#' Read a 'Shimadzu' LC time program
#'
#' Returns one row per line of the program, with its `time`, command `code`
#' and `data`.
#' @noRd
sz_timeprog <- function(doc, name){
  rows <- xml2::xml_find_all(doc, sprintf("/GUD/TP[@PgN='%s']/TPLD", name))
  val <- function(id){
    xml2::xml_text(xml2::xml_find_first(rows, sprintf("TPD[@ID='%s']/Val", id)))
  }
  data.frame(time = vapply(val("TMPGtime"), sz_floats, 0, USE.NAMES = FALSE),
             code = as.numeric(val("TMPGcode")),
             data = vapply(val("TMPGdata"), sz_floats, 0, USE.NAMES = FALSE))
}

#' Decode a 'Shimadzu' method parameter
#'
#' Type 4 is a little-endian float32 written as hex, and types 2, 3 and 18
#' integers. Other types, such as 8, are strings or hex arrays, and are returned
#' as stored.
#' @noRd
sz_value <- function(val, type){
  switch(type,
         "4" = sz_floats(val),
         "2" = , "3" = , "18" = as.numeric(val),
         val)
}

#' @noRd
sz_floats <- function(hex){
  if (is.na(hex) || nchar(hex) < 8) return(NA_real_)
  bytes <- as.raw(strtoi(substring(hex, seq(1, nchar(hex) - 1, 2),
                                   seq(2, nchar(hex), 2)), 16L))
  signif(readBin(bytes, "double", n = length(bytes) %/% 4, size = 4,
                 endian = "little"), 7)
}
