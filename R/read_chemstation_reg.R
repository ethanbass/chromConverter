#' Read 'Agilent ChemStation' register files
#'
#' Reads the instrument traces, the keys and values describing each module, and
#' the tables, such as a pump's timetable, from 'Agilent ChemStation' register
#' (`.REG`) files. Users reach it through [read_agilent_d] with
#' `what = "instrument"` and through [read_chemstation_method]; this page
#' documents the file format.
#'
#' Objects that cannot be parsed are skipped with a warning. Tested on
#' `LCDIAG.REG` files from revisions A.10.02, B.01.03, B.04.02, B.04.03,
#' C.01.03, C.01.07 and C.01.10, and on method register files from four
#' revision B and C instruments and two revision A instruments.
#'
#' @section File format:
#' All integers are little-endian and offsets are counted from 0.
#'
#' **Header.** Bytes `02 33 32 00`, then the length-prefixed string
#' `REGISTER FILE` at 0x04 and a length-prefixed container version at 0x18:
#' `notused` in files from revisions B and C, `A.xx.xx` in files from
#' revision A. At 0x22 is a u32 offset of the object index and at 0x26 a u16
#' object count. The index holds one 8-byte entry per object, starting with the
#' u32 offset of the object; the index itself begins where the last object
#' ends.
#'
#' **Revision B and C objects.** A 12-byte preamble, whose contents differ by
#' revision, then a stream written by the 'MFC' `CArchive` class. Each object
#' starts with a u16 tag: `FFFF` introduces a new class (u16 schema, u16 name
#' length, ASCII name); `8000` plus an index refers to a class already read;
#' `7FFF` is followed by a u32 tag for large indices. Class indices restart in
#' every object. The classes are:
#' * `CHPUserObject` and `CHPLCObject`: two objects (the second a list of
#'   keys and values), then a flag byte and, if it is set, a data object.
#' * `CHPList`: one object. `CObArray`: a u16 count (`FFFF` then a u32 count
#'   when large) followed by that many objects.
#' * `CHPNdrDouble`, `CHPNdrString` and `CHPNdrObject`: a key, 6 bytes, then a
#'   float64, a string or a flag byte and object. Strings are a u16 length
#'   followed by UTF-16LE; a length of `C000` is instead followed by a u16 id
#'   naming a standard key (0 `ObjClass`, 1 `Title`), and `8000` by a u16
#'   index, counted from 0, into the non-empty keys and table column names
#'   already read in the object. String values are not counted.
#' * `CHPDatLongSliced` and `CHPDatDoubleSliced`: 6 bytes, then the y row and
#'   the x row.
#' * `CHPDatLongRow` and `CHPDatDoubleRow`: 14 bytes, the unit string, u16 `7`,
#'   u16, u32 point count n and a u8 flag marking an implicit row. Unless
#'   implicit, n int32 (`Long`) or float64 (`Double`) values follow. Then u32,
#'   float64 first value, u8, u32 flag marking scaled values, float64 scale and
#'   float64 offset. Values are raw x scale + offset if scaled and raw
#'   otherwise; an implicit row is first + i x scale.
#' * `CHPTable`: a header holding a u16 row size at byte 2, a u16 row count n
#'   at byte 4, and the u16 counts of data and extra columns at bytes 20 and 22
#'   (16 and 18 for schema `11`). Then an 18-byte descriptor per column (u16
#'   offset in the row, u16 size, u16 type and u16 flags, then 10 bytes);
#'   (n + 1) x row size - 4 bytes holding a default row, less its first 4
#'   bytes, then the n rows, followed by the value of each extra column with
#'   flag `40`; and each column's name, a string whose length
#'   counts a terminator. A data column of type 3 with flag `40` is followed by
#'   its default and n strings. Type 4 is float32, with -10000 for an empty
#'   cell, and type 5 int32. The extra columns are not returned.
#' * `CHPAnnText` is read past but not returned.
#'
#' In `LCDIAG.REG` each trace is an object whose `Title` names it. Revision
#' B.01 stores traces as `CHPLCObject` with `CHPDatLong` rows and an implicit
#' time axis; B.04 does the same but writes a space before the comma in trace
#' names (`"PMP1 , Pressure"`), which is removed; revision C stores them as
#' `CHPUserObject` with unscaled `CHPDatDouble` rows and explicit, possibly
#' uneven, times. The other objects, titled `Start/Stop Conditions`, hold only
#' keys and values.
#'
#' **Revision A objects.** One byte, a u32 record count n, n 16-byte record
#' headers (u16, u16 type, u32 size, u32, u32 id), 4n further bytes, then the
#' data of each record in turn. Records refer to one another by id. Types
#' `8001` and `8003` are Latin-1 strings and `8006` a string after 2 bytes;
#' `8002` holds a numeric array; `0602` (43 bytes) a key at byte 14 and float64
#' at byte 35; `0601` a key and the u32 id of its value at byte 35; `0603` the
#' key of a table and the u32 id of its `0701` record at byte 35. A `0701`
#' record holds a u16 row size at byte 2, a u16 row count n at byte 4, the u32
#' offset of its rows at byte 6 and a u16 count of data columns at byte 16,
#' then a 30-byte descriptor per column: a 16-byte name, then the u16 offset
#' in the row, size, type and flags, as in revision B and C. The rows are
#' (n + 1) x row size bytes, the first a default row. Types
#' `0501` and `0503` (161 bytes) describe a trace: u32 point count at byte 9,
#' then the u32 ids of the x unit and data and the x scale at bytes 27, 31 and
#' 61, and those of y at bytes 94, 98 and 128. An array id with no record
#' denotes an implicit axis of i x scale.
#'
#' Not known: the meaning of the skipped bytes, and whether the scale or the
#' offset comes first in a row (every file seen has an offset of 0).
#' @param path Path to a `.REG` file.
#' @return A list of:
#'
#' * `traces`: one row per point of each instrument trace, with columns
#'   `trace` (its title, e.g. `"PMP1, Pressure"`), `unit`, `time` (in minutes)
#'   and `value`.
#' * `conditions`: the keys and values stored in every object of the file, with
#'   columns `object` (its title), `key` and `value` (as character). Each
#'   module's `Start/Stop Conditions` object (e.g.
#'   `"PMP1, Start/Stop Conditions"`) records its state at the start and end of
#'   the run, such as `StartPressure` and `StopPressure` for the pump or
#'   `ActInjVolume` for the autosampler. Every object records the start of the
#'   run as `DateTime`, and the object of a solvent trace gives the solvent's
#'   name, where one was entered, as `Description`.
#' * `tables`: a list of data frames, one per table, named by the key holding
#'   it, such as `TIMETABLE` for a pump's timetable.
#' @note The reader for revision A files was adapted from `read_reg_file` in
#' [Aston](https://github.com/bovee/aston) (Copyright 2011-2020 Roderick Bovee, BSD
#' 3-clause license).
#' @author Ethan Bass
#' @keywords internal
read_chemstation_reg <- function(path){
  b <- readBin(path, "raw", file.size(path))
  if (length(b) < 40 || !identical(b[1:4], as.raw(c(0x02, 0x33, 0x32, 0x00))) ||
      rawToChar(b[6:18]) != "REGISTER FILE"){
    stop("`", path, "` is not a 'ChemStation' register file.", call. = FALSE)
  }
  idx <- reg_u32_at(b, 0x22)
  n <- readBin(b[0x26 + 1:2], "integer", size = 2, signed = FALSE,
               endian = "little")
  offs <- c(vapply(seq_len(n) - 1, function(i) reg_u32_at(b, idx + 8 * i), 0),
            idx)
  version <- rawToChar(b[25 + seq_len(as.integer(b[25]))])
  res <- if (startsWith(version, "A")) reg_read_a(b, offs) else
    reg_read_mfc(b, offs)
  if (length(res$failed)){
    warning("Could not parse ", length(res$failed), " object(s) in `",
            basename(path), "`: ", paste(res$failed, collapse = "; "),
            call. = FALSE)
  }
  titles <- make.unique(vapply(res$traces, function(t) t$trace[1], ""))
  res$traces <- Map(function(t, title){
    t$trace <- title
    t
  }, res$traces, titles)
  tables <- lapply(res$tables, `[[`, "data")
  names(tables) <- make.unique(vapply(res$tables, `[[`, "", "key"))
  list(traces = reg_bind(res$traces, data.frame(
         trace = character(), unit = character(), time = numeric(),
         value = numeric())),
       conditions = reg_bind(res$conditions, data.frame(
         object = character(), key = character(), value = character())),
       tables = tables)
}

#' @noRd
reg_bind <- function(x, empty){
  if (length(x)) do.call(rbind, c(x, make.row.names = FALSE)) else empty
}

#' @noRd
reg_u32_at <- function(b, o){
  as_uint32(readBin(b[o + 1:4], "integer", size = 4, endian = "little"))
}

#' @noRd
reg_title <- function(x){
  gsub(" +,", ",", x)
}

#' @noRd
reg_trace <- function(title, x, y){
  if (length(x$values) != length(y$values)){
    stop("trace has ", length(y$values), " values but ", length(x$values),
         " times")
  }
  data.frame(trace = title, unit = y$unit, time = x$values, value = y$values)
}

#' @noRd
reg_take <- function(r, n){
  if (r$o + n > length(r$b)) stop("read past end at byte ", r$o)
  x <- r$b[r$o + seq_len(n)]
  r$o <- r$o + n
  x
}

#' @noRd
reg_u8 <- function(r) as.integer(reg_take(r, 1))

#' @noRd
reg_u16 <- function(r){
  readBin(reg_take(r, 2), "integer", size = 2, signed = FALSE,
          endian = "little")
}

#' @noRd
reg_u32 <- function(r){
  reg_u32_at(reg_take(r, 4), 0)
}

#' @noRd
reg_f64 <- function(r, n = 1){
  readBin(reg_take(r, 8 * n), "double", n = n, size = 8, endian = "little")
}

#' @noRd
reg_wstr <- function(r, term = 0, pool = TRUE){
  n <- reg_u16(r)
  if (n == 0xC000) return(paste0("#", reg_u16(r)))
  if (n == 0x8000){
    i <- reg_u16(r)
    return(if (i < length(r$pool)) r$pool[i + 1] else paste0("#@", i))
  }
  n <- max(n - term, 0)
  if (n == 0) return("")
  s <- iconv(list(reg_take(r, 2 * n)), from = "UTF-16LE", to = "UTF-8")
  if (pool) r$pool <- c(r$pool, s)
  s
}

#' Read one object from an 'MFC' archive stream
#'
#' Class tags index `a$load`, which holds the classes and objects read so far
#' in the current object, as in `CArchive::ReadObject`.
#' @noRd
reg_obj <- function(a){
  r <- a$r
  tag <- reg_u16(r)
  cls_bit <- 0x8000
  if (tag == 0x7FFF){
    tag <- reg_u32(r)
    cls_bit <- 2^31
  }
  if (tag == 0xFFFF){
    schema <- reg_u16(r)
    cls <- list(name = rawToChar(reg_take(r, reg_u16(r))), schema = schema)
    a$load[[length(a$load) + 1]] <- cls
  } else if (tag >= cls_bit){
    cls <- a$load[[tag - cls_bit + 1]]
  } else if (tag == 0){
    return(NULL)
  } else {
    stop("object back-reference ", tag, " at byte ", r$o)
  }
  a$load[length(a$load) + 1] <- list(NULL)
  switch(cls$name,
         CHPUserObject = , CHPLCObject = {
           reg_obj(a)
           reg_obj(a)
           list(data = if (reg_u8(r)) reg_obj(a))
         },
         CHPList = reg_obj(a),
         CObArray = {
           n <- reg_u16(r)
           if (n == 0xFFFF) n <- reg_u32(r)
           lapply(seq_len(n), function(i) reg_obj(a))
         },
         CHPNdrDouble = reg_ndr(a, function() reg_f64(r)),
         CHPNdrString = reg_ndr(a, function() reg_wstr(r, pool = FALSE)),
         CHPNdrObject = reg_ndr(a, function() if (reg_u8(r)) reg_obj(a)),
         CHPAnnText = {
           reg_take(r, 34)
           text <- reg_wstr(r)
           reg_take(r, 2 * reg_u8(r) + 3)
           text
         },
         CHPDatDoubleSliced = , CHPDatLongSliced = {
           reg_take(r, 6)
           list(y = reg_obj(a), x = reg_obj(a))
         },
         CHPDatDoubleRow = reg_row(r, "double"),
         CHPDatLongRow = reg_row(r, "integer"),
         CHPTable = reg_table(a, cls$schema),
         stop("unknown class ", cls$name, " at byte ", r$o))
}

#' @noRd
reg_ndr <- function(a, value){
  key <- reg_wstr(a$r)
  reg_take(a$r, 6)
  key <- switch(key, "#0" = "ObjClass", "#1" = "Title", key)
  a$key <- key
  val <- value()
  a$kv[[length(a$kv) + 1]] <- list(key = key, value = val)
  invisible(NULL)
}

#' @noRd
reg_row <- function(r, type){
  reg_take(r, 14)
  unit <- reg_wstr(r)
  seven <- reg_u16(r)
  reg_u16(r)
  n <- reg_u32(r)
  implicit <- reg_u8(r)
  if (seven != 7) stop("unexpected row header at byte ", r$o)
  size <- if (type == "double") 8 else 4
  raw <- if (!implicit){
    readBin(reg_take(r, size * n), type, n = n, size = size, endian = "little")
  }
  reg_u32(r)
  first <- reg_f64(r)
  reg_u8(r)
  scaled <- reg_u32(r)
  scale <- reg_f64(r)
  offset <- reg_f64(r)
  values <- if (implicit){
    first + (seq_len(n) - 1) * scale
  } else if (scaled){
    raw * scale + offset
  } else as.numeric(raw)
  list(unit = unit, values = values)
}

#' Read a `CHPTable`
#'
#' The layout was inferred from the tables in `LCDIAG.REG` and method register
#' files; tables in `ACQRES.REG` do not follow it. Records the table's rows in
#' `a$tables` under the key of the object holding it.
#' @noRd
reg_table <- function(a, schema){
  r <- a$r
  h <- if (schema == 0x11){
    c(1, 1, 2, 2, 4, 4, 2, 2, 2, 4)
  } else c(1, 1, 2, 2, 2, 4, 2, 4, 2, 2, 2, 4)
  field <- function(size) switch(as.character(size), "1" = reg_u8(r),
                                 "2" = reg_u16(r), "4" = reg_u32(r))
  h <- vapply(h, field, 0)
  rowsize <- h[3]
  nrows <- h[4]
  nm <- h[length(h) - 2]
  ne <- h[length(h) - 1]
  cols <- t(vapply(seq_len(nm + ne), function(i){
    vapply(c(2, 2, 2, 2, 2, 4, 4), field, 0)
  }, numeric(7)))
  ext <- seq_len(ne) + nm
  dat <- reg_take(r, (nrows + 1) * rowsize - 4 +
                    sum(cols[ext[bitwAnd(cols[ext, 4], 0x40) > 0], 2]))
  names <- character(nm + ne)
  strings <- list()
  for (i in seq_len(nm + ne)){
    names[i] <- reg_wstr(r, 1)
    if (cols[i, 3] == 3 && bitwAnd(cols[i, 4], 0x40) > 0){
      s <- vapply(seq_len(if (i <= nm) nrows + 1 else 1),
                  function(j) reg_wstr(r, 1), "")
      if (i <= nm) strings[[i]] <- s[-1]
    }
  }
  starts <- rowsize - 4 + (seq_len(nrows) - 1) * rowsize
  data <- reg_cells(dat, starts, cols[seq_len(nm), 1], cols[seq_len(nm), 3],
                    names[seq_len(nm)])
  for (i in seq_along(strings)){
    if (!is.null(strings[[i]])) data[[i]] <- strings[[i]]
  }
  a$tables[[length(a$tables) + 1]] <- list(key = a$key, data = data)
  invisible(NULL)
}

#' Decode the cells of a table's rows
#'
#' Rows start at the 0-based byte offsets `starts` of `dat`. Columns of a type
#' other than float32 (4) or int32 (5) are `NA`.
#' @noRd
reg_cells <- function(dat, starts, offsets, types, names){
  n <- length(starts)
  cells <- lapply(seq_along(offsets), function(i){
    raw <- dat[c(outer(1:4, starts + offsets[i], `+`))]
    switch(as.character(types[i]),
           "4" = {
             x <- readBin(raw, "double", n = n, size = 4, endian = "little")
             signif(replace(x, x == -10000, NA), 7)
           },
           "5" = readBin(raw, "integer", n = n, size = 4, endian = "little"),
           rep(NA, n))
  })
  data <- as.data.frame(cells, col.names = seq_along(offsets))
  names(data) <- names
  data
}

#' Read a table record from a revision A register file
#'
#' @noRd
reg_table_a <- function(dat){
  u16 <- function(o) readBin(dat[o + 1:2], "integer", size = 2,
                             signed = FALSE, endian = "little")
  rowsize <- u16(2)
  nrows <- u16(4)
  start <- reg_u32_at(dat, 6)
  desc <- 20 + 30 * (seq_len(u16(16)) - 1)
  names <- vapply(desc, function(o){
    x <- dat[o + 1:16]
    rawToChar(x[cumsum(x == as.raw(0)) == 0])
  }, "")
  reg_cells(dat, start + rowsize * seq_len(nrows),
            vapply(desc, function(o) u16(o + 16), 0),
            vapply(desc, function(o) u16(o + 20), 0), names)
}

#' @noRd
reg_read_mfc <- function(b, offs){
  traces <- conditions <- tables <- failed <- list()
  for (i in seq_len(length(offs) - 1)){
    a <- new.env(parent = emptyenv())
    a$r <- new.env(parent = emptyenv())
    a$r$b <- b
    a$r$o <- offs[i] + 12
    a$load <- list(NULL)
    a$kv <- list()
    a$tables <- list()
    ok <- TRUE
    o <- tryCatch({
      o <- reg_obj(a)
      if (a$r$o != offs[i + 1]){
        stop("object ends at byte ", a$r$o, ", expected ", offs[i + 1])
      }
      o
    }, error = function(e){
      failed[[length(failed) + 1]] <<- paste0("object ", i, ": ",
                                              conditionMessage(e))
      ok <<- FALSE
    })
    if (!ok) next
    tables <- c(tables, a$tables)
    parsed <- tryCatch({
      keys <- vapply(a$kv, `[[`, "", "key")
      title <- reg_title(if ("Title" %in% keys){
        a$kv[[max(which(keys == "Title"))]]$value
      } else paste("object", i))
      keep <- vapply(a$kv, function(kv){
        !kv$key %in% c("ObjClass", "Title") &&
          (is.character(kv$value) || is.numeric(kv$value))
      }, TRUE)
      d <- if (is.list(o)) o$data
      list(
        conditions = if (any(keep)){
          data.frame(object = title, key = keys[keep],
                     value = vapply(a$kv[keep],
                                    function(kv) as.character(kv$value), ""))
        },
        trace = if (is.list(d) && !is.null(d$y) &&
                    (length(d$y$values) > 1 || nzchar(d$y$unit) ||
                     nzchar(d$x$unit))){
          reg_trace(title, d$x, d$y)
        })
    }, error = function(e){
      failed[[length(failed) + 1]] <<- paste0("object ", i, ": ",
                                              conditionMessage(e))
      NULL
    })
    conditions[[length(conditions) + 1]] <- parsed$conditions
    traces[[length(traces) + 1]] <- parsed$trace
  }
  list(traces = traces, conditions = conditions, tables = tables,
       failed = unlist(failed))
}

#' Read a revision A register file
#'
#' Each object is a count, a table of 16-byte record headers followed by one
#' 4-byte word per record, and the record data. Records refer to each other by
#' id.
#' @noRd
reg_read_a <- function(b, offs){
  cstr <- function(x) rawToChar(x[cumsum(x == as.raw(0)) == 0])
  f64 <- function(x, o) readBin(x[o + 1:8], "double", size = 8,
                                endian = "little")
  traces <- conditions <- tables <- failed <- list()
  for (i in seq_len(length(offs) - 1)){
    res <- tryCatch({
      o <- offs[i] + 1
      n <- reg_u32_at(b, o)
      d <- o + 4 + 20 * n
      byid <- list()
      kv <- list()
      tabkeys <- list()
      tabs <- list()
      xy <- NULL
      for (j in seq_len(n) - 1){
        h <- o + 4 + 16 * j
        typ <- readBin(b[h + 3:4], "integer", size = 2, signed = FALSE,
                       endian = "little")
        size <- reg_u32_at(b, h + 4)
        id <- as.character(reg_u32_at(b, h + 12))
        dat <- b[d + seq_len(size)]
        d <- d + size
        if (typ %in% c(0x8001, 0x8003)){
          byid[[id]] <- iconv(cstr(dat), "latin1", "UTF-8")
        } else if (typ == 0x8006){
          byid[[id]] <- iconv(cstr(dat[-(1:2)]), "latin1", "UTF-8")
        } else if (typ == 0x8002){
          byid[[id]] <- dat
        } else if (typ == 0x0602 && size == 43){
          kv[[length(kv) + 1]] <- list(key = cstr(dat[15:35]),
                                       value = f64(dat, 35))
        } else if (typ == 0x0601){
          kv[[length(kv) + 1]] <- list(key = cstr(dat[15:35]),
                                       ref = as.character(reg_u32_at(dat, 35)))
        } else if (typ == 0x0603){
          tabkeys[[as.character(reg_u32_at(dat, 35))]] <- cstr(dat[15:35])
        } else if (typ == 0x0701){
          tabs[[id]] <- dat
        } else if (typ %in% c(0x0501, 0x0503)){
          if (size != 161) stop("unexpected trace record of ", size, " bytes")
          xy <- dat
        }
      }
      if (d != offs[i + 1]){
        stop("object ends at byte ", d, ", expected ", offs[i + 1])
      }
      for (ref in intersect(names(tabkeys), names(tabs))){
        tables[[length(tables) + 1]] <- list(key = tabkeys[[ref]],
                                             data = reg_table_a(tabs[[ref]]))
      }
      list(byid = byid, kv = kv, xy = xy)
    }, error = function(e){
      failed[[length(failed) + 1]] <<- paste0("object ", i, ": ",
                                              conditionMessage(e))
      NULL
    })
    if (is.null(res)) next
    parsed <- tryCatch({
      lookup <- function(id){
        v <- res$byid[[id]]
        if (is.character(v)) v else ""
      }
      keys <- vapply(res$kv, `[[`, "", "key")
      vals <- vapply(res$kv, function(kv){
        if (is.null(kv$ref)) as.character(kv$value) else lookup(kv$ref)
      }, "")
      title <- reg_title(if ("Title" %in% keys){
        vals[max(which(keys == "Title"))]
      } else paste("object", i))
      keep <- !keys %in% c("ObjClass", "Title")
      trace <- if (!is.null(res$xy)){
        xy <- res$xy
        npts <- reg_u32_at(xy, 9)
        axis <- function(unit_at, data_at, scale_at){
          raw <- res$byid[[as.character(reg_u32_at(xy, data_at))]]
          scale <- f64(xy, scale_at)
          values <- if (!is.raw(raw)){
            (seq_len(npts) - 1) * scale
          } else if (length(raw) == 4 * npts){
            readBin(raw, "integer", n = npts, size = 4, endian = "little") *
              scale
          } else readBin(raw, "double", n = npts, size = 8,
                         endian = "little") * scale
          list(unit = lookup(as.character(reg_u32_at(xy, unit_at))),
               values = values)
        }
        y <- axis(94, 98, 128)
        x <- axis(27, 31, 61)
        if (npts > 1 || nzchar(y$unit)) reg_trace(title, x, y)
      }
      list(conditions = if (any(keep)){
        data.frame(object = title, key = keys[keep], value = vals[keep])
      }, trace = trace)
    }, error = function(e){
      failed[[length(failed) + 1]] <<- paste0("object ", i, ": ",
                                              conditionMessage(e))
      NULL
    })
    conditions[[length(conditions) + 1]] <- parsed$conditions
    traces[[length(traces) + 1]] <- parsed$trace
  }
  list(traces = traces, conditions = conditions, tables = tables,
       failed = unlist(failed))
}
