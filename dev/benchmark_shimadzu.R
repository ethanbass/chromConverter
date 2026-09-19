# Timing and golden-output harness for the 'Shimadzu' parsers.
#
# The binary fixtures live in `chromConverterExtraTests`, not in this package,
# so nothing here can run on CRAN or in CI. `dev/` is .Rbuildignore'd.
#
#   source("dev/benchmark_shimadzu.R")
#   sz_snapshot()                  # before changing a parser
#   sz_benchmark()                 # timings
#   sz_check_snapshot()            # after changing a parser
#
# Every optimization applied to these parsers is meant to be value-neutral, so
# `sz_check_snapshot()` should report no differences.

sz_cases <- function(){
  list(
    list(id = "qtof_ms",    file = "shimadzu_qtof.lcd",       what = "ms"),
    list(id = "qtof_tic",   file = "shimadzu_qtof.lcd",       what = "tic"),
    list(id = "tlm_ms",     file = "shimadzu_tlm_dda.lcd",    what = "ms"),
    list(id = "sim_tic",    file = "shimadzu_tlm_sim.lcd",    what = "tic"),
    list(id = "scan_tic",   file = "shimadzu_tlm_scan.lcd",   what = "tic"),
    list(id = "tlm_tic",    file = "shimadzu_tlm_dda.lcd",    what = "tic"),
    list(id = "mrm_ms",     file = "shimadzu_tlm_mrm.lcd",     what = "ms"),
    list(id = "mrm_tic",    file = "shimadzu_tlm_mrm.lcd",     what = "tic"),
    list(id = "smrm_ms",    file = "shimadzu_tlm_mrm_multi.lcd", what = "ms"),
    list(id = "smrm_tic",   file = "shimadzu_tlm_mrm_multi.lcd", what = "tic"),
    list(id = "pda_3d",     file = "Anthocyanin.lcd",         what = "pda"),
    list(id = "pda_chroms", file = "Anthocyanin.lcd",         what = "chroms"),
    list(id = "mc_chroms",  file = "multichannel_chrom.lcd",  what = "chroms"),
    list(id = "gcd",        file = "FS19_214.gcd",            what = NA),
    list(id = "qgd",        file = "B4NF.7_C23.qgd",          what = NA)
  )
}

sz_path <- function(file){
  system.file(file, package = "chromConverterExtraTests")
}

# `reticulate` + `olefile` cost ~2.5 s to initialize on first use, which would
# otherwise be charged to whichever case happens to run first.
sz_warm <- function(){
  reticulate::py_run_string("import olefile")
  invisible(NULL)
}

sz_read <- function(case){
  path <- sz_path(case$file)
  if (path == "") return(NULL)
  if (is.na(case$what)){
    if (grepl("\\.gcd$", case$file)) chromConverter::read_shimadzu_gcd(path)
    else chromConverter::read_shimadzu_qgd(path)
  } else {
    chromConverter::read_shimadzu_lcd(path, what = case$what)
  }
}

sz_benchmark <- function(cases = sz_cases()){
  sz_warm()
  out <- lapply(cases, function(case){
    path <- sz_path(case$file)
    if (path == ""){
      message("skipping ", case$id, " (fixture not installed)")
      return(NULL)
    }
    t <- system.time(x <- sz_read(case))[["elapsed"]]
    n <- tryCatch(NROW(x), error = function(e) NA_integer_)
    data.frame(case = case$id, seconds = round(t, 3), rows = n)
  })
  do.call(rbind, out)
}

sz_snapshot_dir <- function(){
  dir <- file.path("dev", "snapshots")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  dir
}

sz_snapshot <- function(cases = sz_cases()){
  sz_warm()
  dir <- sz_snapshot_dir()
  for (case in cases){
    if (sz_path(case$file) == "") next
    saveRDS(sz_read(case), file.path(dir, paste0(case$id, ".rds")))
    message("saved ", case$id)
  }
  invisible(NULL)
}

sz_check_snapshot <- function(cases = sz_cases()){
  sz_warm()
  dir <- sz_snapshot_dir()
  out <- lapply(cases, function(case){
    f <- file.path(dir, paste0(case$id, ".rds"))
    if (sz_path(case$file) == "" || !file.exists(f)) return(NULL)
    cmp <- all.equal(readRDS(f), sz_read(case))
    data.frame(case = case$id,
               identical = isTRUE(cmp),
               difference = if (isTRUE(cmp)) "" else paste(cmp, collapse = "; "))
  })
  do.call(rbind, out)
}
