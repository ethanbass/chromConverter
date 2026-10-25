# Check the IT-TOF decoder against the vendor's TTFLDataExport of an MTBLS688
# run.
#
# Rscript dev/validate_ittof_mtbls688.R <run.lcd> <run_Seg1Ev1.dat> <run_Seg1Ev2.dat>
#
# The .dat files hold the export as a pickled mzXML; dev/mtbls688_dat.py loads
# them without running pickled code. The export is the stored profile after a
# 5-point moving sum scaled by 25.6 and truncated, with m/z rounded to four
# decimals and stored as float32. Every scan should match.

devtools::load_all(quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
dat_py <- reticulate::import_from_path("mtbls688_dat", path = "dev")

read_export <- function(path){
  scans <- reticulate::r_to_py(list())
  dat_py$walk(dat_py$load(path), scans)
  lapply(reticulate::py_to_r(scans), function(s){
    p <- s$children[[1]]$data
    list(num = as.integer(s$attrib$num), mz = as.numeric(p$mz),
         intensity = as.numeric(p$intensity))
  })
}
export <- c(read_export(args[2]), read_export(args[3]))
export <- export[order(vapply(export, `[[`, 1L, "num"))]

x <- read_ittof_scans(args[1])
h <- x$header
cal <- read_ittof_calibration(args[1])
runs <- decode_ittof_runs(x$words, start = x$offsets/2 + 129,
                          n_words = h[, "n_words"])
row <- rep.int(runs$scan, runs$n)
bin <- sequence(runs$n, from = runs$bin)
value <- x$words[sequence(runs$n, from = runs$word)]
stopifnot(nrow(h) == length(export))

bad_int <- bad_mz <- 0L
for (i in seq_len(nrow(h))){
  b <- bin[row == i]
  e <- export[[i]]
  if (!length(b)){
    if (any(e$intensity > 0)){
      bad_int <- bad_int + 1L
      cat("intensity mismatch in scan", i - 1L, "\n")
    }
    next
  }
  grid <- seq(min(b) - 4, max(b) + 4)
  v <- numeric(length(grid))
  v[b - grid[1] + 1] <- value[row == i]
  smooth <- floor(stats::filter(v, rep(1, 5), sides = 2) * 25.6 + 1e-9)
  smooth[is.na(smooth)] <- 0
  mz <- ittof_tof_to_mz(grid + h[i, "t0"], cal[[h[i, "polarity"] + 1L]])
  # the export keeps only the bins where the 5-point window fits in the scan
  keep <- smooth > 0 & grid >= 2 & grid <= h[i, "last_bin"] - 3
  nz <- e$intensity > 0
  if (!identical(as.numeric(smooth[keep]), e$intensity[nz])){
    bad_int <- bad_int + 1L
    cat("intensity mismatch in scan", i - 1L, "\n")
  } else if (any(abs(mz[keep] - e$mz[nz]) > 5e-5 + 1.2e-7 * e$mz[nz])){
    bad_mz <- bad_mz + 1L
    cat("m/z mismatch in scan", i - 1L, "\n")
  }
}
cat(sprintf("%d scans: %d intensity mismatches, %d m/z mismatches\n",
            nrow(h), bad_int, bad_mz))
