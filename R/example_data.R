#' Benzoxazinoid standards
#'
#' A calibration series of a mixed benzoxazinoid standard, run by HPLC with an
#' 'Agilent' diode array detector (G1315A), as ten 'ChemStation' `.D`
#' directories in `extdata`. Find them with
#' `system.file("extdata", "benzoxazinoid_standards", package = "chromConverter")`.
#'
#' * `BENZOS_1000PPM.D` to `BENZOS_4PPM.D`: nine twofold dilutions from 1000
#'   ppm, injected between 20 and 22 June 2023. The names round the last three,
#'   which are 15.625, 7.8125 and 3.90625 ppm.
#' * `MEOH.D`: a methanol blank, run with the same method on 15 June 2023.
#'
#' Each directory holds the 254 nm trace (`dad1A.ch`) and the 'ChemStation'
#' report (`Report.TXT`). `BENZOS_250PPM.D` also holds the traces at 230, 320,
#' 360 and 210 nm (`dad1B.ch` to `dad1E.ch`), and the pump's pressure, flow
#' and solvent composition through the run (`LCDIAG.REG`), which
#' [read_agilent_d] returns with `what = "instrument"`.
#'
#' The standard gives four peaks at 254 nm, eluting in the 250 ppm run at 12.5
#' (DIBOA), 19.1 (DIMBOA), 21.0 (BOA) and 28.2 minutes (MBOA).
#'
#' @section License:
#' These files are released under CC0 1.0
#' (<https://creativecommons.org/publicdomain/zero/1.0/>).
#'
#' @seealso `vignette("chromConverter")`, which works through these files.
#' @name benzoxazinoid_standards
#' @keywords datasets
NULL

#' Shimadzu GC-FID alkane ladder
#'
#' An ASCII export from 'Shimadzu LabSolutions' of a GC-FID run (GC-2014) of an
#' n-alkane ladder with nonadecane (C19) added, sampled by solid-phase
#' microextraction, holding its peak table of 83 peaks and the chromatogram,
#' recorded every 40 ms for 44 minutes. Find it with
#' `system.file("extdata", "alkane_ladder.txt", package = "chromConverter")`.
#'
#' @source Andrew W. Legan (<https://orcid.org/0000-0001-7049-9837>). The
#'   binary file of the same run is `FS19_214.gcd` in 'chromConverterExtraTests'.
#'
#' @section License:
#' Released under CC0 1.0
#' (<https://creativecommons.org/publicdomain/zero/1.0/>).
#'
#' @name alkane_ladder
#' @keywords datasets
NULL
