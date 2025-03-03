% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_chroms.R
\name{write_chroms}
\alias{write_chroms}
\title{Export chromatograms}
\usage{
write_chroms(
  chrom_list,
  path_out,
  export_format = c("mzml", "cdf", "csv"),
  what = "",
  force = FALSE,
  show_progress = TRUE,
  verbose = getOption("verbose"),
  ...
)
}
\arguments{
\item{chrom_list}{A list of chromatograms.}

\item{path_out}{Path to directory for writing files.}

\item{export_format}{Format to export files: either \code{mzml}, \code{cdf},
or \code{csv}.}

\item{what}{What to write. Either \code{MS1} or \code{chrom}.}

\item{force}{Logical. Whether to overwrite existing files. Defaults to \code{TRUE}.}

\item{show_progress}{Logical. Whether to show progress bar. Defaults to \code{TRUE}.}

\item{verbose}{Logical. Whether to print verbose output.}

\item{...}{Additional arguments to write function.}
}
\description{
Export chromatograms
}
\seealso{
Other write functions: 
\code{\link{write_andi_chrom}()},
\code{\link{write_mzml}()}
}
\author{
Ethan Bass
}
\concept{write functions}
