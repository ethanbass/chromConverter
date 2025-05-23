% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_peaklist.R
\name{read_peaklist}
\alias{read_peaklist}
\title{Read peak lists}
\usage{
read_peaklist(
  paths,
  find_files,
  format_in = c("chemstation", "shimadzu_fid", "shimadzu_dad", "shimadzu_lcd",
    "shimadzu_gcd"),
  pattern = NULL,
  data_format = c("chromatographr", "original"),
  metadata_format = c("chromconverter", "raw"),
  read_metadata = TRUE,
  progress_bar,
  cl = 1
)
}
\arguments{
\item{paths}{Paths to files or folders containing peak list files.}

\item{find_files}{Logical. Set to \code{TRUE} (default) if you are providing
the function with a folder or vector of folders containing the files.
Otherwise, set to\code{FALSE}.}

\item{format_in}{Format of files to be imported/converted. Current options
include: \code{chemstation}, \code{shimadzu_fid}, \code{shimadzu_dad},
\code{shimadzu_lcd}, and \code{shimadzu_gcd}.}

\item{pattern}{pattern (e.g. a file extension). Defaults to NULL, in which
case file extension will be deduced from \code{format_in}.}

\item{data_format}{Either \code{chromatographr} or \code{original}.}

\item{metadata_format}{Format to output metadata. Either \code{chromconverter} or
\code{raw}.}

\item{read_metadata}{Logical, whether to attach metadata (if it's available).
Defaults to TRUE.}

\item{progress_bar}{Logical. Whether to show progress bar. Defaults to
\code{TRUE} if \code{\link[pbapply]{pbapply}} is installed.}

\item{cl}{Argument to \code{\link[pbapply]{pbapply}} specifying the number
of clusters to use or a cluster object created by
\code{\link[parallel]{makeCluster}}. Defaults to 1.}
}
\value{
A list of chromatograms in \code{matrix} or \code{data.frame} format,
according to the value of \code{format_out}.
}
\description{
Reads peak lists from specified folders or vector of paths.
}
\examples{
\dontshow{if (interactive()) withAutoprint(\{ # examplesIf}
path <- "tests/testthat/testdata/dad1.uv"
chr <- read_chroms(path, find_files = FALSE, format_in = "chemstation_uv")
\dontshow{\}) # examplesIf}
}
\author{
Ethan Bass
}
