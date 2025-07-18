% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_shimadzu_ascii.R
\name{read_shimadzu}
\alias{read_shimadzu}
\title{Read 'Shimadzu' ASCII}
\usage{
read_shimadzu(
  path,
  what = "chroms",
  format_in = NULL,
  include = c("fid", "lc", "dad", "uv", "tic"),
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  peaktable_format = c("chromatographr", "original"),
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw"),
  ms_format = c("data.frame", "list"),
  collapse = TRUE,
  scale = TRUE
)
}
\arguments{
\item{path}{Path to Shimadzu \code{.txt} ASCII file.}

\item{what}{Whether to extract chromatograms (\code{chroms}),
\code{peak_table}, and/or \code{ms_spectra}. Accepts multiple arguments.}

\item{format_in}{This argument is deprecated and is no longer required.}

\item{include}{Which chromatograms to include. Options are \code{fid},
\code{dad}, \code{uv}, \code{tic}, and \code{status}.}

\item{format_out}{R format. Either \code{matrix}, \code{data.frame}, or
\code{data.table}.}

\item{data_format}{Whether to return data in \code{wide} or \code{long} format.}

\item{peaktable_format}{Whether to return peak tables in \code{chromatographr}
or \code{original} format.}

\item{read_metadata}{Whether to read metadata from file.}

\item{metadata_format}{Format to output metadata. Either \code{chromconverter}
or \code{raw}.}

\item{ms_format}{Whether to return mass spectral data as a (long)
\code{data.frame} or a \code{list}.}

\item{collapse}{Logical. Whether to collapse lists that only contain a single
element.}

\item{scale}{Whether to scale the data by the scaling factor present in the
file. Defaults to \code{TRUE}.}
}
\value{
A nested list of elements from the specified \code{file}, where the
top levels are chromatograms, peak tables, and/or mass spectra according to
the value of \code{what}. Chromatograms are returned in the format specified
by \code{format_out} (retention time x wavelength).
}
\description{
Reads 'Shimadzu' ASCII (\code{.txt}) files. These files can be exported from
'Shimadzu LabSolutions' by right clicking on samples in the sample list and
selecting \code{File Conversion:Convert to ASCII}.
}
\examples{
\dontshow{if (interactive()) withAutoprint(\{ # examplesIf}
path <- "tests/testthat/testdata/ladder.txt"
read_shimadzu(path)
\dontshow{\}) # examplesIf}
}
\seealso{
Other 'Shimadzu' parsers: 
\code{\link{read_shimadzu_gcd}()},
\code{\link{read_shimadzu_lcd}()},
\code{\link{read_shimadzu_qgd}()},
\code{\link{read_sz_lcd_2d}()},
\code{\link{read_sz_lcd_3d}()}
}
\author{
Ethan Bass
}
\concept{'Shimadzu' parsers}
