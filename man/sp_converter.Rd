% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/call_aston.R
\name{sp_converter}
\alias{sp_converter}
\title{Converter for 'Agilent MassHunter' UV files}
\usage{
sp_converter(
  file,
  format_out = c("matrix", "data.frame"),
  data_format = c("wide", "long"),
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw")
)
}
\arguments{
\item{file}{path to file}

\item{format_out}{R format. Either \code{matrix} or \code{data.frame}.}

\item{data_format}{Whether to return data in \code{wide} or \code{long} format.}

\item{read_metadata}{Logical. Whether to read metadata and attach it to the
chromatogram.}

\item{metadata_format}{Format to output metadata. Either \code{chromconverter}
or \code{raw}.}
}
\value{
A chromatogram in \code{data.frame} format (retention time x wavelength).
}
\description{
Converts a single chromatogram from MassHunter \code{.sp} format to R
\code{data.frame}.
}
\details{
Uses the \href{https://github.com/bovee/aston}{Aston} file parser.
}
