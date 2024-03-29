% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_chromeleon.R
\name{read_chromeleon}
\alias{read_chromeleon}
\title{Chromeleon ASCII reader}
\usage{
read_chromeleon(
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

\item{read_metadata}{Whether to read metadata from file.}

\item{metadata_format}{Format to output metadata. Either \code{chromconverter} or
\code{raw}.}
}
\value{
A chromatogram in the format specified by \code{format_out}.
(retention time x wavelength).
}
\description{
Reads 'Thermo Fisher Chromeleon™ CDS' files into R.
}
\author{
Ethan Bass
}
