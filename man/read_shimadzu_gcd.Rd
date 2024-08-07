% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_shimadzu_gcd.R
\name{read_shimadzu_gcd}
\alias{read_shimadzu_gcd}
\title{Shimadzu GCD parser}
\usage{
read_shimadzu_gcd(
  path,
  format_out = c("matrix", "data.frame"),
  data_format = c("wide", "long"),
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw")
)
}
\arguments{
\item{path}{Path to GCD file.}

\item{format_out}{Matrix or data.frame.}

\item{data_format}{Either \code{wide} (default) or \code{long}.}

\item{read_metadata}{Logical. Whether to attach metadata.}

\item{metadata_format}{Format to output metadata. Either \code{chromconverter}
or \code{raw}.}
}
\value{
A 2D chromatogram from the chromatogram stream in \code{matrix} or
\code{data.frame} format, according to the value of \code{format_out}.
The chromatograms will be returned in \code{wide} or \code{long} format
according to the value of \code{data_format}.
}
\description{
Read 2D PDA data stream from 'Shimadzu' GCD files.
}
\details{
A parser to read chromatogram data streams from 'Shimadzu' \code{.gcd} files.
GCD files are encoded as 'Microsoft' OLE documents. The parser relies on the
\href{https://pypi.org/project/olefile/}{olefile} package in Python to unpack the
files. The PDA data is encoded in a stream called \code{PDA 3D Raw Data:3D Raw Data}.
The GCD data stream contains a segment for each retention time, beginning
with a 24-byte header.

The 24 byte header consists of the following fields:
\itemize{
\item 4 bytes: segment label (\code{17234}).
\item 4 bytes: Little-endian integer specifying the sampling interval in milliseconds.
\item 4 bytes: Little-endian integer specifying the number of values in the file.
\item 4 bytes: Little-endian integer specifying the total number of bytes in the file
(However, this seems to be off by a few bytes?).
\item 8 bytes of \code{00}s
}

After the header, the data are simply encoded as 64-bit (little-endian)
floating-point numbers. The retention times can be (approximately?) derived
from the number of values and the sampling interval encoded in the header.
}
\note{
This parser is experimental and may still need some work. It is not
yet able to interpret much metadata from the files.
}
\author{
Ethan Bass
}
