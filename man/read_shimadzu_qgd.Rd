% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_shimadzu_qgd.R
\name{read_shimadzu_qgd}
\alias{read_shimadzu_qgd}
\title{Read 'Shimadzu' QGD files}
\usage{
read_shimadzu_qgd(
  path,
  what = c("MS1", "TIC"),
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  read_metadata = TRUE,
  metadata_format = c("chromconverter", "raw"),
  collapse = TRUE
)
}
\arguments{
\item{path}{Path to 'Shimadzu' \code{.qgd} file.}

\item{what}{What stream to get: current options are \code{MS1} and/or
\code{TIC}. If a stream is not specified, the function will return both
streams.}

\item{format_out}{Matrix or data.frame.}

\item{data_format}{Either \code{wide} (default) or \code{long}. This argument
applies only to TIC and BPC data, since MS data will always be returned in
long format.}

\item{read_metadata}{Logical. Whether to attach metadata. Defaults to \code{TRUE}.}

\item{metadata_format}{Format to output metadata. Either \code{chromconverter}
or \code{raw}.}

\item{collapse}{Logical. Whether to collapse lists that only contain a single
element. Defaults to \code{TRUE}.}
}
\value{
A 2D chromatogram from the chromatogram stream in \code{matrix},
\code{data.frame}, or \code{data.table} format, according to the value of
\code{format_out}. The chromatograms will be returned in \code{wide} or
\code{long} format according to the value of \code{data_format}.

A chromatogram or list of chromatograms in the format specified by
\code{data_format} and \code{format_out}. If \code{data_format} is \code{wide},
the chromatogram(s) will be returned with retention times as rows and a
single column for the intensity. If \code{long} format is requested, two
columns will be returned: one for the retention time and one for the intensity.
The \code{format_out} argument determines whether chromatograms are returned
as a \code{matrix}, \code{data.frame}, or \code{data.table}. Metadata can be
attached to the chromatogram as \code{\link{attributes}} if
\code{read_metadata} is \code{TRUE}.
}
\description{
Reads 'Shimadzu GCMSsolution' \code{.qgd} GC-MS data files.
}
\details{
The MS data is stored in the "GCMS Raw Data" storage, which contains a
\code{MS Raw Data} stream with MS scans, a \code{TIC Data} stream containing
the total ion chromatogram, and a \code{Retention Time} stream containing the
retention times. All known values are little-endian. The retention time
stream is a simple array of 4-byte integers. The TIC stream is a simple array
of 8-byte integers corresponding to retention times stored in the
retention time stream. The MS Raw Data stream is blocked by retention time.
Each block begins with a header consisting of the following elements:
\itemize{
\item scan number (4-byte integer)
\item retention time (4-byte integer)
\item unknown (12-bytes)
\item number of bytes in intensity values (2-byte integer)
\item unknown (8-bytes)
}

After the header, the rest of the block consists of an array of mz values and
intensities. The mz values are encoded as 2-byte integers where each mz value
is scaled by a factor of 20. Intensities are encoded as (unsigned) integers
with variable byte-length defined by the value in the header.
}
\note{
This parser is experimental and may still need some work. It is not
yet able to interpret much metadata from the files.
}
\seealso{
Other 'Shimadzu' parsers: 
\code{\link{read_shimadzu}()},
\code{\link{read_shimadzu_gcd}()},
\code{\link{read_shimadzu_lcd}()},
\code{\link{read_sz_lcd_2d}()},
\code{\link{read_sz_lcd_3d}()}
}
\author{
Ethan Bass
}
\concept{'Shimadzu' parsers}
