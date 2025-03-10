% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/call_openchrom.R
\name{call_openchrom}
\alias{call_openchrom}
\title{Parse files with OpenChrom}
\usage{
call_openchrom(
  files,
  path_out = NULL,
  format_in,
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  export_format = c("mzml", "csv", "cdf", "animl"),
  return_paths = FALSE,
  verbose = getOption("verbose")
)
}
\arguments{
\item{files}{files to parse}

\item{path_out}{directory to export converted files.}

\item{format_in}{Either \code{msd} for mass spectrometry data, \code{csd} for flame
ionization data, or \code{wsd} for DAD/UV data.}

\item{format_out}{R format. Either \code{matrix}, \code{data.frame} or
\code{data.table}.}

\item{data_format}{Whether to return data in \code{wide} or \code{long} format.}

\item{export_format}{Either  \code{mzml}, \code{csv}, \code{cdf},  \code{animl}.
Defaults to \code{mzml}.}

\item{return_paths}{Logical. If TRUE, the function will return a character
vector of paths to the newly created files.}

\item{verbose}{Logical. Whether to print output from OpenChrom to the console.}
}
\value{
If \code{return_paths} is \code{FALSE}, the function will return a
list of chromatograms (if an appropriate parser is available to import the
files into R). The chromatograms will be returned in \code{matrix} or
\code{data.frame} format according to the value of \code{format_out}. If
\code{return_paths} is \code{TRUE}, the function will return a character
vector of paths to the newly created files.
}
\description{
Writes \code{xml} batch-files and calls OpenChrom file parsers using a
system call to the command-line interface. To use this function
\href{https://lablicate.com/platform/openchrom}{OpenChrom} (version 0.4.0) must be
manually installed. The command line interface is no longer supported in the
latest versions of OpenChrom (starting with version 0.5.0), so the function
will no longer work with these newer versions. OpenChrom 1.4 has been scrubbed
from the internet, but OpenChrom 1.3 is still available from
\href{https://sourceforge.net/projects/openchrom/files/REL-1.3.0/}{SourceForge}.
}
\details{
The \code{call_openchrom} works by creating an \code{xml} batchfile and
feeding it to the OpenChrom command-line interface. OpenChrom batchfiles
consist of \code{InputEntries} (the files you want to convert) and
\code{ProcessEntries} (what you want to do to the files). The parsers are
organized into broad categories by detector-type and output format. The
detector-types are \code{msd} (mass selective detectors), \code{csd}
(current selective detectors, e.g. FID, ECD, NPD), and \code{wsd}
(wavelength selective detectors, e.g.  DAD, and UV/VIS). Thus, when calling
the OpenChrom parsers, you must select one of these three options for the
input format (\code{format_in}).
}
\note{
Activating the OpenChrom command-line will deactivate the graphical
user interface (GUI). Thus, if you wish to continue using the OpenChrom GUI,
it is recommended to create a separate command-line version of OpenChrom to
call from R.
}
\section{Side effects}{
 Chromatograms will be exported in the format specified
by \code{export_format} in the folder specified by \code{path_out}.
}

\references{
Wenig, Philip and Odermatt, Juergen. OpenChrom: A Cross-Platform Open Source
Software for the Mass Spectrometric Analysis of Chromatographic Data. \emph{
BMC Bioinformatics} \bold{11}, no. 1 (July 30, 2010): 405.
\doi{10.1186/1471-2105-11-405}.
}
\seealso{
Other external parsers: 
\code{\link{call_entab}()},
\code{\link{call_rainbow}()},
\code{\link{read_thermoraw}()},
\code{\link{sp_converter}()},
\code{\link{uv_converter}()}
}
\author{
Ethan Bass
}
\concept{external parsers}
