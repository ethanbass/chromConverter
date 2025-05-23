% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_chroms.R
\name{write_andi_chrom}
\alias{write_andi_chrom}
\title{Write ANDI chrom CDF file from chromatogram}
\usage{
write_andi_chrom(x, path_out, sample_name = NULL, lambda = NULL, force = FALSE)
}
\arguments{
\item{x}{A chromatogram in (wide) format.}

\item{path_out}{The path to write the file.}

\item{sample_name}{The name of the file. If a name is not provided, the name
will be derived from the \code{sample_name} attribute.}

\item{lambda}{The wavelength to export (for 2-dimensional chromatograms).
Must be a string matching one the columns in \code{x} or the index of the
column to export.}

\item{force}{Whether to overwrite existing files at the specified path.
Defaults to \code{FALSE}.}
}
\value{
Invisibly returns the path to the written CDF file.
}
\description{
Exports a chromatogram in ANDI (Analytical Data Interchange) chromatography
format (ASTM E1947-98). This format can only accommodate unidimensional data.
For two-dimensional chromatograms, the column to export can be specified
using the \code{lambda} argument. Otherwise, a warning will be generated and
the first column of the chromatogram will be exported.
}
\section{Side effects}{

Exports a chromatogram in ANDI chromatography format (netCDF) in the directory
specified by \code{path_out}. The file will be named according to the value
of \code{sample_name}. If no \code{sample_name} is provided, the
\code{sample_name} attribute will be used if it exists.
}

\seealso{
Other write functions: 
\code{\link{write_chroms}()},
\code{\link{write_mzml}()}
}
\author{
Ethan Bass
}
\concept{write functions}
