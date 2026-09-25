# Read 'Varian' SMS

Reads 'Varian Workstation' SMS files.

## Usage

``` r
read_varian_sms(
  path,
  what = c("MS1", "TIC", "BPC"),
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = "long",
  read_metadata = TRUE,
  collapse = TRUE
)
```

## Arguments

- path:

  Path to a 'Varian' `.SMS` file.

- what:

  Which streams to get: mass spectra (`MS1`), the total ion chromatogram
  (`TIC`) and/or the base peak chromatogram (`BPC`). Accepts multiple
  arguments. Defaults to all three.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Whether to return data in `long` (default) or `wide` format. Mass
  spectra are always returned in `long` format.

- read_metadata:

  Logical. Whether to attach metadata. Defaults to `TRUE`.

- collapse:

  Logical. Whether to collapse lists that only contain a single element.
  Defaults to `TRUE`.

## Value

A chromatogram or list of chromatograms from the specified file,
according to the value of `what`. Chromatograms are returned in the
format specified by `format_out`, except that mass spectra are returned
as a `data.table` when `format_out` is `matrix`.

## Details

Varian SMS files begin with a `DIRECTORY` holding the offsets of each
section. The first section is `MSData`, which begins at byte `3238` in
every file seen so far, and is itself divided in two. The first part,
after a short header, holds chromatogram data, one record per scan: the
scan number, the retention time (as a 64-bit float), the ion time (µsec,
as a 2-byte unsigned integer), the total ion chromatogram (TIC), the
base peak chromatogram (BPC), and further unidentified fields. The scan
numbers and the TIC and BPC intensities are stored as 4-byte
little-endian integers. A run of null bytes then separates this part
from the segments holding the mass spectra.

The mass spectra are encoded differently. Each scan is a series of
variable-length values, separated from the next scan by two null bytes.
Within a scan the values are paired: the first of each pair is the
delta-encoded mass-to-charge ratio and the second is the intensity. Each
value is a big-endian integer whose length, `1 + (d %/% 4)` bytes, and
bit width are set by its leading hexadecimal digit (`d`). Values
beginning with `0-3` are single bytes. For `d >= 4`, the lowest `n` bits
are preserved according to the following scheme:

- d = 4-5 -\> preserve lowest 13 bits

- d = 6-7 -\> preserve lowest 14 bits

- d = 8-9 -\> preserve lowest 21 bits

- d = 10-11 (A-B) -\> preserve lowest 22 bits

- d = 12-13 (C-D) -\> preserve lowest 27 bits

- d = 14-15 (E-F) -\> preserve lowest 28 bits

No file seen so far carries a leading digit above `C`, so the rules for
`D` and for `E`-`F` are extrapolated from the others rather than
observed.

## Note

There is still only limited support for the extraction of metadata from
this file format.

## See also

Other 'Varian' parsers:
[`read_varian_peaklist()`](https://ethanbass.github.io/chromConverter/reference/read_varian_peaklist.md)

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # \dontrun{
read_varian_sms(path)
} # }
```
