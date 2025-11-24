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

  Path to 'Varian' `.SMS` files.

- what:

  Whether to extract chromatograms (`chroms`) and/or `MS1` data. Accepts
  multiple arguments.

- format_out:

  R format. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Either `wide` (default) or `long`. This argument applies only to TIC
  and BPC data, since MS data will always be returned in long format.

- read_metadata:

  Whether to read metadata from the file.

- collapse:

  Logical. Whether to collapse lists that only contain a single element.

## Value

A chromatogram or list of chromatograms from the specified `file`,
according to the value of `what`. Chromatograms are returned in the
format specified by `format_out`.

## Details

Varian SMS files begin with a "DIRECTORY" with offsets for each section.
The first section (in all the files I've been able to inspect) is
"MSData" generally beginning at byte 3238. This MSdata section is in
turn divided into two sections. The first section (after a short header)
contains chromatogram data. Some of the information found in this
section includes scan numbers, retention times, (as 64-bit floats), the
total ion chromatogram (TIC), the base peak chromatogram (BPC), ion time
(µsec), as well as some other unidentified information. The scan numbers
and intensities for the TIC and BPC are stored at 4-byte little-endian
integers. Following this section, there is a series of null bytes,
followed by a series of segments containing the mass spectra.

The encoding scheme for the mass spectra is somewhat more complicated.
Each scan is represented by a series of values of variable length
separated from the next scan by two null bytes. Within these segments,
values are paired. The first value in each pair represents the
delta-encoded mass-to-charge ratio, while the second value represents
the intensity of the signal. Values in this section are variable-length,
big-endian integers that are encoded using a selective bit masking based
on the leading digit (`d`) of each value. The length of each integer
seems to be determined as 1 + (d %/% 4). Integers beginning with digits
0-3 are simple 2-byte integers. If d \>= 4, values are determined by
masking to preserve the lowest `n` bits according to the following
scheme:

- d = 4-5 -\> preserve lowest 13 bits

- d = 6-7 -\> preserve lowest 14 bits

- d = 8-9 -\> preserve lowest 21 bits

- d = 10-11 (A-B) -\> preserve lowest 22 bits

- d = 12-13 (C-D) -\> preserve lowest 27 bits

- d = 14-15 (E-F) -\> preserve lowest 28 bits (?)

## Note

There is still only limited support for the extraction of metadata from
this file format. Also, the timestamp conversions aren't quite right.

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
