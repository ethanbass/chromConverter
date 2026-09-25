# Read TIC from 'Shimadzu' TLM stream

Reads total ion current chromatograms from the `TLM Raw Data` streams of
'Shimadzu LabSolutions' `.lcd` files.

## Usage

``` r
read_sz_tlm_tic(
  path,
  format_out = c("matrix", "data.frame", "data.table"),
  data_format = c("wide", "long"),
  read_metadata = TRUE,
  metadata_format = "shimadzu_lcd",
  what = c("tic", "sumtic")
)
```

## Arguments

- path:

  Path to `.lcd` file.

- format_out:

  Class of output. Either `matrix`, `data.frame`, or `data.table`.

- data_format:

  Either `wide` (default) or `long`.

- read_metadata:

  Logical. Whether to attach metadata.

- metadata_format:

  Format to output metadata.

- what:

  Which curve to return: `tic` (default) for the per-spectrum total ion
  current, or `sumtic` for the per-cycle curve.

## Value

A named list of chromatograms, one per acquisition event, or a single
chromatogram for `what = "sumtic"`.

## Details

`TIC Data` holds two little-endian `uint32`s per spectrum: the total ion
current and a flag that is `0x80000000` when any point in the spectrum
saturated the detector. The retention times come from the
`Retention Time` stream. Neither stream needs the spectra themselves to
be decompressed, so this is much cheaper than `read_sz_tlm`.

Each acquisition event is returned as a separate chromatogram, named
`Event 1`, `Event 2` and so on, since events can differ in polarity, MS
level and scan range, and are interleaved in acquisition order.

The `SumTIC Data` stream (one 12-byte record per cycle: retention time,
value, saturation flag) is a curve computed by the instrument. It is not
a plain sum of the per-spectrum TICs: in the MRM files examined it
matches the cycle mean (which, with one event per cycle, is also the
sum), in the scan files the cycle mean divided by six, and in a file
mixing SIM with product-ion scans neither. It is therefore returned as
stored rather than recomputed.

## Author

Ethan Bass
