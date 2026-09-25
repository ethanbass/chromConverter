# Read 'Shimadzu' peak tables

Read the integration results 'LabSolutions' stored alongside the raw
data in a 'Shimadzu' OLE container (`.lcd` or `.gcd`), one table per
channel.

## Usage

``` r
read_sz_tables(path, format_out = "data.frame")
```

## Arguments

- path:

  Path to a 'Shimadzu' OLE file (`.lcd` or `.gcd`).

- format_out:

  Class of output. Either `data.frame` or `data.table`. A peak table is
  heterogeneous and has no useful matrix representation, so `matrix`
  resolves to `data.table`, as it does for mass spectra.

## Value

A named list with one peak table per channel, named for the stream it
was read from. A table that cannot be parsed is returned as `NA` with a
message, so one bad channel does not lose the others.

## Details

Each table lives in its own stream, a short header followed by one
fixed-length record per peak. Only chromatographic tables are read:
streams named `PT-` plus the channel (`PT-LC.1.1.DET.1.CH#1`, matching
that channel's `channel_id` metadata), or `Peak Table-` plus a number
(`Peak Table-100`). The tables a mass spectrometry run produces
(`Mass Peak Table`, `Compound Peak Table`, `Ident Peak Table`) have
layouts of their own and are skipped.

There are two layouts. **V0** has no magic number and opens with an
8-byte header: the peak count as a `uint32`, then four unparsed bytes.
**V1** opens with the magic number `56 45 52 31` (`VER1`), the peak
count as a `uint32`, and twelve further unparsed bytes, for a 20-byte
header.

In every file seen so far the layout follows the stream name: `PT-`
streams, under `LSS Data Processing`, are V1, and `Peak Table-` streams,
under `LC Data Processing`, are V0. The parser still dispatches on the
magic number rather than on the name of the stream, since the name is
the weaker signal.

Retention, initial and final times are stored in milliseconds and
converted to minutes, as elsewhere in the package. All values are
little-endian.

A V0 record is 280 bytes:

|            |              |                                 |
|------------|--------------|---------------------------------|
| **Offset** | **Type**     | **Field**                       |
| 0–3        | `uint32`     | Unparsed (peak number?)         |
| 4–7        | `uint32`     | Retention time (`R.time`)       |
| 8–15       | `double`     | Area                            |
| 16–23      | `double`     | Unparsed                        |
| 24–31      | `double`     | Height                          |
| 32–39      | `double`     | Unparsed                        |
| 40–55      | `uint32` x 4 | Unparsed                        |
| 56–59      | `uint32`     | Start of the peak (`I.time`)    |
| 60–63      | `uint32`     | End of the peak (`F.time`)      |
| 64–67      | `uint32`     | Area/height ratio x 1000 (`AH`) |
| 68–215     |              | Unparsed                        |
| 216–223    | `double`     | Theoretical plates (`Plate.no`) |
| 224–231    | `double`     | Plate height (`Plate.ht`)       |
| 232–239    | `double`     | Tailing factor                  |
| 240–247    | `double`     | Resolution                      |
| 248–255    | `double`     | Separation factor               |
| 256–279    |              | Unparsed                        |

A V1 record is longer and its length is not fixed by the format: it is
derived as `(stream size - 20) / peak count`. The first 728 bytes are
the part this parser reads, and any remainder is skipped.

|  |  |  |
|----|----|----|
| **Offset** | **Type** | **Field** |
| 0–3 | `uint32` | Unparsed (peak number?) |
| 4–7 | `uint32` | Retention time (`R.time`) |
| 8–15 | `double` | Area |
| 16–23 | `double` | Unparsed |
| 24–31 | `double` | Height |
| 32–39 | `double` | Unparsed |
| 40–55 | `uint32` x 4 | Unparsed |
| 56–59 | `uint32` | Start of the peak (`I.time`) |
| 60–63 | `uint32` | End of the peak (`F.time`) |
| 64–71 | `double` | Area/height ratio (`AH`) |
| 72–175 |  | Unparsed |
| 176–183 | `double` | Concentration (`Conc`) |
| 184–187 | `int32` | Compound identifier (`ID`), `0` for an unidentified peak |
| 188–239 |  | Unparsed |
| 240–247 | `double` | Retention factor (`k`) |
| 248–255 | `double` | Theoretical plates (`Plate.no`) |
| 256–375 |  | Unparsed, with an unidentified `double` at 312 |
| 376–383 | `double` | Plate height (`Plate.ht`) |
| 384–503 |  | Unparsed, with an unidentified `double` at 440 |
| 504–511 | `double` | Tailing factor |
| 512–519 | `double` | Resolution |
| 520–639 |  | Unparsed, with unidentified `double`s at 568 and 632 |
| 640–647 | `double` | Separation factor |
| 648–711 |  | Unparsed |
| 712–719 | `double` | Concentration, percent |
| 720–727 | `double` | Concentration, normalized |

Two things the tables above do not show. `AH` is a scaled integer in V0
but a `double` in V1, so the same field has to be decoded differently in
each. The V1 values are also spaced rather than packed: seven doubles
lie exactly 64 bytes apart — `Plate.no` (248), `Plate.ht` (376) and the
tailing factor (504), with unidentified values at 312, 440, 568 and 632
— while `k` (240), the resolution (512) and the separation factor (640)
each sit 8 bytes to one side of that grid, paired with the value on it.
Each derived quantity therefore seems to occupy a 64-byte slot and use
only the first eight bytes of it. The field names are this package's
reading of the format rather than the vendor's own.

A V0 record is not always 280 bytes: the `Peak Table-100` streams under
`LC Data Processing` hold 544-byte records whose first 256 bytes follow
the table above, with the retention factor (`k`) as a `double` at 208.
As for V1, the record length is derived as
`(stream size - 8) / peak count` and any remainder beyond 280 bytes is
skipped. A table whose records are shorter than 280 bytes, or do not
divide the stream evenly, is rejected.

## See also

Other 'Shimadzu' parsers:
[`read_shimadzu()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu.md),
[`read_shimadzu_gcd()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu_gcd.md),
[`read_shimadzu_lcd()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu_lcd.md),
[`read_shimadzu_qgd()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu_qgd.md),
[`read_sz_lcd_2d()`](https://ethanbass.github.io/chromConverter/reference/read_sz_lcd_2d.md),
[`read_sz_lcd_3d()`](https://ethanbass.github.io/chromConverter/reference/read_sz_lcd_3d.md)

## Author

Ethan Bass
