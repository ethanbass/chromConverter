# Read a 'Shimadzu' method

Reads the instrument settings stored with a 'Shimadzu' GC run (`.gcd`),
LC run (`.lcd`) or GC-MS run (`.qgd`), such as the oven program,
gradient, flows and column.

## Usage

``` r
read_shimadzu_method(
  path,
  what = NULL,
  format_out = c("data.frame", "tibble", "data.table")
)
```

## Arguments

- path:

  Path to a `.gcd`, `.lcd` or `.qgd` file.

- what:

  One or more modules to read. For GC runs, any of `"oven"`,
  `"injector"`, `"detector"`, `"autosampler"` and `"column"`; for GC-MS
  runs, `"oven"`, `"injector"`, `"column"` and `"ms"`; for LC runs,
  `"pump"`, `"column"` and, with a PDA detector, `"dad"`. Defaults to
  all the modules the run has.

- format_out:

  Class of the tables returned: `"data.frame"`, `"tibble"` or
  `"data.table"`.

## Value

A named list with one element per module.

For GC runs:

**`oven`**: a list with scalar elements `equilibration_time_min`,
`initial_temperature_C` and `initial_hold_min`, plus `program`, a table
of the temperature ramps with columns `rate_C_min`, `temperature_C` and
`hold_min`.

**`injector`**: a list with scalar elements `name`, `temperature_C`,
`split_ratio`, `column_flow_mL_min`, `linear_velocity_cm_s`,
`pressure_kPa`, `total_flow_mL_min` and `purge_flow_mL_min`.

**`detector`**: a list with scalar elements `name`, `temperature_C`,
`h2_flow_mL_min`, `air_flow_mL_min`, `makeup_flow_mL_min` and
`sampling_rate_ms`.

**`autosampler`**: a list with scalar element `injection_volume_uL`.

**`column`**: a list with scalar elements `name`, `length_m`,
`diameter_mm`, `film_thickness_um`, `max_temperature_C` and
`serial_number`.

For GC-MS runs, `oven` and `column` as for GC runs but without
`equilibration_time_min`, `diameter_mm` and `film_thickness_um`;
`injector` with `temperature_C`, `split_ratio`, `column_flow_mL_min`,
`linear_velocity_cm_s`, `total_flow_mL_min` and `purge_flow_mL_min`; and
**`ms`**, a list with scalar elements `start_time_min` and
`end_time_min`, the window in which the mass spectrometer acquires. A
split ratio of -1 appears to mean the split is off. The injector
settings are the values at the start of the run; a method may program
the pressure or purge flow to change later, so a purge flow of 0 can be
the start of a purge program. Unlike the other settings, `temperature_C`
could not be checked against other values: it is identified by its
position, which matches the order of the labelled settings in `.gcd`
files.

For LC runs:

**`pump`**: a list with scalar elements `mode`, the abbreviation
'LabSolutions' stores for the pump mode (e.g. `ISO`, `BGE` or `LPGE`),
`flow_mL_min` and `stop_time_min`, plus `gradient`, a table with columns
`time_min` and `pct_B`. The gradient starts from the pump's initial
concentration of B at time 0 and changes linearly between the times
listed. It is `NULL` for an isocratic run whose time program sets no
concentration, as is `flow_mL_min`.

**`column`**: a list with scalar element `temperature_C`, the
temperature of the column oven, which is `NA` if the oven is not in use.

**`dad`**: the PDA detector, a list with scalar elements
`start_wavelength_nm`, `end_wavelength_nm`, `sampling_interval_ms`,
`end_time_min` and `cell_temperature_C` (`NA` if the cell is not
thermostatted), plus `channels`, a table of the channels the acquisition
method extracts, with columns `channel`, `wavelength_nm` and
`bandwidth_nm`. Data processing can change these channels; the
wavelengths of the peak tables are those of the processed channels.

## Details

Method parameters are also attached automatically to every chromatogram
and peak table read from these files, in the `method_params` metadata
field. This includes all scalar values but leaves out the oven program
and gradient tables, which are only returned by this function. The `ms`
module of a GC-MS run is attached as `ms_params`.

## File format

`.gcd` and `.lcd` files store the method and instrument configuration as
labelled XML, in the `GUC.1.METHOD` and `GUC.1.CONFIG` streams, and the
PDA settings in `PDA.1.METHOD`.

`.qgd` files store the method as unlabelled binary records, one stream
per module, at fixed byte offsets. Values are little-endian float32
unless marked otherwise, and bytes not listed are not read. The same
streams are found in files from 'GCMS-QP2010' and 'QP2020 NX'
instruments:

- `GC-2010 Instrument Parameters/Column Oven Parameter`, 260 bytes:

  - 8: the number of ramps, n (int32).

  - 12, 16: the initial temperature and hold.

  - 20, 100, 180: the rates, target temperatures and holds, each an
    array of 20 slots, of which those past n are zero.

- `GC-2010 Instrument Parameters/Injection Parameter-1`, 512 bytes:

  - 12, 16, 20: the column flow, linear velocity and split ratio.

  - 24, 120, 216, 312: the temperature, total flow, pressure and purge
    flow programs, 96 bytes each: the number of ramps (int32), the
    initial value and hold, then arrays of 7 rates, targets and holds.

- `GCMS Configuration/Column-1`, 832 bytes:

  - 0, 64: the column name and serial number, null-terminated strings of
    up to 64 bytes.

  - 132: the length in tenths of a metre (int32).

  - 140: the maximum temperature (int32).

- `QP5K Instrument Parameters/MS Parameter`, 600 or 664 bytes:

  - 44, 48: the start and end of acquisition in milliseconds (int32).

## See also

[read_chemstation_method](https://ethanbass.github.io/chromConverter/reference/read_chemstation_method.md)
and
[read_agilent_amx](https://ethanbass.github.io/chromConverter/reference/read_agilent_amx.md)
for 'Agilent' methods.

Other 'Shimadzu' parsers:
[`read_shimadzu()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu.md),
[`read_shimadzu_gcd()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu_gcd.md),
[`read_shimadzu_lcd()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu_lcd.md),
[`read_shimadzu_qgd()`](https://ethanbass.github.io/chromConverter/reference/read_shimadzu_qgd.md),
[`read_sz_lcd_2d()`](https://ethanbass.github.io/chromConverter/reference/read_sz_lcd_2d.md),
[`read_sz_lcd_3d()`](https://ethanbass.github.io/chromConverter/reference/read_sz_lcd_3d.md),
[`read_sz_tables()`](https://ethanbass.github.io/chromConverter/reference/read_sz_tables.md)

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # \dontrun{
method <- read_shimadzu_method("path/to/file.gcd")
method$oven$program
} # }
```
