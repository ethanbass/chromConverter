# Read an 'Agilent ChemStation' method

Reads the pump, diode array detector, autosampler and column thermostat
settings from the module register files (`LPMP1.REG`, `LDAD1.REG`,
`LALS1.REG` and `LTHM1.REG`) of a 'ChemStation' `.M` method directory.

## Usage

``` r
read_chemstation_method(
  path,
  what = c("pump", "dad", "sampler", "column"),
  format_out = c("data.frame", "tibble", "data.table"),
  gradient_format = c("wide", "long")
)
```

## Arguments

- path:

  Path to a `.M` directory, or to a `.D` directory holding a copy of its
  method.

- what:

  One or more instrument modules to read: any combination of `"pump"`,
  `"dad"`, `"sampler"` and `"column"`. Defaults to all four.

- format_out:

  Class of the `solvents`, `gradient`, `signals` and `temp_controls`
  tables: `"data.frame"`, `"tibble"` or `"data.table"`.

- gradient_format:

  Whether to return the gradient in `"wide"` (default) or `"long"`
  format.

## Value

A named list with one element per module read. A module the method has
no register file for is left out with a warning, and it is an error if
none of the requested modules are present. The `sampler` module is
returned as `autosampler`. The elements follow
[read_agilent_amx](https://ethanbass.github.io/chromConverter/reference/read_agilent_amx.md):

**`pump`**: a list with scalar elements `flow_mL_min`, `stop_time_min`,
`post_time_min`, `pressure_low_bar` and `pressure_high_bar`, plus:

- `solvents`: a table of the channels in use, with columns `channel`,
  `percentage` (at the start of the run) and `solvent` (the name entered
  for it, or `NA`).

- `gradient`: the timetable. Wide format: `time_min` and a
  `pct_<channel>` column per channel in use, plus `flow_mL_min` if the
  timetable sets the flow. Long format: `time_min`, `channel` and
  `percent`, where a `flow` channel holds the flow in mL/min. An empty
  timetable gives the starting composition at time 0.

**`dad`**: a list with scalar elements `spectra_from_nm`,
`spectra_to_nm`, `spectra_step_nm`, `uv_lamp_required` and
`vis_lamp_required`, plus `signals`, a table of the stored signals with
columns `id`, `wavelength_nm`, `bandwidth_nm`, `reference_nm` and
`reference_bandwidth_nm` (`NA` without a reference).

**`autosampler`**: a list with scalar elements `injection_volume_uL`,
`draw_speed_uL_min` and `eject_speed_uL_min`.

**`column`**: a list holding `temp_controls`, a two-row table (`Left`,
`Right`) with columns `side` and `temperature_C`. A temperature stored
below absolute zero (e.g. -274) is `NA`; this may mean no temperature
was set.

## Details

Depending on how 'ChemStation' is configured, a `.D` directory may hold
a copy of the method it was acquired with, in `ACQ.M` or, in older
revisions, `RUN.M`. `path` can then be the `.D` directory itself. Its
`DA.M` holds the data analysis method instead, whose pump settings may
not be those the run was acquired with; reading it gives a warning.

The gradient follows the pump's timetable, with each parameter changing
linearly between the times it is set. Channel A is the pump's primary
channel and delivers whatever the other channels leave, so it is always
included and its share is computed rather than read.

## See also

[read_agilent_amx](https://ethanbass.github.io/chromConverter/reference/read_agilent_amx.md)
for 'OpenLab CDS' methods, and
[read_agilent_d](https://ethanbass.github.io/chromConverter/reference/read_agilent_d.md)
with `what = "instrument"` for the pump's record of what it delivered.

Other 'Agilent' parsers:
[`read_agilent_d()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_d.md),
[`read_agilent_dx()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_dx.md),
[`read_agilent_rslt()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_rslt.md),
[`read_chemstation_ch()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_ch.md),
[`read_chemstation_csv()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_csv.md),
[`read_chemstation_logs()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_logs.md),
[`read_chemstation_ms()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_ms.md),
[`read_chemstation_reports()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_reports.md),
[`read_chemstation_uv()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_uv.md)

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # \dontrun{
method <- read_chemstation_method("path/to/METHOD.M")
method$pump$gradient
} # }
```
