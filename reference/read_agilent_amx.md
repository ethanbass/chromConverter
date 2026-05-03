# Read Agilent AMX method file

Parses an Agilent `.amx` method archive, extracting instrument
parameters from one or more of its driver sub-files.

## Usage

``` r
read_agilent_amx(
  path,
  what = c("dad", "pump", "comp", "sampler"),
  path_out = NULL,
  format_out = c("data.frame", "tibble", "data.table"),
  gradient_format = c("wide", "long")
)
```

## Arguments

- path:

  Path to the `.amx` file.

- what:

  One or more instrument modules to parse. Any combination of `"dad"`,
  `"pump"`, `"comp"`, and `"sampler"`. Defaults to all four.

- path_out:

  Directory into which the archive is extracted. If `NULL` (default), a
  temporary directory is used and cleaned up on exit.

- format_out:

  Class of output (for tables). Either `"data.frame"`, `"tibble"` or
  `"data.table"`.

- gradient_format:

  Whether to return the gradient in `"wide"` (default) or `"long"`
  format.

## Value

A named list with one element per parsed module, plus `"metadata"`.
Elements present depend on `what`; see below for the structure of each.

**`metadata`** — a list with scalar elements:

- `method_name`:

  Original method name.

- `version`:

  Method version string.

- `status`:

  Approval state.

- `created`:

  Creation timestamp (`POSIXct`, UTC).

- `created_by`:

  Username of creator.

- `modified`:

  Last-modified timestamp (`POSIXct`, UTC).

- `modified_by`:

  Username of last modifier.

**`pump`** — a list with scalar elements `flow_mL_min`, `stop_time_min`,
`post_time_min`, `pressure_low_bar`, `pressure_high_bar`, plus:

- `solvents`:

  A data.frame of active solvent channels: `channel`, `percentage`,
  `solvent`.

- `gradient`:

  A data.frame of timetable entries. Wide format (default): `time_min`
  plus one `pct_<channel>` column per active channel. Long format:
  `time_min`, `channel`, `percent`.

**`dad`** — a list with scalar elements `peakwidth_nm`, `slitwidth_nm`,
`uv_lamp_required`, `vis_lamp_required`, `spectra_from_nm`,
`spectra_to_nm`, `spectra_step_nm`, plus:

- `signals`:

  A data.frame of active signals: `id`, `wavelength_nm`, `bandwidth_nm`.

**`comp`** — a list with scalar element `post_time_min`, plus:

- `temp_controls`:

  Two-row data.frame (Left/Right): `side`, `temperature_C`,
  `not_ready_limit_C`, `equilibration_time_min`.

**`sampler`** — a list with scalar elements: `thermostat_installed`,
`draw_speed_uL_min`, `eject_speed_uL_min`, `wait_after_draw_min`,
`injection_volume_uL`, `wash_time_s`.
