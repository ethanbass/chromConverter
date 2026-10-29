# Write Agilent AMX method file

Writes a copy of an 'OpenLab CDS' acquisition method (`.amx`) with a new
pump gradient, flow, stop time, post time or column temperature. All
other settings are copied unchanged from the template.

## Usage

``` r
write_agilent_amx(
  path,
  path_out,
  gradient = NULL,
  flow = NULL,
  stop_time = NULL,
  post_time = NULL,
  temperature = NULL,
  force = FALSE
)
```

## Arguments

- path:

  Path to the template `.amx` file.

- path_out:

  Path to write the new method.

- gradient:

  A table of solvent composition over time (see Details). `NULL`
  (default) keeps the template's gradient.

- flow:

  Flow rate in mL/min. `NULL` (default) keeps the template's value.

- stop_time:

  Pump stop time in minutes. `NULL` (default) keeps the template's
  value.

- post_time:

  Pump post time in minutes, at least 0.01. `0` turns the post time off.
  `NULL` (default) keeps the template's value.

- temperature:

  Column temperature in degrees Celsius. `NULL` (default) keeps the
  template's value.

- force:

  Whether to overwrite `path_out` if it exists. Defaults to `FALSE`.

## Value

Invisibly returns `path_out`.

## Details

The writer has only been tested on methods for a G7104C quaternary pump
and a G7116A column compartment. Other modules may lay out their methods
differently. If the writer fails on one of your methods, or writes one
that OpenLab will not load, please [open an
issue](https://github.com/ethanbass/chromConverter/issues) and attach
the template. Open each written method in OpenLab and check its settings
before you run it.

`gradient` takes the table that
[`read_agilent_amx()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_amx.md)
returns as `pump$gradient`, in wide or long format. It needs a
percentage for every solvent channel the template uses, and each row
must add up to 100. It must have a row at time zero, which sets the
starting composition. The other rows replace the composition steps in
the pump timetable. Flow steps in the timetable are kept, and `gradient`
cannot change them. If the template has flow steps, drop the
`flow_mL_min` column (or the `"flow"` rows in long format) from the
table
[`read_agilent_amx()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_amx.md)
returns.

Setting `flow` gives a constant flow rate and removes the template's
flow steps.

`temperature` sets both sides of the column compartment, and the
template must already control the temperature. A template whose
compartment is set to "Not Controlled" gives an error.

Setting `stop_time` also sets the run time of the method. The checksum
of each edited part is recalculated. The `Agilent/ReportableInformation`
part, which repeats the settings as formatted text, is not rewritten and
still describes the template.

The writer needs the 'zip' package.

## See also

[`read_agilent_amx()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_amx.md)

Other write functions:
[`write_andi_chrom()`](https://ethanbass.github.io/chromConverter/reference/write_andi_chrom.md),
[`write_andi_ms()`](https://ethanbass.github.io/chromConverter/reference/write_andi_ms.md),
[`write_chroms()`](https://ethanbass.github.io/chromConverter/reference/write_chroms.md),
[`write_mzml()`](https://ethanbass.github.io/chromConverter/reference/write_mzml.md)

## Examples

``` r
if (FALSE) { # \dontrun{
gradient <- data.frame(time_min = c(0, 2, 8),
                       pct_A = c(70, 70, 5),
                       pct_B = c(30, 30, 95))
write_agilent_amx("template.amx", "new.amx", gradient = gradient,
                  stop_time = 10)
} # }
```
