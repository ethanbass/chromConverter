# Read 'Agilent ChemStation' log files

Reads the event log 'Agilent ChemStation' writes to the folder of each
sequence, or the `RUN.LOG` it writes to each `.D` folder when that file
is named in `paths`. These record instrument errors, aborted runs and
routine readings such as pump pressure and column temperature.

## Usage

``` r
read_chemstation_logs(
  paths,
  what = c("injections", "problems", "events"),
  format_out = "data.frame"
)
```

## Arguments

- paths:

  Paths to 'ChemStation' `.LOG` files, or to directories to search for
  sequence logs.

- what:

  What to return: one row per injection (`injections`, the default),
  only the events marked as a `problem` (`problems`), or every event
  (`events`).

- format_out:

  Class of the returned object: `data.frame` (the default) or
  `data.table`.

## Value

A `data.frame` or `data.table`. For `injections`, one row per injection,
with columns:

- `sequence`: the name of the 'ChemStation' sequence, taken from the
  log's own sequence events or, failing those, from the name of the
  file. `NA` for a `RUN.LOG`, which has neither.

- `folder`: the folder the sequence's data are in, which can differ from
  `sequence`. This is the folder holding the log, or for a `RUN.LOG` the
  folder holding its `.D` folder.

- `injection`: numbers the injections in the order the log starts them.

- `data_file`: the `.D` folder the log names for the injection. The log
  records it only once the run's data are analyzed, so an aborted
  injection has none, and its case can differ from the folder on disk.

- `sample`: the sample, as the log names it.

- `start`: `POSIXct`, UTC. The `run_datetime` of the traces and reports
  in a `.D` folder is instead the instrument's local time labeled as
  UTC, so match injections to them by `data_file` rather than by time.

- `minutes`: from the injection's first event to its last.

- `status`: `completed`, `aborted`, `stopped by user` or `incomplete`.

- `pressure_start`, `pressure_end`: the first and last reading of pump
  1, in bar.

- `problems`: the injection's problem messages, separated by `"; "`.

For `events`, one row per event, with `sequence`, `folder`, `injection`
and `data_file` as above, and:

- `time`: `POSIXct`, UTC, as recorded in the event's header. The local
  time printed in the log's text is not returned.

- `problem`: whether the event reports an alarm from an instrument
  module (any event from a module with a non-zero `event_code`, such as
  a leak or a shutdown), a method that was aborted, timed out, stopped
  by an instrument error or stopped by the user, or a sequence that was
  terminated or stopped.

- `event_code`, `module_id`: hexadecimal strings, since 'Agilent' does
  not document them. The same module always carries the same `module_id`
  (for example, `1da6` for the pump in 'Agilent 1100' logs).

- `source`: e.g. `"1100 PMP 1"`, `"Method"` or `"Sequence"`.

- `message`: the event's text. The log splits a message longer than 45
  characters over several events, which are joined, but stores a message
  of exactly 45 characters without its last character.

Events of the sequence itself, events between injections such as loading
the next method, and method runs that acquire no data, such as a
data-analysis-only re-run of the sequence, have no `injection` or
`data_file`.

For `problems`, the events marked as a `problem`, without that column
and with `incident`, which numbers the runs of problem events in the
same log that are less than a minute apart, such as a leak and the
shutdowns that follow it.

## Details

Directories in `paths` are searched, including their subdirectories, for
sequence logs: every `.LOG` file outside a `.D` folder. `RUN.LOG` files
are left out, since the sequence log holds their events. A file found
this way that is not a 'ChemStation' log is skipped with a warning; a
file named in `paths` that is not one is an error.

Tested on logs from 'ChemStation' revisions A.10.02, B.01.03 and
B.04.02.

## See also

Other 'Agilent' parsers:
[`read_agilent_d()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_d.md),
[`read_agilent_dx()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_dx.md),
[`read_agilent_rslt()`](https://ethanbass.github.io/chromConverter/reference/read_agilent_rslt.md),
[`read_chemstation_ch()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_ch.md),
[`read_chemstation_csv()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_csv.md),
[`read_chemstation_method()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_method.md),
[`read_chemstation_ms()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_ms.md),
[`read_chemstation_reports()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_reports.md),
[`read_chemstation_uv()`](https://ethanbass.github.io/chromConverter/reference/read_chemstation_uv.md)

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # interactive()
read_chemstation_logs("tests/testthat/testdata/chemstation_sequence.LOG")
read_chemstation_logs("path/to/sequences", what = "problems")
}
```
