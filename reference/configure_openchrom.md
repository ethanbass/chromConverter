# Configure 'OpenChrom' parser

Configures [OpenChrom](https://lablicate.com/platform/openchrom) to use
command-line interface. Requires OpenChrom version prior to 1.5.0.

## Usage

``` r
configure_openchrom(cli = c("null", "true", "false", "status"), path = NULL)
```

## Arguments

- cli:

  One of `"null"` (default), `"true"`, `"false"` or `"status"`. `"true"`
  and `"false"` rewrite the OpenChrom ini file to enable or disable the
  command-line interface. `"null"` leaves the ini file alone unless the
  interface is disabled, in which case it asks whether to enable it.
  `"status"` reports the current setting without changing anything.

- path:

  Path to the 'OpenChrom' executable (optional). The supplied path is
  saved and used in later calls.

## Value

If `cli` is `"status"`, the string `"true"` or `"false"`, saying whether
the command-line interface is enabled. Otherwise, the path to the
OpenChrom command-line application.

## See also

[`call_openchrom`](https://ethanbass.github.io/chromConverter/reference/call_openchrom.md)

## Author

Ethan Bass

## Examples

``` r
if (FALSE) { # \dontrun{
configure_openchrom(cli = "status")
} # }
```
