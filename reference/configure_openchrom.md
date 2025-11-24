# Configure 'OpenChrom' parser

Configures [OpenChrom](https://lablicate.com/platform/openchrom) to use
command-line interface. Requires OpenChrom version prior to 0.5.0.

## Usage

``` r
configure_openchrom(cli = c("null", "true", "false", "status"), path = NULL)
```

## Arguments

- cli:

  Defaults to NULL. If "true", R will rewrite openchrom ini file to
  enable CLI. If "false", R will disable CLI. If NULL, R will not modify
  the ini file.

- path:

  Path to 'OpenChrom' executable (Optional). The supplied path will
  overwrite the current path.

## Value

If `cli` is set to `"status"`, returns a Boolean value indicating
whether 'OpenChrom' is configured correctly. Otherwise, returns the path
to OpenChrom command-line application.

## See also

[call_openchrom](https://ethanbass.github.io/chromConverter/reference/call_openchrom.md)

## Author

Ethan Bass
