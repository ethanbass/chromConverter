# Configure python environment

Configures python virtual environment or conda environment for parsers
that have python dependencies, according to the value of `what`. While
this should not be necessary in most cases starting with reticulate
`v1.41.0`, this function can be used to create a dedicated
chromConverter environment.

## Usage

``` r
configure_python_environment(
  what = c("venv", "conda"),
  envname = "chromConverter",
  python = reticulate::virtualenv_starter(),
  ...
)
```

## Arguments

- what:

  What kind of virtual environment to create. A python virtual
  environment (`"venv"`) or a conda environment (`"conda"`).

- envname:

  The name of, or path to, a Python virtual environment.

- python:

  Argument to
  [`reticulate::virtualenv_create`](https://rstudio.github.io/reticulate/reference/virtualenv-tools.html),
  specifying the path to a Python interpreter.

- ...:

  Additional arguments to
  [reticulate::virtualenv_create](https://rstudio.github.io/reticulate/reference/virtualenv-tools.html)
  or
  [reticulate::conda_create](https://rstudio.github.io/reticulate/reference/conda-tools.html)
  according to the value of `what`.

## Value

There is no return value.

## Side effects

Creates and configures either a python virtual environment or conda
environment (according to the value of `what`) with all the packages
required for running chromConverter.

## Author

Ethan Bass
