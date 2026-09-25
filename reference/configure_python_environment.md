# Configure python environment

Creates a dedicated python virtual environment (or conda environment)
with the packages required by the parsers that have python dependencies.
This should not be necessary in most cases, since (starting with
reticulate `v1.41.0`) chromConverter declares its python requirements
with
[reticulate::py_require](https://rstudio.github.io/reticulate/reference/py_require.html)
and they are provisioned automatically the first time a python parser is
called. It can still be useful if you need a persistent environment,
e.g. to work offline or to avoid re-resolving packages. It is an error
if an environment named `envname` already exists.

## Usage

``` r
configure_python_environment(
  what = c("venv", "conda"),
  envname = "chromConverter",
  parser = c("all", "aston", "rainbow", "olefile"),
  python = NULL,
  ...
)
```

## Arguments

- what:

  What kind of environment to create. A python virtual environment
  (`"venv"`) or a conda environment (`"conda"`).

- envname:

  The name of, or path to, the environment to create.

- parser:

  Which parser to install requirements for. Either `"all"` (default),
  `"aston"`, `"rainbow"` or `"olefile"`.

- python:

  Path to the Python interpreter passed to
  [reticulate::virtualenv_create](https://rstudio.github.io/reticulate/reference/virtualenv-tools.html).
  Used only when `what = "venv"`. Defaults to `NULL`, in which case
  [reticulate::virtualenv_starter](https://rstudio.github.io/reticulate/reference/virtualenv-tools.html)
  chooses one.

- ...:

  Additional arguments to
  [reticulate::virtualenv_create](https://rstudio.github.io/reticulate/reference/virtualenv-tools.html)
  or
  [reticulate::conda_create](https://rstudio.github.io/reticulate/reference/conda-tools.html)
  according to the value of `what`.

## Value

The name of the environment, invisibly.

## Side effects

Creates and configures either a python virtual environment or conda
environment (according to the value of `what`) with the packages
required for running the specified chromConverter parsers.

## Author

Ethan Bass
