# chromConverter

chromConverter is an R package to facilitate the conversion of chromatographic data from proprietary binary formats. 

It currently consists of a wrapper around the file parsers included in the [Aston](https://github.com/bovee/aston) package for python.

This includes Agilent ChemStation `.uv`, `.ms`, MassHunter `.sp`, etc.

## Installation
The development version of `chromConverter` is hosted on GitHub. You can install
it using `devtools` as follows:

```
install.packages("devtools")
devtools::install_github("https://github.com/ethanbass/chromConverter/")
```

## Usage

Convert files by specifying the path to a directory (or a vector of directories) and the appropriate file format.

```
library(chromConverter)
dat <- read_chroms(path, format.in=c("chemstation.uv", "masshunter.dad")
```
