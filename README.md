# chromConverter
chromConverter is an R package to facilitate the conversion of chromatographic data from proprietary binary formats. 

It currently consists of a wrapper around the file parsers included in the [Aston](https://github.com/bovee/aston) package for python.

This includes Agilent Chemstation `.uv`, `.ms`, Masshunter `.sp`, etc.

## Installation
You can install `chromConverter` from GitHub using the devtools package:
```
install.packages("devtools")
devtools::install_github("https://github.com/ethanbass/chromConverter/")
```

## Usage

Convert files by specifying the path to a directory (or a vector of directories) and the appropriate file format.

```
library(chromConverter)
dat <- import_chroms(path, format.in=c("chemstation.uv", "masshunter.dad")
```
