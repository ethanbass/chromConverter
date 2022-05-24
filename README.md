# chromConverter <a href='https://cran.r-project.org/web/packages/chromConverter/'><img src='man/figures/logo.png' align="right" height="160" /></a>

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/chromConverter)](https://cran.r-project.org/package=chromConverter)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/last-week/chromConverter?color=blue)](https://r-pkg.org/pkg/chromConverter)
<!-- badges: end -->

## Overview

chromConverter is an R package to facilitate HPLC-DAD/UV file conversion from proprietary binary formats. 

It currently consists of a wrapper around the file parsers included in the [Aston](https://github.com/bovee/aston) and [Entab](https://github.com/bovee/entab) packages as well as some parsers written natively in R for text-based formats.

## Formats

- Agilent ChemStation CH, FID, MS, MWD, and UV
- Agilent MassHunter DAD (`.sp`)
- Shimadzu LabSolutions GC-FID ascii (`.txt`) format
- Chromeleon UV ascii (`.txt`) format

## Installation

chromConverter can now be installed directly from CRAN:

```
install.packages("chromConverter")
```

Alternatively, the development version of `chromConverter` is hosted on GitHub. You can install
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

For downstream analyses of chromatographic data, you can also check out my package [chromatographR](https://ethanbass.github.io/chromatographR).
