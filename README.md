# chromConverter <a href='https://cran.r-project.org/web/packages/chromConverter/'><img src='man/figures/logo.png' align="right" height="160" /></a>

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/chromConverter)](https://cran.r-project.org/package=chromConverter)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/last-week/chromConverter?color=blue)](https://r-pkg.org/pkg/chromConverter)
<!-- badges: end -->

## Overview

chromConverter is an R package to facilitate HPLC-DAD/UV file conversion from proprietary binary formats. 

It currently consists of a wrapper around the file parsers included in the [Aston](https://github.com/bovee/aston) and [Entab](https://github.com/bovee/entab) packages as well as some parsers written natively in R for text-based formats. The Aston parsers are deprecated since the package is no longer actively supported. It is recommended to use the newer `entab` parsers.

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

Some of the parsers rely on software that must be manually installed.

#### Entab

To use parsers from `entab`, a local installation of [Rust](https://www.rust-lang.org/tools/install) is necessary. After installing Rust, you can install `entab` from github:

```
devtools::install_github("https://github.com/bovee/entab/")
```

## Usage

Convert files by specifying the path to a directory (or a vector of directories) and the appropriate file format.

```
library(chromConverter)
dat <- read_chroms(path, format.in=c("chemstation.uv")
```

For downstream analyses of chromatographic data, you can also check out my package [chromatographR](https://ethanbass.github.io/chromatographR). For interactive visualization of chromatograms, you can check out my new package [ShinyChromViewer](https://github.com/ethanbass/ShinyChromViewer) (alpha release).
