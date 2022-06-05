# chromConverter <a href='https://cran.r-project.org/web/packages/chromConverter/'><img src='man/figures/logo.png' align="right" height="160" /></a>

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/chromConverter)](https://cran.r-project.org/package=chromConverter)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/chromConverter?color=blue)](https://r-pkg.org/pkg/chromConverter)
<!-- badges: end -->

## Overview

chromConverter aims to facilitate the conversion of chromatography data from various proprietary formats so it can be easily read into R for further analysis.

It currently consists of a wrapper around the file parsers included in the [Aston](https://github.com/bovee/aston) and [Entab](https://github.com/bovee/entab) packages as well as some parsers written natively in R for text-based formats. It is recommended to use the newer Entab parsers, since Aston is no longer actively supported. However, they require a little more work to install (see [Installation](README.md#Installation) section below).

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

Alternatively, the development version of chromConverter is hosted on GitHub. You can install
it using `devtools` as follows:

```
install.packages("devtools")
devtools::install_github("https://github.com/ethanbass/chromConverter/")
```

Some of the parsers rely on software that must be manually installed.

#### Entab

To use parsers from Entab, you must manually install [Rust](https://www.rust-lang.org/tools/install) and entab. After follow the [instructions](https://www.rust-lang.org/tools/install) to install Rust, you can install Entab from GitHub as follows:

```
devtools::install_github("https://github.com/bovee/entab/")
```

## Usage

Convert files by specifying the path to a directory (or a vector of directories) and the appropriate file format.

```
library(chromConverter)
dat <- read_chroms(path, format.in=c("chemstation.uv")
```

## Further analysis

For downstream analyses of chromatographic data, you can also check out my package [chromatographR](https://ethanbass.github.io/chromatographR). For interactive visualization of chromatograms, you can check out my new package [ShinyChromViewer](https://github.com/ethanbass/ShinyChromViewer) (alpha release).
