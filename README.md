# chromConverter <a href='https://cran.r-project.org/web/packages/chromConverter/'><img src='man/figures/logo.png' align="right" height="160" /></a>

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/chromConverter)](https://cran.r-project.org/package=chromConverter)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/chromConverter?color=blue)](https://r-pkg.org/pkg/chromConverter)
<!-- badges: end -->

## Overview

chromConverter aims to facilitate the conversion of chromatography data from various proprietary formats so it can be easily read into R for further analysis.

It currently consists of a wrapper around the file parsers included in the [Aston](https://github.com/bovee/aston) and [Entab](https://github.com/bovee/entab) packages as well as some parsers written natively in R for text-based formats. It is recommended to use the newer Entab parsers, since Aston is no longer actively supported. However, they're slightly more complicated to install (see [installation instructions](README.md#Installation) below).

## Formats
##### Binary formats
- Agilent ChemStation CH, FID, MS, MWD, and UV
- Agilent MassHunter DAD (`.sp`)
- Thermo RAW (see [installation instructions](README.md#Installation) for the ThermoRawFileParser)
##### Text formats
- Chromeleon UV ascii (`.txt`) format
- Shimadzu LabSolutions GC-FID ascii (`.txt`) format
- Waters ascii (`.arw`) format (*provisional support*)

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

### Optional additional dependencies

Some of the parsers rely on software that must be manually installed.

#### Entab

To use parsers from Entab, you must first install [Rust](https://www.rust-lang.org/tools/install) and entab. After follow the [instructions](https://www.rust-lang.org/tools/install) to install Rust, you can install Entab from GitHub as follows:

```
devtools::install_github("https://github.com/bovee/entab/", subdir = "entab-r")
```

#### ThermoRawFileParser

To read Thermo RAW files you must first install the [ThermoRawFileParser](https://github.com/compomics/ThermoRawFileParser). If you are running Linux or Mac OS X, you will first need to install mono, following the instructions provided at the link. When you use the chromConverter to convert Thermo RAW files for the first time you will be asked to enter the path to the program.

## Usage

Use the `read_chroms` function to convert files by specifying the path to a directory (or a vector of directories) containing your files and the appropriate file format. The current options are `chemstation_uv`, `masshunter_dad`, `shimadzu_fid`, `chromeleon_uv`, `thermoraw`, or `mzml`. For formats where there are multiple parsers available, you can choose between them using the `parser` argument. For example, Chemstation and Masshunter files can be parsed using either the `aston` or `entab` parsers.

```
library(chromConverter)
dat <- read_chroms(path, find_files = TRUE, format.in = "chemstation_uv")
```

If you want to provide direct paths to files, include the argument `find_files = FALSE`.

```
library(chromConverter)
dat <- read_chroms(path, find_files = FALSE, format.in = "chemstation_uv")
```

## Further analysis

For downstream analyses of chromatographic data, you can also check out my package [chromatographR](https://ethanbass.github.io/chromatographR). For interactive visualization of chromatograms, you can check out my new package [ShinyChromViewer](https://github.com/ethanbass/ShinyChromViewer) (alpha release).
