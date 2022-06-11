# chromConverter <a href='https://cran.r-project.org/web/packages/chromConverter/'><img src='man/figures/logo.png' align="right" height="160" /></a>

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/chromConverter)](https://cran.r-project.org/package=chromConverter)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/chromConverter?color=blue)](https://r-pkg.org/pkg/chromConverter)
<!-- badges: end -->

## Overview

chromConverter aims to facilitate the conversion of chromatography data from various proprietary formats so it can be easily read into R for further analysis. It currently consists of wrappers around file parsers from various external libraries including [Aston](https://github.com/bovee/aston), [Entab](https://github.com/bovee/entab), and the [ThermoRawFileParser](https://github.com/compomics/ThermoRawFileParser) as well as some parsers written natively in R for text-based formats. For Agilent files, it is recommended to use the newer Entab parsers, since Aston is no longer actively supported. However, Entab is slightly more complicated to install (see [installation instructions](README.md#Installation) below).

## Formats
##### Binary formats
###### Aston/Entab
- Agilent ChemStation CH, FID, MS, MWD, and UV
- Agilent MassHunter DAD (`.sp`)

###### ThermoRawFileParser (*requires separate installation, see [instructions](README.md#Installation)*)
- Thermo RAW (`.raw`)

###### OpenChrom (*requires separate installation, see [instructions](README.md#Installation)*)
- Shimadzu FID (`.gcd`, `.C0#`)
- Varian FID (`.run`)
- DataApex FID (`.PRM`)
- MassFinder FID/MSD (`*.mfg`)
- and many more (see full list [here](https://lablicate.com/platform/openchrom)).

##### Text formats
- Chromeleon UV ascii (`.txt`)
- mzML (`.mzml`)
- Shimadzu LabSolutions ascii (`.txt`)
- Waters ascii (`.arw`) (*provisional support*)

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

#### Optional additional dependencies

Some of the parsers rely on software that must be manually installed.

##### Entab

To use parsers from Entab, you must first install [Rust](https://www.rust-lang.org/tools/install) and Entab-R. After following the [instructions](https://www.rust-lang.org/tools/install) to install Rust, you can install Entab from GitHub as follows:

```
devtools::install_github("https://github.com/bovee/entab/", subdir = "entab-r")
```

##### ThermoRawFileParser

The Thermo RAW parser works by calling the ThermoRawFileParser on the command line. Thus, to parse Thermo RAW files you must first install the [ThermoRawFileParser](https://github.com/compomics/ThermoRawFileParser). If you are running Linux or Mac OS X, you will also need to install mono, following the instructions provided at the link. When you use chromConverter to convert Thermo RAW files for the first time you will be asked to enter the path to the program.

##### OpenChrom

OpenChrom contains a large number of [file parsers](https://lablicate.com/platform/openchrom) which can now be conveniently accessed directly from R. Strangely, configuring OpenChrom for use on the command-line deactivates the graphical user interface (GUI). Thus, it is recommended to make a separate copy of OpenChrom, if you'd still like to have access to the GUI. Steps for configuring OpenChrom:  

1) Download OpenChrom from the [website](https://lablicate.com/platform/openchrom/download) and place it into a folder of your choice.  
  2) If you wish to use the GUI, make a separate copy of OpenChrom. Otherwise, you can activate the command-line interface on your main OpenChrom installation.  
  3) Follow the [instructions](https://github.com/OpenChrom/openchrom/wiki/CLI) here to activate the command-line interface.   Alternatively, the command-line option can be activated from R by calling `configure_openchrom_parser(cli="true")`.
  4) Call the `openchrom_parser` from R or `read_chroms` with `parser="openchrom"`. The first time you call the parser, it will ask you to provide a path to your installation of OpenChrom. The path will then be saved for future use. If the command-line interface is disabled, it will also give you the option to automatically activate the command-line.  

## Usage

The workhorse of chromConverter is the `read_chroms` function, which functions as a wrapper around all of the supported parsers. To convert files, call `read_chroms`, specifying the `paths` to a directory (or a vector of directories) containing the files you wish to convert and the appropriate file format (`format.in`). The supported formats include `chemstation_uv`, `masshunter_dad`, `shimadzu_fid`, `chromeleon_uv`, `thermoraw`, `mzml`, and `waters_arw`. For formats where there are multiple parsers available, you can choose between them using the `parser` argument. For example, Chemstation and Masshunter files can be parsed using either the Aston or Entab parsers.

```
library(chromConverter)
dat <- read_chroms(path, find_files = TRUE, format.in = "chemstation_uv")
```

If you want to provide direct paths to files, instead of a folder, include the argument `find_files = FALSE`.

```
library(chromConverter)
dat <- read_chroms(path, find_files = FALSE, format.in = "chemstation_uv")
```

## Further analysis

For downstream analyses of chromatographic data, you can also check out my package [chromatographR](https://ethanbass.github.io/chromatographR). For interactive visualization of chromatograms, you can check out my new package [ShinyChromViewer](https://github.com/ethanbass/ShinyChromViewer) (alpha release).
