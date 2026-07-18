# chromConverter <a href='https://CRAN.R-project.org/package=chromConverter'><img src='man/figures/logo.png' align="right" height="160" /></a>

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/chromConverter)](https://cran.r-project.org/package=chromConverter)
[![chromConverter status badge](https://ethanbass.r-universe.dev/badges/chromConverter)](https://ethanbass.r-universe.dev/chromConverter)
[![Last commit](https://img.shields.io/github/last-commit/ethanbass/chromConverter)](https://github.com/ethanbass/chromConverter)
<br>
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/chromConverter?color=blue)](https://r-pkg.org/pkg/chromConverter)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/last-month/chromConverter)](https://cran.r-project.org/package=chromConverter)
<br>
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6944342.svg)](https://doi.org/10.5281/zenodo.6944342)
<!-- badges: end -->

##### Table of contents
- [Overview](#overview)
- [Installation](#installation)
- [Quick Start](#quick-start)
- [File formats](#formats)
- [Usage](#usage)
- [Additional dependencies](#optional-dependencies)
- [Further analysis](#further-analysis)
- [Contributing](#contributing)
- [Citation](#citation)

### Overview

chromConverter provides a simple way to read chromatography data into R from a variety of vendor formats. It includes parsers implemented directly in R as well as interfaces to various external tools including [Entab](https://github.com/bovee/entab), [rainbow](https://rainbow-api.readthedocs.io/), the [ThermoRawFileParser](https://github.com/compomics/ThermoRawFileParser), and [RaMS](https://github.com/wkumler/RaMS/).

We aim to support open science and reproducible research by reducing dependence on proprietary vendor software. Since most of the supported file formats are not publicly documented, many of the parsers are developed through reverse-engineering. If you run into a file that doesn't parse correctly (or at all), please [open an issue](https://github.com/ethanbass/chromConverter/issues).

### Installation

chromConverter can be installed from CRAN:

```r
install.packages("chromConverter")
```

However, it's recommended to install the development version, either from GitHub:

```r
if (!require("pak", quietly=TRUE)) install.packages("pak")
pak::pak("ethanbass/chromConverter")
```

or from [R Universe](https://r-universe.dev/):

```r
install.packages("chromConverter", repos="https://ethanbass.r-universe.dev/")
```

Some parsers require additional software. See [Optional Dependencies](#optional-dependencies) for details.

### Quick start

```r
library(chromConverter)

# Read a folder of Agilent ChemStation UV files
dat <- read_chroms("path/to/files", format_in = "chemstation_uv")
```

### Formats

##### ChromConverter (internal parsers)
- 'Agilent ChemStation' & 'OpenLab' `.uv` files (versions 131, 31)
- 'Agilent ChemStation' & 'OpenLab' `.ch` files (versions 30, 130, 8, 81, 179, 181)
- 'Agilent OpenLab' `.dx`, `acaml`, and `amx` files.
- Allotrope® Simple Model (ASM) 2D chromatograms (`.asm`)
- ANDI (Analytical Data Interchange) Chromatography & MS formats (`.cdf`)
- mzML (`.mzml`) & mzXML (.`mzxml`) (via *RaMS*).
- 'Shimadzu LabSolutions' ascii (`.txt`)
- 'Shimadzu GCsolution' data files (`.gcd`)
- 'Shimadzu GCMSsolution' data files (`.qgd`) 
- 'Shimadzu LabSolutions'`.lcd` (PDA, chromatogram, and peak table streams)
- 'Thermo Scientific Chromeleon' ascii (`.txt`)
- 'Varian Workstation' (`.SMS`)
- 'Waters Empower' ascii (`.arw`)
- 'Waters Empower' `.raw` files (2D chromatograms only)
- Chromatotec `.Chrom` files.

##### External Libraries

###### Entab (*Entab requires separate installation, see [instructions below](README.md#Installation)*)
- Agilent ChemStation (`.ch`, `.fid`, `.ms`, .`mwd`, & `.uv`)
- Agilent MassHunter DAD (`.sp`)  

###### ThermoRawFileParser (*requires separate installation, see [instructions below](README.md#Installation)*)
- Thermo RAW (`.raw`)

###### rainbow
- Agilent (`.ch`, `.fid`, `.ms`, .`MSProfile.bin`, & `.uv`)
- Waters (`.raw` [UV, MS, CAD, ELSD])

### Usage

##### Importing chromatograms

The central function of chromConverter is `read_chroms`, which functions as a wrapper around all of the supported parsers. To convert a set of files, call `read_chroms`, specifying the `paths` to a vector of directories or files and the appropriate file format (`format_in`). Supported formats include `chemstation_uv`, `chemstation_csv`, `masshunter_dad`, `shimadzu_fid`, `shimadzu_dad`, `chromeleon_uv`, `thermoraw`, `mzml`, `waters_arw`, `msd`, `csd`, and `wsd`.

```r
library(chromConverter)
dat <- read_chroms(path, format_in = "chemstation_uv")
```

The `read_chroms` function will attempt to determine an appropriate parser to use and whether you've provided a vector of directories or files. However, if you'd like to be more explicit, you can provide arguments to the `parsers` and `find_files` arguments. Setting `find_files = FALSE` will instruct the function that you are providing a vector of files, while `find_files = TRUE` implies that you are providing a vector of directories.

###### Exporting files

If you'd like to automatically export the files, include the desired file format (`export_format`) and the path where you'd like to export the files (`path_out`). Some parsers (e.g. `ThermoRawFileParser`) need to export files for their basic operations. Thus, if these parsers are selected, you will need to specify an argument to `path_out`.

```r
library(chromConverter)
dat <- read_chroms(path, find_files = FALSE, path_out="temp", export=TRUE)
```

###### Choosing between multiple parsers

For formats where multiple parsers are available, you can choose between them using the `parser` argument. For example, 'Agilent' files can now be read using parsers from a number of external libraries, including Entab and rainbow. Some of these parsers must be installed manually as described in the [installation instructions](README.md#Optional-dependencies).

###### Extracting metadata

chromConverter includes some options to extract metadata from the provided files. If `read_metadata = TRUE`, metadata will be extracted and stored as [attributes](https://stat.ethz.ch/R-manual/R-devel/library/base/html/attributes.html) of the associated object. The metadata can then be accessed using the `attributes` or `attr` functions on individual chromatograms, or extracted into a data.frame or tibble from a list of chromatograms using the `extract_metadata` function. 

##### Importing peak lists

The `read_peaklist` function can be used to import peak lists from 'Agilent ChemStation' REPORT files or 'Shimadzu' ascii files. The syntax is similar to `read_chroms`. In the simplest case, you can just provide paths to the files or directory you want to read in along with the format (`format_in`), e.g.

```r
pks <- read_peaklist(<path_to_directory>, format_in = "chemstation")
```

### Optional dependencies

Some of the parsers rely on external software libraries that must be manually installed.

##### **Entab**

[Entab](https://github.com/bovee/entab) is a Rust-based parsing framework for converting a variety of scientific file formats into tabular data. To use parsers from Entab, you must first install Rust and Entab-R. After following the [instructions](https://rust-lang.org/tools/install/) to install Rust, you can install Entab from GitHub as follows:

```r
pak::pak("bovee/entab/entab-r")
```

##### **ThermoRawFileParser**

Thermo RAW files can be converted by calling the [ThermoRawFileParser](https://github.com/compomics/ThermoRawFileParser) on the command-line. To install the ThermoRawFileParser, follow the instructions [here](https://github.com/compomics/ThermoRawFileParser). If you are running Linux or Mac OS X, you will also need to install [mono](https://www.mono-project.com/download/stable/#download-lin), following the instructions provided at the link. In addition, when you use chromConverter to convert Thermo RAW files for the first time you will be asked to enter the path to the program.

##### **OpenChrom** 
###### (**Note:** Support for the command line interface has been removed from OpenChrom (as of `version 1.5.0`). Version 1.4, which is required for command-line use, is no longer available for download. The instructions below are preserved for users who already have it installed.

[OpenChrom](https://lablicate.com/platform/openchrom) is a free chromatography software, containing a large number of file parsers, which can now be conveniently accessed directly from R. Strangely, configuring OpenChrom for use on the command-line deactivates the graphical user interface (GUI). Thus, it is recommended to make a separate copy of OpenChrom if you'd still like to access the GUI. To use the OpenChrom parsers, follow the steps detailed below: 

1) [OpenChrom](https://lablicate.com/platform/openchrom/download) **version ≤ 1.4** is unfortunately no longer available from Lablicate. If you already have version 1.4 and like using the commandline interface make sure you have it backed up!!
2) If you intend to use the GUI in the future, it is recommended to make a separate copy of OpenChrom for command-line use.
3) Call `read_chroms` with `parser = "openchrom"`. The first time you call the parser, you may be asked to provide the path to your local installation of OpenChrom. The path will then be saved for future use. If the command-line interface is disabled, you will be given the option to automatically activate the command-line.  Alternatively, the command-line option can be activated from R by calling `configure_openchrom(cli = "true")` or following the [instructions](https://github.com/OpenChrom/openchrom/wiki/CLI) to manually activate the CLI. This process can be reversed using the same function: e.g. `configure_openchrom(cli = "false"). To specify an OpenChrom executable in a non-standard location, call `configure_openchrom` with the `path` argument, e.g. `configure_openchrom(cli = "true", path = "path_to_openchrom_executable").

### Further analysis

For downstream analyses of chromatographic data, you can also check out my package [chromatographR](https://ethanbass.github.io/chromatographR/). For interactive visualization of chromatograms, you can check out my new package [ShinyChromViewer](https://github.com/ethanbass/ShinyChromViewer) (alpha release). There is also a vignette providing an introduction to some basic syntax for [plotting mass spectrometry data](https://ethanbass.github.io/chromConverter/articles/plot_ms.html) returned by chromConverter in various R dialects (e.g., base R, tidyverse, and data.table).

### Contributing

Contributions of source code, ideas, or documentation are always welcome. Please get in touch (preferable by opening a GitHub [issue](https://github.com/ethanbass/chromConverter/issues)) to discuss any suggestions or to file a bug report. Some good reasons to file an issue:

- You think you've found a bug.  
- You're getting a cryptic error message that you don't understand.  
- You have a file format you'd like to read that isn't currently supported by chromConverter.  (Please make sure to attach example files or a link to the files).  
- There's another new feature you'd like to see implemented.  

**Note: Before filing a bug report, please make sure to install the latest development version of chromConverter from GitHub**, in case your bug has already been patched. After installing the latest version, you may also need to refresh your R session to remove the older version from the cache.

### Citation

You can cite chromConverter as follows:

Bass, E. (2026). chromConverter: Chromatographic File Converter. http://doi.org/10.5281/zenodo.6792521.

If you use external libraries to convert your files, it is suggested to also cite these libraries in published work.
