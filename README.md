# chromConverter <a href='https://CRAN.R-project.org/package=chromConverter'><img src='man/figures/logo.png' align="right" height="160" /></a>

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/chromConverter)](https://cran.r-project.org/package=chromConverter)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/chromConverter?color=blue)](https://r-pkg.org/pkg/chromConverter)
<!-- badges: end -->

### Overview

chromConverter aims to facilitate the conversion of chromatography data from various proprietary formats so it can be easily read into R for further analysis. It currently consists of wrappers around file parsers from various external libraries including [Aston](https://github.com/bovee/aston), [Entab](https://github.com/bovee/entab), the [ThermoRawFileParser](https://github.com/compomics/ThermoRawFileParser), and [OpenChrom](https://lablicate.com/platform/openchrom) as well as some parsers written natively in R for various text-based formats.

### Formats
##### External Libraries
###### Aston/Entab (*Entab requires separate installation, see [instructions below](README.md#Installation)*)
- Agilent ChemStation CH, FID, MS, MWD, and UV
- Agilent MassHunter DAD (`.sp`)  

###### ThermoRawFileParser (*requires separate installation, see [instructions below](README.md#Installation)*)
- Thermo RAW (`.raw`)

###### OpenChrom (*requires separate installation, see [instructions below](README.md#Installation)*)
- Shimadzu FID (`.gcd`, `.C0#`)
- PerkinElmer FID (`.raw`)
- Varian FID (`.run`)
- DataApex FID (`.PRM`)
- MassFinder FID/MSD (`*.mfg`)
- ABSciex DAD (`.wiff`)
- and many more (see full list [here](https://lablicate.com/platform/openchrom)).

##### Text formats
- Chromeleon UV ascii (`.txt`)
- mzML (`.mzml`)
- Shimadzu LabSolutions ascii (`.txt`)
- Waters ascii (`.arw`) (*provisional support*)

### Installation

chromConverter can now be installed directly from CRAN:

```
install.packages("chromConverter")
```

Alternatively, the development version of chromConverter can be installed from GitHub as follows:

```
install.packages("devtools")
devtools::install_github("https://github.com/ethanbass/chromConverter/")
```

#### Optional additional dependencies

Some of the parsers rely on external software libraries that must be manually installed.

##### **Aston**

To install Aston, call the `configure_aston()` function to install miniconda along with the necessary python dependencies. Running `read_chroms` with the Aston parser selected should also trigger a prompt to install Aston. If you're running Windows, you may need to install the latest version of ['Microsoft Visual C++'](https://docs.microsoft.com/en-US/cpp/windows/latest-supported-vc-redist?view=msvc-170) if you don't already have it.

##### **Entab**

[Entab](https://github.com/bovee/entab) is a Rust-based parsing framework for converting a variety of scientific file formats into tabular data. To use parsers from Entab, you must first install [Rust](https://www.rust-lang.org/tools/install) and Entab-R. After following the [instructions](https://www.rust-lang.org/tools/install) to install Rust, you can install Entab from GitHub as follows:

```
devtools::install_github("https://github.com/bovee/entab/", subdir = "entab-r")
```

##### **ThermoRawFileParser**

Thermo RAW files can be converted by calling the [ThermoRawFileParser](https://github.com/compomics/ThermoRawFileParser) on the command-line. To install the ThermoRawFileParser, follow the instructions [here](https://github.com/compomics/ThermoRawFileParser). If you are running Linux or Mac OS X, you will also need to install mono, following the instructions provided at the link. In addition, when you use chromConverter to convert Thermo RAW files for the first time you will be asked to enter the path to the program.

##### **OpenChrom**

[OpenChrom](https://lablicate.com/platform/openchrom) is opensource chromatography software, containing a large number of [file parsers](https://lablicate.com/platform/openchrom), which can now be conveniently accessed directly from R. Strangely, configuring OpenChrom for use on the command-line deactivates the graphical user interface (GUI). Thus, it is recommended to make a separate copy of OpenChrom if you'd still like to access the GUI. To use the OpenChrom parsers, follow the steps detailed below: 

1) Download OpenChrom from the [website](https://lablicate.com/platform/openchrom/download) and place it into a directory of your choice.  
2) If you intend to use the GUI in the future, it is recommended to make a separate copy of OpenChrom for command-line use.
3) Follow the [instructions](https://github.com/OpenChrom/openchrom/wiki/CLI) to activate OpenChrom's command-line interface. Alternatively, the command-line option can be activated from R by calling `configure_openchrom_parser(cli="true")` or by calling the openchrom_parser and following the prompts.
4) Call `read_chroms` with `parser="openchrom"`. The first time you call the parser, it will ask you to provide the path to your local installation of OpenChrom. The path will then be saved for future use. If the command-line interface is disabled, you will be given the option to automatically activate the command-line.  

### Usage

##### `read_chromes` function

The workhorse of chromConverter is the `read_chroms` function, which functions as a wrapper around all of the supported parsers. To convert files, call `read_chroms`, specifying the `paths` to a vector of directories or files and the appropriate file format (`format_in`). The supported formats include `chemstation_uv`, `chemstation_csv`, `masshunter_dad`, `shimadzu_fid`, `shimadzu_dad`, `chromeleon_uv`, `thermoraw`, `mzml`, `waters_arw`, `msd`, `csd`, and `wsd`.

```
library(chromConverter)
dat <- read_chroms(path, format.in = "chemstation_uv")
```

The `read_chroms` function will attempt to determine an appropriate parser to use and whether you've provided a vector of directories or files. However, if you'd like to be more explicit, you can provide input to the `parsers` and `find_files` arguments. Setting `find_files = FALSE` will instruct the function that you are providing a vector of files, while `find_files = TRUE` implies that you are providing a vector of directories.

###### Exporting files

If you'd like to automatically export the files, include the argument `export=TRUE` along with the path where you'd like to export the files (`path_out`). Some parsers (e.g. `OpenChrom` and `ThermoRawFileParser`) need to export files for their basic operations. Thus, if these parsers are selected, you will need to specify an argument to `path_out`.

```
library(chromConverter)
dat <- read_chroms(path, find_files = FALSE, path_out="temp", export=TRUE)
```

###### Choosing between multiple parsers

For formats where multiple parsers are available, you can choose between them using the `parser` argument. For example, Agilent files can be read using either the Aston or Entab parsers (or in some cases OpenChrom). In this case, it is recommended to use the newer Entab parsers, since Aston is no longer actively supported. However Entab is slightly more complicated to install (see [installation instructions](README.md#Installation) above).

###### OpenChrom parsers

Parsers in OpenChrom are organized by detector-type. Thus, for the `format_in` argument, the user must specify whether the files come from a mass selective detector (`msd`), a current-selective detector like a flame-ionization detector (`csd`), or a wavelength-selective detector (`wsd`), rather than providing a specific file format. In addition, the user should specify what format they'd like to export (`export_format`). Current options include `csv`, `cdf`, `mzml`, or `animl` (the analytical information markup language). The files will then be converted by calling OpenChrom through the command-line interface. If the files are exported in `csv` format, the chromatograms will be automatically read into R. Otherwise, files will be exported to the specified folder but will not be read into the R workspace.

###### Extracting metadata

chromConverter includes some options to extract metadata from the provided files. If `read_metadata=TRUE`, metadata will be extracted and stored as `attributes` of the associated object. A list of [`attributes`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/attributes.html) can be extracted from any R object using the `attributes()` function.

### Further analysis

For downstream analyses of chromatographic data, you can also check out my package [chromatographR](https://ethanbass.github.io/chromatographR/). For interactive visualization of chromatograms, you can check out my new package [ShinyChromViewer](https://github.com/ethanbass/ShinyChromViewer) (alpha release).
