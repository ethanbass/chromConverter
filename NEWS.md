## chromConverter 0.5.0

### New features

* Added support for parallel processing through `pbapply` package. (The `pbapply` 
package must be manually installed to enable parallel processing). 
* Added preliminary support for reading 'Agilent' (`.dx`) files through
`read_agilentdx` function.
* Added support for reading 'Chemstation' REPORT files.
* Added `read_peaklist` function for reading peak lists. Currently 'Agilent Chemstation' and
'Shimadzu ASCII' formats are supported.

### Other Improvements

* Improved automatic format detection by `read_chroms`.
* Refactored `read_thermoraw` function to simplify paths.
* The `read_thermoraw` function now uses temp directory (without prompting) if an export directory is not specified through the `path_out` argument.
* Added additional tests, attaining 74% test coverage.

### Bug fixes

* Corrected 'Shimadzu' DAD parser so it uses wavelengths written to file instead of inferring them.
* Fixed bug causing failure of 'Shimadzu' ascii parser (when `what == "peak_table"` and `read_metadata == TRUE`).
* Fixed bug causing 'MDF' files to export as data.frames when `format_out == "matrix"`.
* Fixed misleading `data_format` attributes in 'Waters ARW' and 'Chromeleon' parsers.

## chromConverter 0.4.3

* Fixed bug in `chemstation_ch` parser (version 130) ([#17](https://github.com/ethanbass/chromConverter/issues/17))

## chromConverter 0.4.2

Added support for parsing "Waters" ascii (`.arw`) PDA files.

## chromConverter 0.4.1

### New features 

* Added support for "Chemstation" UV (`.ch`) files (version 30).

### Minor improvements

* Updated `read_chromeleon` to better deal with comma decimal separators in metadata.
* Updated `read_chromeleon` to deal with more datetime formats.
* Updated `read_chromeleon` to deal with unicode microliters.
* Added tests for rainbow parser and `read_chemstation_ch`.

### Bug fixes

* Fixed bug preventing compilation of PDF manual.
* Fixed new bug causing failure to correctly read names of chemstation files from .D directory.

## chromConverter 0.4.0

### New features

* Added parser for ANDI chrom `cdf` files through the `read_cdf` function.
* Added parser for 'Lumex' `.mdf` files through the `read_mdf` function.
* Added additional options for file exports. New options for writing
`chemstation_csv` (utf-16) and ANDI chrom `cdf` files through `read_chroms`.
* Added preliminary support for automatic filetype detection by `read_chroms` when providing direct paths to files (i.e. when `find_files == FALSE`).
* Added `read_varian_peaklist` function for reading peak lists from 'Varian MS Workstation'.

### Other improvements and bug fixes:

* Added `wide` and `long` `data_format` options for 2D data, such that the `wide` format option writes retention times as rownames of the matrix or data.frame. while the `long` format writes retention times as the first column of the object.
* Updated `configure_openchrom` for better discovery of 'OpenChrom' path and added `path` argument for directly specifying the path to 'OpenChrom'.
* Slightly restructured metadata fields. Added `source_file` field to track
data origin.
* Standardized datetime stamps so they are always converted to POSIXct format.
* Now use `fs` package for parsing paths, eliminating buggy `check_paths` function.
* Fixed bug causing sloppy 'Chemstation' FID metadata.
* Fixed bug that caused padding of 'Chemstation 130' files with extra zeros.
* Added additional tests.

## chromConverter 0.3.3

* Added R-based parser for "Chemstation" UV (`.uv`) files (version 131) through
the `read_chemstation_uv` function.
* Added `extract_metadata` function for extracting metadata from a list of chromatograms
and returning it as a `data.frame` or `tibble`.
* Added `progress_bar` option in `read_chroms`.
* Updated `reshape_chroms` and `reshape_chrom` to allow switching between "wide" and "long" formats.
* Added wide format option in `read_mzml`.
* Added automatic detection of file formats by `read_chroms`.
* Minor changes to storage of metadata in attributes for the purpose of simplification.
* Fixed bug preventing removal of file extensions for 'Agilent' data when using `read_chroms`.
* Standardized run date/time in metadata to `POSIXct` format.
* Minor updates to documentation.

## chromConverter 0.3.2

* Fix 'Shimadzu' ascii parser so it can cope with variable entries in PDA header.

## chromConverter 0.3.1

* Added support for "Chemstation" UV (`.ch`) files (version 130).
* Added provisional support for "Chemstation" FID (version 8).
* Changed name of `read_chemstation_fid` function to `read_chemstation_ch`.
* Ignore case when matching file extensions in `read_chroms`.
* Added note to README about configuring RStudio correctly for accessing python parsers.

## chromConverter 0.3.0

* Fixed bug causing "Chromeleon" metadata parser to fail.
* Fixed bug in "Chemstation" metadata parser.
* Changed `format_data` argument to `data_format` to select wide or long format.
* Added support for parsing `mzML` files with `RaMS`.
* Added support for parsing "Agilent" (`.D`) and "Waters" (`.raw`) files with [rainbow](https://rainbow-api.readthedocs.io/).
* Made `data_format` option available consistently for choosing `wide` or `long` format.
* Added parser in R for "Chemstation" FID (`.ch`) data (versions 81, 179 & 181).
* Improved error handling when loading python modules.
* Improved error-handling for parsing metadata so small problems no longer error out the whole program.

#### Shimadzu ascii parser

* Fixed bug in 'Shimadzu' ascii parser that was cutting chromatograms short.
* Added automatic detection of decimal separator for reading European-style files.
* Generalized algorithm to acquire 'Shimadzu' DAD metadata.

## chromConverter 0.2.2

* Fixed bug in `call_openchrom` so that it can actually find 'OpenChrom' path.
* Fixed bug in `call_openchrom` to allow 'animl' as valid `export_format`.
* Allow 'Thermo RAW' files to be parsed using 'Entab'.

## chromConverter 0.2.1

* Added `configure_aston` function for configuration of Aston parsers and fixed issues with configuration of Aston.

## chromConverter 0.2.0

#### New features and formats

* Added `read_shimadzu` function for parsing 'Shimadzu' ascii files.
* Added `read_chromeleon` function for parsing 'Chromeleon' ascii files.
* Added `read_thermoraw` function to convert 'Thermo Raw' files by calling the 'ThermoRawFileParser'.
* Added `read_mzml` function to extract UV data from mzML files using mzR.
* Added `call_entab` function for calling Entab parsers.
* Added `call_openchrom` to call OpenChrom parsers through the command-line interface.

(All of the new functions described above can be called from the `read_chroms` function by setting the `format_in` and `parser` arguments).

* New option to read and attach instrumental metadata to chromatograms by setting `read_metadata = TRUE` in `read_chroms`.

#### Simplification of `read_chroms` syntax
* `read_chroms` will now automatically assign a parser if the parser isn't specified.
* `read_chroms` will throw more informative errors for mismatch between `format_in` and `parser` arguments.
* `read_chroms` will try to automatically determine if files or directories are being provided.

#### Other changes to `read_chroms` syntax:
Dots were replaced with underscores in all arguments to `read_chroms` for internal syntactical consistency across the package. Thus:

* The argument to specify the format of R objects was changed from `R.format` to `format_out`.
* The argument to specify the format for exported files was changed from `format.out` to `export_format`.

## chromConverter 0.1.0

* Added a `NEWS.md` file to track changes to the package.

