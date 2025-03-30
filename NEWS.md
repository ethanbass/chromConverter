## chromConverter 0.7.5

* **Changes to `sample_names` argument in `read_chroms`:** This argument can no longer be supplied with a vector of names. Instead, `sample_names` can be provided with one of two arguments: `basename` or `sample_name`. The default setting (`basename`) will use the basename of the file, while `sample_name` will instead use the sample name encoded in the file's metadata.
* Fixed bug in `extract_metadata` when sub-setting metadata elements.
* Added more informative error and warning messages to `extract_metadata`.
* Added `data.table` format option to `extract_metadata`.
* Fixed path issue in `call_openchrom`.
* Updated documentation in README and `call_openchrom` manual concerning OpenChrom installation.

## chromConverter 0.7.4

* Use updated syntax for reticulate (hopefully this will solve some of the issues with python configuration failure).
* Invisibly return paths to exported CDF files.
* Small updates to documentation (e.g., addition of functional families, standardization of function titles and descriptions).
* Fixed Windows path issue when R is installed locally.
* Fixed error reading Shimadzu metadata on Windows (due to "Extra content at end of document").

## chromConverter 0.7.3

* Updated `read_shimadzu_lcd` to infer retention times in Shimadzu 3D Data from `Max Plot` stream since it is always (?) present.
* Updated `read_shimadzu_lcd` to skip parsing of metadata from 3D Data Item when it is not present.
* Updated `read_shimadzu_lcd` to include `Max Plot` stream when parsing 2D chromatograms.
* Fixed bug in `read_chromeleon` related to inference of decimal separators.
* Added `decimal_mark` argument to `read_chromeleon` to manually set decimal separator.

## chromConverter 0.7.2

* Added preliminary support for extraction of peak tables from 'Shimadzu' `.lcd` files.
* Added support for inference of retention times from 'Shimadzu' `.lcd` files lacking `Data Item` streams.
* Added support for raw format File Properties stream in 'Shimadzu' `.lcd` files.
* Added support for parsing 3D data field from 'Chromeleon' ascii files.

## chromConverter 0.7.1

* Fixed automatic file detection for directories (e.g., Waters `.raw` and Agilent `.D`)
* Fixed bug preventing extraction of `Waters` chromatograms with lowercase filenames.
* Added support for extracting metadata from 'Waters' `.raw` header files.
* Added support for extraction of detector units from 'Waters' chromatograms.

## chromConverter 0.7.0

### Major features

* Added preliminary support for 'Varian Worktation' (`.sms`) format through `read_varian_sms` function.
* Added preliminary support for 'Shimadzu QGD' GC-MS files through the `read_shimadzu_qgd` function.
* Added preliminary support for 'Allotrope Simple Model' (ASM) 2D chromatography date files.
* Added support for reading multiple files from 'Agilent' `.D` directories through `read_agilent_d` function.
* Added internal parser for 'Agilent ChemStation' MS files through `read_agilent_ms`.
* Added option to write mzML files (MS1 and DAD).
* Added option to write ANDI MS netCDF files.

### UI changes

* Changed order of arguments in `read_chroms` so that `format_in` comes second after `path`.
* Removed extraneous `export` argument from `read_chroms`. To export files, you now only need to provide an argument to `export_format`.
* Updated handling of multiple chromatograms by `read_shimadzu_lcd`. The function now returns a list of named chromatograms if `data_format == "wide"` and returns multiple chromatograms in a single `data.frame` if `data_format == "long"`.
* Added `scale` argument to `read_chemstation_uv` and `read_shimadzu_ascii` to toggle scaling of chromatograms.
* Harmonized file path arguments across parser functions by changing `file` arguments to `path`.
* Harmonized column names in output across parsers.
* Small changes in `read_cdf` UI: `what` now defaults to `NULL` and defaults are coded into downstream `read_andi_chrom` and `read_andi_ms` functions.
* Added extra verbosity in `read_chroms` when `verbose` is `TRUE`.

### Other improvements

* Fixed bug causing reticulate to attempt reinstallation of Aston every time the package is loaded (due to case sensitivity of packages names in `reticulate::configure_environment`).
* Added `data.table` as an option for `format_out`.
* Improved speed of `read_shimadzu_lcd` by dealing with twos-complements more sensibly.
* Start 'Shimadzu LCD' chromatogram retention times at dwell time (DLT).
* Give temp files generated from Shimadzu OLE files informative names.
* Use 'Output Date' field instead of 'Type' to find 'Shimadzu' ASCII delimiter. (This seems to be a more generalizable solution since some files do not contain the 'Type' field).
* Allow relative paths for `path_out` when using 'ThermoRawFileParser' and 'OpenChrom' parsers.
* Allow creation of new directories by `read_chroms` if `path_out` does not exist.
* Fixed bug affecting some `mdf` files lacking null bytes after the file header.
* Eliminated 'magrittr' dependency by using xpath to parse XML in a more straightforward fashion.
* Fixed bug causing truncation of sample names at the first period by `read_chroms`.
* Modified `export_csv` function to label first column for wide-format chromatograms.
* Improved handling of metadata from rainbow parsers.
* Fixed error when providing single chromatogram to \code{extract_metadata}.
* Added metadata field for source checksum (SHA1) and source file format.
* Other minor changes to metadata fields.
* Return all times in Coordinated Univeral Time (UTC) for consistency across systems.

## chromConverter 0.6.4

* Added support for 'Agilent ChemStation' version 8 (`.ch`) files through `read_chemstation_ch`.
* Fixed failure to return units in some `Agilent Chemstation` files due to typo.
* Fixed bug causing "spill-over" of Agilent metadata fields.
* Fixed bug causing failure to read `ChemStation` CSV files on (some) mac machines by specifying little-endian format in call to `read.csv`.

## chromConverter 0.6.3

* Added parser for total ion chromatogram (TIC) stream in 'Shimadzu' LCD files.
* Added additional support for extraction of metadata from 'Shimadzu' LCD and GCD files.
* Updated docs for `read_shimadzu_lcd_2d` to more accurately reflect file structure.
* Added `scale` argument to `read_chemstation_ch` and `read_shimadzu_lcd` to 
toggle scaling of chromatograms resolving ([#30](https://github.com/ethanbass/chromConverter/issues/30)).

## chromConverter 0.6.2

* Updated `read_shimadzu_lcd` function to correctly determine the number of blocks in the "Shimadzu" LCD PDA stream (thanks to [kco-hereon](https://github.com/kco-hereon)).
* Added preliminary support for 2D data streams from "Shimadzu LCD" files.
* Added parser for 'Shimadzu GCD' files (from GC-FID).

## chromConverter 0.6.1

* Added support for 'Shimadzu' ASCII files with '[LC Chromatogram...]' sub-header.
* Correct 'Shimadzu' ASCII chromatograms by 'Intensity Multiplier' if it is provided.
* Fixed bug in logic in `export_cdfs` function to permit conversion of files lacking metadata.
* Minor, cosmetic changes to documentation.

## chromConverter 0.6.0

* Added parser for reading ANDI MS (`.cdf`) files.
* Fixed parsing of Agilent MS files with 'entab' reader.
* Fixed `read_chemstation_ch` parser to correctly read "Mustang Chemstation" 179 files with 8-byte encoding.
* Re-factored `read_shimadzu` function and added support for new types of chromatograms (e.g. status, uv and total ion chromatograms). Added support for reading multiple types of chromatograms at once.
* Added support for reading MS spectra from 'Shimadzu' ascii files using `read_shimadzu`.
* Exported `write_cdf` and added additional arguments (`lambda` and `force`) for greater control by users.
* Added internal parser for 1D 'Waters RAW' chromatograms (`read_waters_raw`).
* Added `collapse` argument to `call_rainbow` and  to collapse superfluous lists. 
* Added `...` argument to `read_chroms` for supplying additional arguments to parsers.
* Added alias to `read_chroms` for reading `mzxml` files with `RaMS`.
* Added `precision` argument to `call_rainbow` to control number of digits "mz" values are rounded to. (Also changed default behavior so values are rounded to one decimal by default).
* Fixed bug in `read_shimadzu_lcd` on Windows due to issue with passing escaped paths to Python.
* Updated documentation of various functions.

## chromConverter 0.5.0

### New features

* Added support for parallel processing through `pbapply` package. (**Note**: The `pbapply` package must be manually installed to enable parallel processing). 
* Added internal parser for 'Agilent ChemStation' version 31 files (through `read_chemstation_uv` function).
* Added support for 'Agilent OpenLab' version 131 files through internal parser. 
* Added preliminary support for reading 'Agilent' (`.dx`) files (through `read_agilentdx` function).
* Added support for reading 'ChemStation' REPORT files.
* Added parser for Shimadzu `.lcd` files through the `read_shimadzu_lcd` function. Only the PDA stream (not MS) is currently supported.
* Added `read_peaklist` function for reading peak lists. Currently 'Agilent ChemStation' and 'Shimadzu ASCII' formats are supported.
* Added `verbose` argument to control console output for external parsers ('OpenChrom' and 'ThermoRawFileParser').

### Other Improvements

* Improved automatic file type detection by `read_chroms`.
* Refactored `read_thermoraw` function to simplify paths.
* The `thermoraw` and `openchrom` parsers now use a proper temp directory if an export directory is not specified through the `path_out` argument.
* Re-factored `reshape_chroms`, speeding up conversion from wide to long format.
* Added additional tests, attaining 82% test coverage.
* Changed default `openchrom` export format to `mzml`.
* Minor changes to some metadata fields to better standardize results across different file formats and parsers.

### Bug fixes

* Corrected 'Shimadzu' DAD parser so it reads wavelengths from the file instead of inferring them.
* Fixed bug causing failure of 'Shimadzu' ascii parser (when `what == "peak_table"` and `read_metadata == TRUE`).
* Fixed bug causing 'MDF' files to export as data.frames when `format_out == "matrix"`.
* Fixed misleading `data_format` attributes in 'Waters ARW' and 'Chromeleon' parsers.

## chromConverter 0.4.3

* Fixed bug in `chemstation_ch` parser (version 130) ([#17](https://github.com/ethanbass/chromConverter/issues/17))

## chromConverter 0.4.2

* Added support for parsing "Waters" ascii (`.arw`) PDA files.

## chromConverter 0.4.1

### New features 

* Added support for "ChemStation" UV (`.ch`) files (version 30).

### Minor improvements

* Updated `read_chromeleon` to better deal with comma decimal separators in metadata.
* Updated `read_chromeleon` to deal with more datetime formats.
* Updated `read_chromeleon` to deal with unicode microliters.
* Added tests for rainbow parser and `read_chemstation_ch`.

### Bug fixes

* Fixed bug preventing compilation of PDF manual.
* Fixed new bug causing failure to correctly read names of 'ChemStation' files from .D directory.

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
* Fixed bug causing sloppy 'ChemStation' FID metadata.
* Fixed bug that caused padding of 'ChemStation 130' files with extra zeros.
* Added additional tests.

## chromConverter 0.3.3

* Added R-based parser for "ChemStation" UV (`.uv`) files (version 131) through
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

* Added support for "ChemStation" UV (`.ch`) files (version 130).
* Added provisional support for "ChemStation" FID (version 8).
* Changed name of `read_chemstation_fid` function to `read_chemstation_ch`.
* Ignore case when matching file extensions in `read_chroms`.
* Added note to README about configuring RStudio correctly for accessing python parsers.

## chromConverter 0.3.0

* Fixed bug causing "Chromeleon" metadata parser to fail.
* Fixed bug in "ChemStation" metadata parser.
* Changed `format_data` argument to `data_format` to select wide or long format.
* Added support for parsing `mzML` files with `RaMS`.
* Added support for parsing "Agilent" (`.D`) and "Waters" (`.raw`) files with [rainbow](https://rainbow-api.readthedocs.io/).
* Made `data_format` option available consistently for choosing `wide` or `long` format.
* Added parser in R for "ChemStation" FID (`.ch`) data (versions 81, 179 & 181).
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

