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

