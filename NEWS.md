## chromConverter 0.2.1

* Added `configure_aston` function for configuration of Aston parsers and fixed issues with configuration of Aston.

## chromConverter 0.2.0

#### New features and formats

* Added `read_shimadzu` function for parsing Shimadzu ascii files.
* Added `read_chromeleon` function for parsing Chromeleon ascii files.
* Added `read_thermoraw` function to convert ThermoRawFiles by calling the ThermoRawFileParser.
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

