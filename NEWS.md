# chromConverter 0.1.1

### New features
* Added `read_shimadzu` function for parsing Shimadzu ascii files.
* Added `read_chromeleon` function for parsing Chromeleon HPLC-UV files.
* Added `read_entab` function for calling Entab parsers.
* Added `read_thermoraw` function to convert ThermoRawFiles by calling the ThermoRawFileParser.
* Added `read_mzml` function to extract UV data from mzML files using `mzR`.
* Added `openchrom_parser` to call OpenChrom parsers through the commandline interface.

All of the above functions can also be called from `read_chroms` by setting the `format_in` and `parser` arguments.  

### Changes to arguments in `read_chroms`:
Some arguments in the `read_chroms` function were reformatted for internal consistency across package:  

* Dots were replaced with underscores in all function arguments.
* The argument to specify the format of R objects was changed from `R.format` to `format_out`.
* The argument to specify the format for exported files was changed from `format.out` to `export_format`.

# chromConverter 0.1.0

* Added a `NEWS.md` file to track changes to the package.

