# chromConverter 0.1.1

### New features and formats

* Added `read_shimadzu` function for parsing Shimadzu ascii files.
* Added `read_chromeleon` function for parsing Chromeleon ascii files.
* Added `read_thermoraw` function to convert ThermoRawFiles by calling the ThermoRawFileParser.
* Added `read_mzml` function to extract UV data from mzML files using mzR.
* Added `call_entab` function for calling Entab parsers.
* Added `call_openchrom` to call OpenChrom parsers through the command-line interface.
* Added an option to read and attach instrumental metadata to chromatograms by setting `read_metadata = TRUE` in `read_chroms`.

(All of the new functions described above can be called from the `read_chroms` function by setting the `format_in` and `parser` arguments).


### Updated syntax
##### Changes to arguments in `read_chroms`:
Some arguments in the `read_chroms` function were reformatted for internal syntactical consistency across the package. (Dots were replaced with underscores in all function arguments).

* The argument to specify the format of R objects was changed from `R.format` to `format_out`.
* The argument to specify the format for exported files was changed from `format.out` to `export_format`.

##### New function names

* `uv_converter` was renamed to `aston_uv_converter`
* `sp_converter` was renamed to `aston_sp_converter`

### Other changes
* Added error message for mismatch between `format_in` and `parser`.
* `read_chroms` will now automatically assign a parser if the parser isn't specified. 

# chromConverter 0.1.0

* Added a `NEWS.md` file to track changes to the package.

