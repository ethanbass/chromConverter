## chromConverter 0.10.0

### New features

* Added `read_agilent_rslt` function to read whole sequence of files from OpenLab and automatically attach corresponding metadata from the `acaml` file.
* Added `sort_by` argument to `read_chroms` to control chromatogram order. Options are "none" (default), "acquisition_time" (using `run_datetime` from metadata), and "file_time" (using file modification time). The default will change to "acquisition_time" in a future release.
* Added a `detector` argument to `extract_metadata` to select which detectors to include (e.g. `detector = "UV"` or `detector = c("UV", "MS")`), matched case-insensitively against each chromatogram's `detector` attribute. This is useful for lists containing more than one detector per sample, such as those returned by the `rainbow` parser.
* `write_andi_ms` is now exported, like `write_andi_chrom` and `write_mzml`. It was previously reachable only through `write_chroms(what = "MS1")` or `read_chroms(export_format = "cdf")`, which write a whole list of chromatograms and offer no control over the file name or the instrument settings.
* `write_chroms(export_format = "cdf")` now forwards `...` to the underlying writer, as the `mzml` exporter already did. This makes the `ms_params` argument of `write_andi_ms` and the `lambda` argument of `write_andi_chrom` usable when writing a batch of files.
* Added a `bin_width` argument to `call_rainbow` as an alternative to `precision`, for m/z grids that are not a power of ten (e.g. `bin_width = 0.5`). `precision` is unchanged and remains the default.

### Improved handling of Python dependencies

* chromConverter is now more robust when you are offline. Python is only started when a parser that needs it (`rainbow`, `olefile` or `Aston`) is actually called, so the formats read by the internal parsers no longer require an internet connection. When Python is needed and the package index can't be reached, chromConverter now falls back on a previously cached environment instead of failing.
* Python packages are now requested only for the parser you actually call, so using the `rainbow` or `olefile` parsers no longer installs the 'Aston' requirements or constrains which version of `scipy` you can have.
* chromConverter no longer creates Python module objects in your global environment when the package is loaded.
* Fixed `configure_python_environment` so it accepts the `parser` argument it is called with, and removed its interactive prompts, which failed in non-interactive sessions.

### Performance

* Refactored internal 'Agilent' parsers for increased speed through vectorization of byte operations (~3.5-30x for the delta-encoded formats).  For example, a 10.8 MB 'ChemStation' version 31 `.uv` file went from ~9 s to ~0.57 s.
* Refactored 'Shimadzu' binary parsers for increased speed (7-55x) through vectorization of byte operations. Reading MS1 scans from a 40 MB `.qgd` file went from ~56 s to ~1 s, and reading a PDA stream from an `.lcd` file went from ~7 s to ~1 s.
* Refactored `read_varian_sms` for increased speed (~8x) through vectorization. Reading `STRD15.SMS` (2.4 MB, 935k MS1 rows) drops from ~13 s to ~1.7 s. The stream is also bounded by the end of the `MSData` section rather than the end of the file, which reduced peak memory requirements for files carrying a large tail of peak tables and results.
* Refactored `write_mzml` for increased speed (~1.4x) and lower memory use. The spectra are now sliced out of the long-format table in place instead of being copied into a list of per-scan tables, and the byte offsets for the index are accumulated as the file is written rather than probed with `seek()` once per scan. Writing 3432 scans (935k points) drops from ~1.24 s to ~0.87 s, with peak memory falling from ~384 MB to ~339 MB.
* Refactored conversion to long format for increased speed (~150x). The reshaping step now assembles the three columns directly instead of pivoting the table and then coercing it, which also avoids the rounding described below. Reshaping a 4689 x 328 PDA matrix drops from ~2.0 s to ~0.012 s, with peak memory falling from ~455 MB to ~227 MB. This affects every parser called with `data_format = "long"` (or `format_out = "data.table"`, which implies long format), as well as the mzML and ANDI MS writers, which reshape to long format internally.
* The temporary files that are extracted from 'Shimadzu' OLE containers are now deleted once they have been read, instead of accumulating in the session's temporary directory until R exits. This matters most when converting many files at once.

### Metadata field changes

* `read_acaml` now also returns the injection volume (`InjectionVolume`, `InjectionVolume_unit`) and the acquisition software name and version (`Software`, `SoftwareVersion`).
* `detector_range` is now reserved for the numeric wavelength range recorded by `.uv` files. For 'ChemStation' versions 30 and 130 the signal descriptor was previously reported in this field, and is now reported as `signal_descriptor`.
* The `detector` field is now `NA` for 'ChemStation' `.ch` files. These files do not record a detector type; the field previously reported the detector module, duplicating `detector_id`.
* The acquisition time of 'Thermo' RAW files is now named `run_datetime`, like every other format, rather than `run_date`.

### Deprecations

* The `aston` parser is deprecated and will be removed in a future release. 'Aston' has been unmaintained since 2020. It is now used only by `sp_converter` to read 'Agilent MassHunter' `.sp` files (`format_in = "masshunter_dad"`), and `read_chroms` selects it automatically only as a last resort, when no other parser can read the file. Please use the internal chromConverter parsers or the `entab` parser (by the same author as 'Aston') instead.
* `uv_converter` is now defunct; use `read_chemstation_uv` or the `entab` parser instead. The `aston` binding for `format_in = "other"` has also been removed; this format is still handled by the `entab` parser. Both relied on an 'Aston' reader that requires `scipy < 1.14`, which would otherwise constrain the Python environment for every user.
* Deprecated `dat` argument in `read_chroms`. Instead, chrom_lists can be combined with `c()`.
* Renamed the `data_format` argument of `read_peaklist` and `read_chemstation_reports` to `peaktable_format`. This argument selects `chromatographr` or `original` peak table layout, so it had nothing to do with the `data_format` argument of the chromatogram readers, which selects `wide` or `long` format. `peaktable_format` is the name already used for this option by `read_shimadzu`. The old name still works but warns, and will be removed in a future release.

### Bug fixes and other minor changes

* Fixed the `rainbow` parser, which raised `read() no longer takes precision` on every call once `rainbow-api` v1.5.0 was released. v1.5.0 split `precision` into `bin_width` (the m/z grid, in daltons) and `display_precision` (label rounding, in decimals); chromConverter now derives both from `precision`, so the argument and the data it returns are unchanged. v1.5.0 is now the minimum required version.
* Fixed a loss of precision in long-format data. The conversion from wide to long format finished by coercing the assembled table with `apply(x, 2, as.numeric)`. Because retention times entered that table as character (from the rownames), the coercion routed every column through a character matrix, formatting each intensity with `getOption("digits")` and so rounding it to 7 significant figures. Long-format intensities now match the wide-format values exactly. Exported files were affected too, since `write_mzml` and `write_andi_ms` reshape to long format before encoding.
* String metadata read from 'Agilent ChemStation' and 'Shimadzu' files is now decoded as Latin-1 and stripped of control characters. Previously these fields could contain bytes that made the resulting string invalid in the session encoding, so `nchar()` and `toupper()` failed on them and `grepl()` could not match them. Accented characters in a path or sample name are now preserved rather than mangled.
* Fixed a bug where the `thermoraw`, `openchrom`, `agilent_dx` and `agilent_amx` parsers deleted the whole session temporary directory on exit, instead of just the files they created. This behavior could potentially create conflicts with other packages. Each call now gets its own directory inside the session temp directory which is cleaned up on exit.
* 'Shimadzu' OLE containers are now closed as soon as they have been read. Previously the contents of the last stream read were also kept in memory until R exited, and file handles were released only when garbage collection got around to them.
* Fixed a bug on 'Windows' causing paths with backslashes to be rejected on Windows by the 'Shimadzu' binary parsers.
* The 'OpenChrom' batch file is now deleted after the conversion, instead of accumulating in the export directory.

#### 'Agilent'

* Fixed missing `detector_id` for 'ChemStation' version 130 files.
* Added `sample_position` metadata field for 'ChemStation' 179 files (`.ch` and `.it`).
* The acquisition time of 'Agilent MassHunter' files is now converted to `POSIXct` instead of being attached as an unparsed string, which `extract_metadata` reported as `NA`.
* Fixed a bug causing `read_agilent_dx`, `read_agilent_amx` and `read_agilent_rslt` to fail when `path_out` was supplied.
* Fixed the documentation of the peak table format argument to `read_chemstation_reports`, which listed the accepted values as `chromatographr` or `chemstation`. The second value has always been `original`, so following the documentation raised an error.

#### 'Shimadzu'

* Fixed `read_shimadzu_lcd` so it can return PDA data in long format. `read_shimadzu_lcd(what = "pda", data_format = "long")` previously failed with an error about a missing `lambda` column, because the reshaping step was called with the wrong target format.
* Fixed export of OLE streams to a path containing `~`, which is not expanded by Python.

#### 'Varian' SMS

* Fixed the acquisition timestamps for 'Varian SMS' files. The corrected start matches the timestamp written by 'OpenChrom' for the same sample, and the interval between the start and end times matches the span of the chromatogram.
* The `run_datetime` for 'Varian SMS' files is now the acquisition start time, as a single value rather than a start/end pair.
* Fixed `read_varian_sms` for `format_out = "data.table"`, which failed previously with an error. The `TIC` and `BPC` returned by this parser also had their intensity column named `tic`/`bpc` instead of `intensity` for this value of `format_out`.
* Added support for reading `instrument` and `method` metadata from Varian SMS files (read from the `InjectionLog` section).

#### ANDI (netCDF)

* Fixed the MS1 scans returned by `read_cdf` for 'ANDI MS' files in which every scan holds the same number of points (common when the instrument scans a fixed mass range). The retention times arrived as a matrix and were split into one column per scan, so a 20-scan file returned a table with `rt.1`, `rt.2`, ... `rt.20` columns instead of a single `rt` column. With `ms_format = "list"` the same files returned a list of individual numbers rather than a list of spectra. Files with a varying number of points per scan were unaffected, and their output is unchanged.
* `read_cdf` no longer opens the netCDF file twice, and the peak table returned for 'ANDI chrom' files is no longer transposed when it holds a single peak.

#### mzML export

* Fixed `write_chroms(export_format = "mzml")`, which failed for every file unless `what` was given explicitly. The streams to write are now inferred from the data, as they are when `write_mzml` is called directly.
* `write_mzml` no longer writes a one-dimensional chromatogram as DAD spectra, and points to `write_andi_chrom` instead. mzML stores scans of (m/z or wavelength, intensity), so a single trace has no axis to put in one: it was written as a single-point spectrum per retention time, which for a 66,000-point trace meant 66,000 scans, as many warnings about empty ranges, and a 128 MB file. The trace is skipped with a warning if other streams were requested, and reported as an error if it was the only one, since skipping it would leave an empty file. `TIC` and `BPC` are unaffected, since the format has terms for those MS-derived summaries and they are written to the chromatogram list rather than as spectra.
* `write_mzml` now throws a more informative error when handed a single chromatogram whose `detector` attribute is missing, `NA`, or names a detector it has no mzML stream for, rather than failing with `EXPR must be a length 1 vector` or quietly writing an unnamed stream.
* Fixed the `fileChecksum` written into indexed mzML files, which was the SHA-1 of the first line of the file (`<?xml version="1.0" encoding="UTF-8"?>`) rather than of the file itself, because the digest was taken over a multi-element character vector. Files are now checksummed as required by the mzML specification, over the bytes up to and including the opening `<fileChecksum>` tag, and the file no longer has to be read back into memory to do it.
* Fixed the offsets in the `indexList` of mzML files. `<indexListOffset>` pointed one byte before `<indexList>`, and every offset in the DAD spectrum index pointed at the newline preceding its `<spectrum>` element rather than at the element. Offsets are now counted as the file is written instead of being probed with `seek()`, which is unreliable on a connection opened in text mode and ignores the write buffer.
* Fixed the chromatogram index of mzML files. Each `<offset>` pointed four bytes before its `<chromatogram>` element, and its `idRef` named the element's `index` rather than its `id`, so no entry in the index resolved to the chromatogram it was meant to locate. 
* Fixed the spectrum-type term written into mzML files, which was always `MS:1000580` ("MSn spectrum") even though `ms level` was `1`.
* Fixed `write_mzml(compress = FALSE)`, which was ignored for spectra (though not for chromatograms), since the argument was never passed on.
* Fixed the `count` attribute of `<spectrumList>`, which was always written as `1` for data read as a `data.table`.
* mzML files are now written as binary, so their line endings are `LF` on all platforms.

#### Metadata and printing

* Fixed `metadata_format`, which several readers mishandled. `metadata_format = "raw"` errored for 'Chromeleon' files and returned `NULL` instead of a chromatogram for the `rainbow` parser; the `entab` parser and `read_shimadzu` ignored the argument altogether; and `parser = "entab"` with `format_in = "other"` returned `NULL`. The argument is now resolved in one place instead of separately by each reader.
* Metadata from 'Agilent ChemStation' report files is now attached to the peak lists that `read_peaklist` returns, so `extract_metadata` can see it.
* `extract_metadata` now returns a row for every chromatogram, however deeply nested, and reads sample-level attributes from the list enclosing a sample's traces as well as from the traces themselves. Previously only the top level of the list was examined, so nested traces, and any metadata held on the list grouping them, were left out of the table. A field that varies from trace to trace, such as `detector` in a multichannel file, stays with the trace; where the traces agree, the value on the enclosing list is used, since it describes the sample as a whole.
* `extract_metadata` now matches attribute names exactly. Previously a requested element could be filled in from a different attribute that merely started with the same characters, so a chromatogram with no `detector` attribute could report its `detector_y_unit` as its detector.
* `extract_metadata` now returns `NA` instead of a metadata frame with only a `name` column when none of the requested metadata elements are found.
* `print.chrom_list` now handles lists holding more than one trace per sample, such as a multichannel 'Shimadzu' file or an 'Agilent' `.dx` read with `what = c("chroms", "dad")`. Traces are grouped under the sample they belong to, however deeply nested, and attributes shared by all of a sample's traces are shown in that sample's block header instead of being repeated on every row. Previously only the top-level elements were counted, so the chromatogram count was wrong and only the first trace of each sample was shown.
* Improved `print.chrom_list` formatting: datetimes print as timestamps rather than raw epoch seconds; the header wraps to the width of the console, breaking between fields; long values, such as a 'Windows' `method` path, are shortened from the middle; and a field that is empty for every chromatogram is dropped. `print` no longer errors when none of the requested `cols` are present or when `n` is negative, and `n` now defaults to `10`, as documented.
* The file-level properties that `read_mzml` recovers are now attached as attributes, so `extract_metadata` and `print.chrom_list` can see them, and the `metadata` element carrying them is no longer counted as a chromatogram. `run_datetime`, `time_range`, `time_unit` and `detector_range` previously came back as `NA` for mzML files even though 'RaMS' had parsed them, which also meant `read_chroms(sort_by = "acquisition_time")` could not order them. The element is still returned in full, since it carries fields with no attribute equivalent.

#### `read_chroms`

* Refactored the dispatch in `read_chroms`. A single internal table now records which formats it can read and, for each one, the file extension, and the reader each parser uses. These facts were previously spread across the argument list, an `if`/`else` chain, and several utility functions (`check_parser` and `format_to_extension`). The change is internal, but it resolves several problems listed below.
* Arguments passed through `...` are now matched against the arguments the selected parser actually accepts. An unrecognized argument previously made every file fail, surfacing from inside `try()` as a warning with an unreadable message; such arguments are now ignored with a warning naming them.
* When `format_in` is not supplied and the type of a file cannot be recognized, `read_chroms` now says so and asks for a format, rather than failing with `argument is of length zero`.
* `read_chroms` now gives an informative error when no parser is available for a format, instead of failing with `missing value where TRUE/FALSE needed`.
* When a file cannot be interpreted, `read_chroms` now names it instead of reporting its position in the list.
* A file that cannot be interpreted now produces a single warning naming it, rather than a warning followed by a separate message. The message could not be silenced with `suppressWarnings` and was invisible to callers handling the warning.
* Some formats can now be named in more than one way. `format_in` accepts an alias as readily as the format's own name, and the two behave identically: `rslt` and `sirslt` for `agilent_rslt`, `openlab_dx` for `agilent_dx`, `chemstation_fid` for `chemstation_ch`, `andi` for `cdf`, and `allotrope` for `asm`.
* The `shimadzu_ascii`, `csv`, `asm`, `openlab_131` and `chemstation` formats are now matched to the correct file extension, instead of falling through to a pattern matching any file containing a `.`.
* The `metadata_format` argument now reaches the `agilent_dx`, `agilent_rslt`, `shimadzu_lcd`, `shimadzu_qgd`, `cdf` and `entab` parsers. Previously, `metadata_format = "raw"` had no effect for these formats.
* For formats that return more than one chromatogram per sample, `sample_names = "sample_name"` named every sample with the literal string `"NULL"` and then warned about duplicate names. This was because the `sample_name` attribute is attached to the individual chromatograms rather than to the list grouping them, so the lookup came up empty and the resulting `NULL` was coerced to a string. Samples with no recorded sample name now fall back to the file name, with a warning naming them. A name that is recorded but empty counts as no name, since a parser that finds the field but reads nothing out of it leaves an empty string behind. The traces making up a sample are now also checked against each other: if they disagree about the sample name there is no basis for preferring one over another, so the file name is used instead, again with a warning.

## chromConverter 0.9.1

### New features

* Added a `[.chrom_list` method so that subsetting a `chrom_list` preserves its class instead of dropping it to a plain `list`.
* Added a `c.chrom_list` method so that combining `chrom_list` objects with `c()` preserves the class instead of dropping it to a plain `list`.

### Bug fixes and other minor changes

* Fixed encoding bug when parsing XML metadata in `read_shimadzu_lcd`: (bytes are now read explicitly as ISO-8859-1 rather than relying on system locale via `readLines()`).
* Updated for compatibility with rainbow v1.3.0, which renamed the `prec` argument to `precision`; chromConverter now requires rainbow >= 1.3.0.
* Fixed vignette example for `varian_sms` so the example file is downloaded  in binary mode (`mode = "wb"`), preventing file corruption on Windows.
* Added `sample_position` field to `extract_metadata`.

## chromConverter 0.9.0

### Breaking changes

* Consolidated `sample_id` and `vial` metadata fields into new `sample_position` field.
* Added `sparse` argument for `rainbow` parser (now enabled by default) to remove zeros form long-format MS data.
* Changed order of `what` arguments in `read_agilent_d` to prioritize DAD data (instead of 2D chromatograms).
* Fixed bug so that Varian long-format MS data is returned as a data.frame by default (rather than a matrix).
* Added `chrom_list` class and `print.chrom_list` method. Instead of dumping the full contents of every chromatogram, prints a compact metadata summary with configurable columns (`cols`) and row limit (`n`).

### New features

* Added support for reading Agilent Common Analytical Markup Language (ACAML) files.
* Added support for reading Agilent OpenLab method files (`.amx`). 
* Added preliminary support for reading "Chromatotec" `.Chrom` files through the `read_chromatotec` function.
* Added support for reading regular utf8-encoded `csv` files.
* Pass source file through when reading `agilent_dx` files so that the original source file is stored in metadata instead of a temp file.
* Added warning in `read_chroms` for duplicated names as they may silently interfere with downstream analyses.

### Improved support for exporting files

* Added option to write ARW files in `write_chroms`. This format seems to be the simplest way to get DAD data into `OpenChrom`.
* Added support for writing standard utf8-encoded `csv` files and fixed a bug causing column names to be prepended with "X".
* Modified `write_chroms` so it invisibly returns file names of the exported chromatograms.
* Improved error handling within `write_chroms`.

#### CDF

* Fixed bug causing failure to write chromatograms with missing attributes to `.cdf` (thanks to @pbulsink for PR #37).
* Fixed bug causing failure to write existing timestamp data to `.cdf` files.
* Fixed bug causing failure to write ANDI chrom files on Windows due to failure to coerce numeric metadata to text.
* Added additional metadata fields to exported CDF files.
* Added additional test for writing CDF files with missing attributes.
* Fixed CDF time range metadata bug.

#### mzML

* Fixed `write_mzml` to correctly handle DAD spectra.
* Added chromatograms to mzML files written by `write_mzml`.
* Fixed incorrect CV accession for no compression (MS:1000576).

### Other bug fixes and minor changes

* Fixed entab parser so it translates `.ch` files to wide format when specified.
* Fixed bug in `read_agilent_d` when subsetting data with the `what` argument.
* Fixed Waters ARW parser so it can read files with missing metadata.
* Added assumption that time units for data generated by `rainbow` parser are in minutes.
* Rainbow parser now returns sparse MS data by default (excluding zeros) when long format is requested to match format returned by other parsers.
* Fixed problems with Aston converters due to changes in reticulate behavior.
* Fixed bug so that data from ANDI MS netCDF files can be returned as `data.table` object when specified by user.
* Fixed 'Shimadzu' QGD bug affecting large values. Resolves ([#44](https://github.com/ethanbass/chromConverter/issues/44)).
* Fixed bug causing sample_position/vial metadata to be dropped when reading Agilent files.
* Fixed bug so that temp directories created by certain file parsers (e.g., `read_agilent_dx` and `read_themoraw`) are actually deleted on completion.
* Refactored `extract_metadata` function for simplicity.

## chromConverter 0.8.0

* Improved support for 'Agilent OpenLab' `.dx` files: extraction of DAD and auxiliary instrumental data (stored in `.IT` files).
* Refactored `read_shimadzu_qgd` for a 1.4x speedup in the parsing of Shimadzu `.qgd` files, cutting execution time by 30%.
* Refactored `read_shimadzu_lcd` for a 2.4x speedup in the parsing of Shimadzu `.lcd` files, cutting execution time by 60%.
* Refactored `write_mzml` for massive speed-up when writing mzML files, especially for large MS data.
* Fixed 'Shimadzu' metadata time zone offsets.
* Fixed misplaced parentheses in `read_agilent_d` that were causing possible bug.
* Fixed bug in `read_chemstation_uv` causing error for long format data.
* Added more informative error messages for `read_agilent_d`.
* Added additional tests for retention times and `data_format` attribute.
* Added `data_format` and `read_metadata` arguments for `read_chemstation_csv`.
* Fixed incorrect `data_format` attributes for MS data to reflect that they are always returned in long format.
* Fixed documentation to accurately reflect the fact that MS data is always returned in long format.
* Automatically return long format when `data.table` output is selected since data.tables do not have rownames.
* Fixed error due to fractional timezones in Shimadzu metadata (e.g., India +05:30).
* Fixed bug in `write_mzml` causing retention time shifts for BPC and TIC.
* Rewrote `configure_python_environment` function to facilitate configuration of a chromConverter virtual environment or conda environment, though a dedicated environment is no longer required (as of chromConverter v0.7.4).
* Fixed bug in `collapse` argument causing functions to return vector when `format_out` is `data.frame`.
* Fixed bug causing elimination of retention times when `format_out` is `data.table`.
* Enabled `data.table` format in `read_shimadzu_ascii`.
* Enabled automatic recognition of 'Agilent OpenLab' `.dx` file by `read_chroms`.
* Fixed long format output for `read_shimadzu` ('Shimadzu' ASCII files).
* Fixed timezone issue in some 'Agilent ChemStation' files.

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

