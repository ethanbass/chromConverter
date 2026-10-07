# Read 'Agilent ChemStation' register files

Reads the instrument traces, and the keys and values describing each
module, from 'Agilent ChemStation' register (`.REG`) files. Users reach
it through
[read_agilent_d](https://ethanbass.github.io/chromConverter/reference/read_agilent_d.md)
with `what = "instrument"`; this page documents the file format.

## Usage

``` r
read_chemstation_reg(path)
```

## Arguments

- path:

  Path to a `.REG` file.

## Value

A list of two data frames:

- `traces`: one row per point of each instrument trace, with columns
  `trace` (its title, e.g. `"PMP1, Pressure"`), `unit`, `time` (in
  minutes) and `value`.

- `conditions`: the keys and values stored in every object of the file,
  with columns `object` (its title), `key` and `value` (as character).
  Each module's `Start/Stop Conditions` object (e.g.
  `"PMP1, Start/Stop Conditions"`) records its state at the start and
  end of the run, such as `StartPressure` and `StopPressure` for the
  pump or `ActInjVolume` for the autosampler. Every object records the
  start of the run as `DateTime`, and the object of a solvent trace
  gives the solvent's name, where one was entered, as `Description`.

## Details

Objects that cannot be parsed are skipped with a warning, and tables
inside register files are not returned. Tested on `LCDIAG.REG` files
from revisions A.10.02, B.01.03, B.04.02, B.04.03, C.01.03, C.01.07 and
C.01.10.

## Note

The reader for revision A files was adapted from `read_reg_file` in
[Aston](https://github.com/bovee/aston) (Copyright 2011-2020 Roderick
Bovee, BSD 3-clause license).

## File format

All integers are little-endian and offsets are counted from 0.

**Header.** Bytes `02 33 32 00`, then the length-prefixed string
`REGISTER FILE` at 0x04 and a length-prefixed container version at 0x18:
`notused` in files from revisions B and C, `A.xx.xx` in files from
revision A. At 0x22 is a u32 offset of the object index and at 0x26 a
u16 object count. The index holds one 8-byte entry per object, starting
with the u32 offset of the object; the index itself begins where the
last object ends.

**Revision B and C objects.** A 12-byte preamble, whose contents differ
by revision, then a stream written by the 'MFC' `CArchive` class. Each
object starts with a u16 tag: `FFFF` introduces a new class (u16 schema,
u16 name length, ASCII name); `8000` plus an index refers to a class
already read; `7FFF` is followed by a u32 tag for large indices. Class
indices restart in every object. The classes are:

- `CHPUserObject` and `CHPLCObject`: two objects (the second a list of
  keys and values), then a flag byte and, if it is set, a data object.

- `CHPList`: one object. `CObArray`: a u16 count (`FFFF` then a u32
  count when large) followed by that many objects.

- `CHPNdrDouble`, `CHPNdrString` and `CHPNdrObject`: a key, 6 bytes,
  then a float64, a string or a flag byte and object. Strings are a u16
  length followed by UTF-16LE; a length of `C000` is instead followed by
  a u16 id naming a standard key (0 `ObjClass`, 1 `Title`).

- `CHPDatLongSliced` and `CHPDatDoubleSliced`: 6 bytes, then the y row
  and the x row.

- `CHPDatLongRow` and `CHPDatDoubleRow`: 14 bytes, the unit string, u16
  `7`, u16, u32 point count n and a u8 flag marking an implicit row.
  Unless implicit, n int32 (`Long`) or float64 (`Double`) values follow.
  Then u32, float64 first value, u8, u32 flag marking scaled values,
  float64 scale and float64 offset. Values are raw x scale + offset if
  scaled and raw otherwise; an implicit row is first + i x scale.

- `CHPAnnText` and `CHPTable` are read past but not returned.

In `LCDIAG.REG` each trace is an object whose `Title` names it. Revision
B.01 stores traces as `CHPLCObject` with `CHPDatLong` rows and an
implicit time axis; B.04 does the same but writes a space before the
comma in trace names (`"PMP1 , Pressure"`), which is removed; revision C
stores them as `CHPUserObject` with unscaled `CHPDatDouble` rows and
explicit, possibly uneven, times. The other objects, titled
`Start/Stop Conditions`, hold only keys and values.

**Revision A objects.** One byte, a u32 record count n, n 16-byte record
headers (u16, u16 type, u32 size, u32, u32 id), 4n further bytes, then
the data of each record in turn. Records refer to one another by id.
Types `8001` and `8003` are Latin-1 strings and `8006` a string after 2
bytes; `8002` holds a numeric array; `0602` (43 bytes) a key at byte 14
and float64 at byte 35; `0601` a key and the u32 id of its value at byte
35. Types `0501` and `0503` (161 bytes) describe a trace: u32 point
count at byte 9, then the u32 ids of the x unit and data and the x scale
at bytes 27, 31 and 61, and those of y at bytes 94, 98 and 128. An array
id with no record denotes an implicit axis of i x scale.

Not known: the meaning of the skipped bytes, and whether the scale or
the offset comes first in a row (every file seen has an offset of 0).

## Author

Ethan Bass
