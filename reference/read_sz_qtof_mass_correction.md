# Read the cached mass correction from a 'Shimadzu' QTOF file

`Mass Data Load Format/Mass Correction Cache` holds the mass correction
'LabSolutions' applied to the run: the reference ions it found and the
coefficients it fitted to them. Like the other QTOF parameter streams it
is a protocol buffer with a block per polarity (field `10` positive,
`20` negative), the reference compounds under `10` and the result under
`30`:

## Usage

``` r
read_sz_qtof_mass_correction(path, polarity = read_qtof_polarity(path))
```

## Arguments

- path:

  Path to 'Shimadzu' .lcd file.

- polarity:

  Ion polarity, either `positive` or `negative`, selecting which of the
  two blocks to read. Defaults to the polarity recorded in the file.

## Value

A list of `scale`, `offset` and `shift`, or `NULL` if the stream is
missing or holds no usable result.

## Details

- `30.<pol>.10.10` — offset of the correction, in flight-time units

- `30.<pol>.10.20` — scale factor of the correction, near 1

- `30.<pol>.20.10.10`, `.20` — id of a reference compound and the m/z at
  which it was measured in this run, scaled by 1e9

The correction is applied to the flight time, as
`t' = scale * t + offset`, which is the same as shifting the calibration
in the square root of the mass: \$\$\sqrt{mz'} = scale \sqrt{mz} +
shift\$\$ where `shift` is the mean of
`sqrt(theoretical) - scale * sqrt(measured)` over the reference ions.
Reading the vendor's own correction out reproduces its mass axis
exactly, where refitting on the reference ions found in the data leaves
a few tenths of a ppm.
