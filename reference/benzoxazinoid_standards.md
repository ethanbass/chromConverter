# Benzoxazinoid standards

A calibration series of a mixed benzoxazinoid standard, run by HPLC with
an 'Agilent' diode array detector (G1315A), as ten 'ChemStation' `.D`
directories in `extdata`. Find them with
`system.file("extdata", "benzoxazinoid_standards", package = "chromConverter")`.

## Details

- `BENZOS_1000PPM.D` to `BENZOS_4PPM.D`: nine twofold dilutions from
  1000 ppm, injected between 20 and 22 June 2023. The names round the
  last three, which are 15.625, 7.8125 and 3.90625 ppm.

- `MEOH.D`: a methanol blank, run with the same method on 15 June 2023.

Each directory holds the 254 nm trace (`dad1A.ch`) and the 'ChemStation'
report (`Report.TXT`). `BENZOS_250PPM.D` also holds the traces at 230,
320, 360 and 210 nm (`dad1B.ch` to `dad1E.ch`), and the pump's pressure,
flow and solvent composition through the run (`LCDIAG.REG`), which
[read_agilent_d](https://ethanbass.github.io/chromConverter/reference/read_agilent_d.md)
returns with `what = "instrument"`.

The standard gives four peaks at 254 nm, eluting in the 250 ppm run at
12.5 (DIBOA), 19.1 (DIMBOA), 21.0 (BOA) and 28.2 minutes (MBOA).

## License

These files are released under CC0 1.0
(<https://creativecommons.org/publicdomain/zero/1.0/>).

## See also

`vignette("chromConverter")`, which works through these files.
