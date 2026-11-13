This is an update from chromConverter 0.11.2, the version currently on CRAN.
It fixes the M1mac test failures reported for 0.11.2.

## Fix for the M1mac test failures

The M1mac additional checks reported four test failures in 0.11.2. They came
from parsing AM/PM with `%p`, which depends on the `LC_TIME` locale.
Date-times are now parsed with `LC_TIME` set to "C".

I reproduced the failures on GitHub Actions under the M1mac locale settings
(`en_GB.UTF-8`, `LC_COLLATE=C`, `TZ=Europe/London`), and all tests pass with
the fix. The GitHub Actions checks now run under these settings and under
French locales.

## R CMD check results

There were no ERRORs or WARNINGs.

There is 1 NOTE:

* Suggests or Enhances not in mainstream repositories:
  entab, chromConverterExtraTests
Availability using Additional_repositories specification:
  entab                      yes   https://ethanbass.github.io/drat/
  chromConverterExtraTests   yes   https://ethanbass.github.io/drat/

Uses the non-portable package: ‘mzR’

Entab, mzR, and chromConverterExtraTests are not required; all uses of them are
conditional. Both entab and chromConverterExtraTests are available from my drat
repository, which is listed in 'Additional_repositories', and where to obtain
them is also described in the 'Description' field.

## Test environments

R CMD check gives no ERRORs or WARNINGs on the environments below, and no NOTEs
other than the one above.

Local:

    macOS Sonoma 14.7.6 (aarch64), R 4.6.1, R CMD check --as-cran

GitHub actions:

    Linux (Ubuntu 26.04): R-release 4.6.1 (fr_FR.UTF-8),
      R-devel (2026-10-26 r90766), R-oldrel-1 4.5.3
    macOS Tahoe 26.6 (aarch64): R-release 4.6.1 (fr_FR.UTF-8),
      R-release 4.6.1 (en_GB.UTF-8, as on M1mac)
    Windows: R-release 4.6.1 (French_France.utf8)

CRAN win-builder:

    Windows Server 2022 x64: R-devel (2026-11-12 r90881 ucrt)

mac-builder:

    macOS Tahoe 26.6 (aarch64): R 4.6.1 Patched (2026-07-27 r90311)
