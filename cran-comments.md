## R CMD check results

There were no ERRORs or WARNINGs. 

There is 1 NOTE:

* Suggests or Enhances not in mainstream repositories:
  entab, chromConverterExtraTests
Availability using Additional_repositories specification:
  entab                      yes   https://ethanbass.github.io/drat/
  chromConverterExtraTests   yes   https://ethanbass.github.io/drat/
  ?                            ?   https://ethanbass.r-universe.dev/

Uses the non-portable package: ‘mzR’

Entab, mzR, and chromConverterExtraTests are not required. The entab and 
chromConverterExtraTests packages can be installed from my drat repository or 
from my R-universe repository.

## Test environments

The package has been checked on all environments listed below, and generates 
only the single note described above.

GitHub actions:

    Linux: R-release, R-devel, R-oldrel-1
    Mac-OS: R-release, R-devel
    Windows: R-release, R-devel

CRAN win-builder:

    R-release, R-devel
