## R CMD check results
There were no ERRORs or WARNINGs. 

There were 2 NOTES:

* Suggests or Enhances not in mainstream repositories:
  entab
Availability using Additional_repositories specification:
  entab   yes   https://ethanbass.github.io/drat/

Entab is suggested but not required. It can be downloaded from my drat repository or from its official github page https://github.com/bovee/entab.

* Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1021/acs.jproteome.9b00328
    From: man/read_thermoraw.Rd
    Status: 503
    Message: Service Unavailable
    
The URL in question appears to be perfectly valid. At least it loads in the web browser just fine.
