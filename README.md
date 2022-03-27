# chromConverter
chromConverter is an R wrapper around the agilent UV converter in the [Aston](https://github.com/bovee/aston) package for python.

## Installation
You can install `chromatographR` from GitHub using the devtools package:
```
install.packages("devtools")
devtools::install_github("https://github.com/ethanbass/chromatographR/")
```

After installing, you can (hopefully) install the python dependencies using a built in function, as follows:

```
library(chromConverter)
install_python_dependencies()
```
