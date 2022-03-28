# chromConverter
chromConverter is an R package to facilitate the conversion of chromatographic data from binary formats. 

It currently consists of a wrapper around the converters in the [Aston](https://github.com/bovee/aston) package for python.

This includes Agilent `.uv`, `.ms`, etc.

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

Then convert files by specifying the path and the format.

```
dat <- import_chrom(path, format.in=c("chemstation.uv", "masshunter.DAD")
```
