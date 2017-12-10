# jimpipeline
This package contain functions used by the [Jefferis Lab](http://jefferislab.org) during our large scale
image processing and registration work. We will try to make these as
generic as possible, but for the moment use by third parties is not a major
design consideration.

## Installation
Currently there isn't a released version on [CRAN](http://cran.r-project.org/).

You can use the **devtools** package to install the development version:

```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("jefferislab/jimpipeline")
```

Note: Windows users need [Rtools](http://www.murdoch-sutherland.com/Rtools/) and [devtools](http://CRAN.R-project.org/package=devtools) to install this way.
