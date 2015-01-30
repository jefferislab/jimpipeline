# jimpipeline

## Installation
Currently there isn't a released version on [CRAN](http://cran.r-project.org/).

### Released versions
Released versions are available from our lab repository:

```r
install.packages("jimpipeline",repos='http://jefferislab.org/R',type='source')
```

### Bleeding Edge
You can use the **devtools** package to install the development version:

```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("jefferislab/jimpipeline")
```

Note: Windows users need [Rtools](http://www.murdoch-sutherland.com/Rtools/) and [devtools](http://CRAN.R-project.org/package=devtools) to install this way.
