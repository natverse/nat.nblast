# nat.nblast
[![Build Status](https://travis-ci.org/jefferislab/nat.nblast.svg)](https://travis-ci.org/jefferislab/nat.nblast)

Very preliminary attempt to clean up the neuron search tools originally developed as part of the [AnalysisSuite](https://github.com/jefferis/AnalysisSuite) codebase. These tools are designed to be an addon for the [NeuroAnatomy Toolbox](https://github.com/jefferis/nat) (nat) package.

## Installation
There is currently no released version on CRAN.

### Bleeding Edge
You can, however, download the [tar ball](https://github.com/jefferislab/nat.nblast/tarball/master),
and run `R CMD INSTALL` on it, or use the **devtools** package to install the development version:

  ```r
# install.packages("devtools")
devtools::install_github("nat.nblast", "jefferislab")
```

Note: Windows users need [Rtools](http://www.murdoch-sutherland.com/Rtools/) and
[devtools](http://CRAN.R-project.org/package=devtools) to install this way.
