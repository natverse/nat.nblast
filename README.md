# nat.nblast
[![Release Version](https://img.shields.io/github/release/jefferislab/nat.nblast.svg)](https://github.com/jefferislab/nat.nblast/releases/latest) 
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/nat.nblast)](https://cran.r-project.org/package=nat.nblast) 
[![Build Status](https://travis-ci.org/jefferislab/nat.nblast.svg?branch=master)](https://travis-ci.org/jefferislab/nat.nblast)

## Quick Start

For the impatient ...

```r
# install
if (!require("devtools")) install.packages("devtools")
devtools::install_github(c("jefferis/nat", "jefferislab/nat.nblast"))

# use
library(nat.nblast)

# run examples for search
example("nblast")

# run examples for clustering
example("nhclust")

# get overview help for package
?nat.nblast
# help for functions
?nblast
?nhclust

# run tests
library(testthat)
test_package("nat.nblast")
```

## Introduction
This R package implements the NBLAST neuron similarity algorithm described in a preprint available at
<http://dx.doi.org/10.1101/006346>.  In addition to basic pairwise comparison, the package implements search of
databases of neurons.  There is also suport for all x all comparison for a group of neurons. This can produce a distance
matrix suitable for hierarchical clustering, which is also implemented in the package.

These tools are designed as an addon for the [NeuroAnatomy Toolbox](https://github.com/jefferis/nat) (nat) R package, 
which you must first install.

## Installation
This package has now been released to [CRAN](https://cran.r-project.org/package=nat.nblast)
(as of v1.5), but since this is the first official release,
you may wish to install the development version from GitHub, especially if you notice a bug.
Although **nat** (the main dependency) is available on [CRAN](https://cran.r-project.org/package=nat),
it it recommended that you install the development version of both packages 
if you want to install the development version of **nat.nblast**: **nat.nblast** is evolving fast 
and has required several minor tweaks to **nat**, which is otherwise quite mature.

### CRAN release
```r
install.packages("nat.nblast")
```

### Development version
You can download the [tar ball](https://github.com/jefferislab/nat.nblast/tarball/master),
and run `R CMD INSTALL` on it, or (recommended) use the **devtools** package to install the development version:

```r
# install devtools if required
if (!require("devtools")) install.packages("devtools")
# then install nat
devtools::install_github("jefferis/nat")
# then nat.nblast
devtools::install_github("nat.nblast", "jefferislab")
```

Note: Windows users need [Rtools](http://www.murdoch-sutherland.com/Rtools/) to install this way.
