# nat.nblast <img src="man/figures/logo.svg" align="right" height="139" />
<!-- badges: start -->
[![natverse](https://img.shields.io/badge/natverse-Part%20of%20the%20natverse-a241b6)](https://natverse.org/)
[![Release Version](https://img.shields.io/github/release/natverse/nat.nblast.svg)](https://github.com/natverse/nat.nblast/releases/latest) 
[![CRAN status](https://www.r-pkg.org/badges/version/nat.nblast)](https://CRAN.R-project.org/package=nat.nblast)
[![Docs](https://img.shields.io/badge/docs-100%25-brightgreen.svg)](https://natverse.org/nat.nblast/reference/)
[![R-CMD-check](https://github.com/natverse/nat.nblast/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/natverse/nat.nblast/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/natverse/nat.nblast/graph/badge.svg?token=VqX3HEdELo)](https://codecov.io/gh/natverse/nat.nblast)
<!-- badges: end -->

**nat.nblast** is part of the [NeuroAnatomy Toolbox](https://jefferislab.github.io/)
suite of R packages.

## Quick Start

For the impatient ...

```r
# install
if (!require("devtools")) install.packages("devtools")
devtools::install_github(c("natverse/nat", "natverse/nat.nblast"))

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
This R package implements the NBLAST neuron similarity algorithm described in

Costa M, Manton JD, Ostrovsky AD, Prohaska S, Jefferis GS.
NBLAST: Rapid, Sensitive Comparison of Neuronal Structure and Construction of Neuron Family
Databases.Neuron. 2016 Jul 20;91(2):293-311. doi:
[10.1016/j.neuron.2016.06.012](http://doi.org/10.1016/j.neuron.2016.06.012). 

In addition to basic pairwise comparison, the package implements search of
databases of neurons. There is also support for all x all comparison for a group
of neurons. This can produce a distance matrix suitable for hierarchical clustering,
which is also implemented in the package.

These tools are designed as an addon for the [NeuroAnatomy Toolbox](https://natverse.org/nat/)
(nat) R package, which will be installed as dependency.

You will probably find the following online documentation helpful:

* https://natverse.org/nat.nblast/ - Online documentation for this R package
* https://jefferislab.org/si/nblast/ - Overview of NBLAST algorithm and online tools 

## Installation
This package has been released to [CRAN](https://cran.r-project.org/package=nat.nblast)
(since v1.5), but we generally recommend installing the development version from
GitHub, especially if you notice a bug.

### CRAN release
```r
install.packages("nat.nblast")
```

### Development version
Use **remotes** to install the development version:

```r
# install devtools if required
if (!require("remotes")) install.packages("remotes")
# then nat.nblast
remotes::install_github("natverse/nat.nblast")
```
Note that this will also update the [nat package](https://github.com/natverse/nat)
to the latest development version from github. Windows users need 
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) to install this way.


