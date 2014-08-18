# nat.nblast
[![Build Status](https://travis-ci.org/jefferislab/nat.nblast.svg)](https://travis-ci.org/jefferislab/nat.nblast)

## Quick Start

For the impatient ...

```r
# install
devtools::install_github(c("jefferis/nat", "jefferislab/nat.nblast"))

# use
library(nat.nblast)

# get help
?nat.nblast

# example clustering 20 Kenyon cells by morphology
data(kcs20, package='nat')
# all by all nblast scores
kcs20.scores=nblast_allbyall(kcs20)
# cluster using ward's (default) hierarchical clustering
hckcs=nhclust(scoremat = kcs20.scores)
# plot resultant dendrogram
plot(hckcs)
# the three groups (red, green, blue) correspond to alpha/beta, alpha'/beta', 
# and gamma neurons, respectively. 
open3d()
plot3d(hckcs, k=3, db=kcs20)

# colour dendrogram 
library(dendroextras)
hckcs.d=colour_clusters(hckcs, k=3)
# label neurons according to manually defined morphological type:
labels(hckcs.d)=with(kcs20[labels(hckcs.d)],as.character(type))
plot(hckcs.d)
```

## Introduction
This R package implements the NBLAST neuron similarity algorithm described in a preprint available at
<http://dx.doi.org/10.1101/006346>.  In addition to basic pairwise comparison, the package implements search of
databases of neurons.  There is also suport for all x all comparison for a group of neurons. This can produce a distance
matrix suitable for hierarchical clustering, which is also implemented in the package.

These tools are designed as an addon for the [NeuroAnatomy Toolbox](https://github.com/jefferis/nat) (nat) R package, 
which you must first install.

## Installation
There is currently no released version on CRAN.

### Bleeding Edge
You can, however, download the [tar ball](https://github.com/jefferislab/nat.nblast/tarball/master),
and run `R CMD INSTALL` on it, or use the **devtools** package to install the development version:

  ```r
# install devtools if required
if (!require("devtools")) install.packages("devtools")
# then install nat
devtools::install_github("jefferis/nat")
# then nat.nblast
devtools::install_github("nat.nblast", "jefferislab")
```

Note: Windows users need [Rtools](http://www.murdoch-sutherland.com/Rtools/) and
[devtools](http://CRAN.R-project.org/package=devtools) to install this way.
