# nat.nblast 1.6.9

* fix: stop using deprecated `rgl::rgl.close()`

# nat.nblast 1.6.8

* docs: add missing package anchors as requested by CRAN 

# nat.nblast 1.6.6

* Keep scale factor when normalising (#38)
* Doc fixes

# nat.nblast 1.6.4

* pkgdown documentation
* use development version of nat package when doing install_github of nat.nblast
* spelling

# nat.nblast 1.6.3

* fixes/examples for show_similarity
* allow score matrices to be passed as first param to nhclust (but issue a
  warning)
* change vignette engine to rmarkdown

# nat.nblast 1.6.2

* minor fixes for 3.3
* add vignette
* simplify travis setup

# nat.nblast 1.6.1

* dev: fix test for different normalisation types

# nat.nblast 1.6

* fix handling of non-square matrices in sub_score_mat
* minor doc fixes

# nat.nblast 1.5

* switch from importing nat to depending on it

# nat.nblast 1.4

* use nabor package, not RANN, for finding nearest neighbours
* add functions for creating sparse score matrices, using spam package
* speed up extraction of diagonal elements for on-disk score matrices
* nblast now handles all combinations of neuron and dotprops objects

# nat.nblast 1.3

* functions to create scoring matrices; see create_scoringmatrix for details.
* fix: really use nhclust's distfun argument
* new nblast_allbyall function
* add plyr support (gives parallelisation and progress bar options)

# nat.nblast 1.2

* add nhclust, plot3d.hclust, subset.hclust to help cluster/plot based on
  nblast scores (all ported from flycircuit package).
* rename showSimilarity -> show_similarity
* dev: make sure tests run during check()
* test: update baselines given score matrix changes in 1.1

# nat.nblast 1.1

* fix: make smat.fcwb the default scoring matrix (rather than smat_alpha) when
  nblast's UseAlpha parameter is FALSE (the default) and smat_alpha.fcwb the
  default when UseAlpha=TRUE.
* rename smat->smat.fcwb and smat_alpha->smat_alpha.fcwb to make it clear that
  these are defined in the FCWB template space (though they should work OK for
  other fly template brains that have an absolute physical scale)
* don't set options("nat.nblast.defaultsmat""), just query it.
* doc: basic package documentations
