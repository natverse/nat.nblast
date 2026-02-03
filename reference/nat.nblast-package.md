# Neuron similarity, search and clustering tools

**nat.nblast** provides tools to compare neuronal morphology using the
NBLAST algorithm (Costa et al. 2016).

## Similarity and search

The main entry point for similarity and search functions is
[`nblast`](https://natverse.org/nat.nblast/reference/nblast.md). Traced
neurons will normally be converted to the
[`dotprops`](https://rdrr.io/pkg/nat/man/dotprops.html) format for
search. When multiple neurons are compared they should be in a
[`neuronlist`](https://rdrr.io/pkg/nat/man/neuronlist.html) object.

The current NBLAST version (2) depends on a scoring matrix. Default
matrices trained using *Drosophila* neurons in the FCWB template brain
space are distributed with this package (see
[`smat.fcwb`](https://natverse.org/nat.nblast/reference/smat.fcwb.md));
see **Scoring Matrices** section below for creating new scoring
matrices.

`nblast` makes use of a more flexible but more complicated function
`NeuriteBlast` which includes several additional options. The function
`WeightedNNBasedLinesetMatching` provides the primitive functionality of
finding the nearest neighbour distances and absolute dot products for
two sets of segments. Neither of these functions are intended for end
use.

Calculating all by all similarity scores is facilitated by the
[`nblast_allbyall`](https://natverse.org/nat.nblast/reference/nblast_allbyall.md)
function which can take either a
[`neuronlist`](https://rdrr.io/pkg/nat/man/neuronlist.html) as input or
a character vector naming (a subset) of neurons in a (large)
[`neuronlist`](https://rdrr.io/pkg/nat/man/neuronlist.html). The
[`neuronlist`](https://rdrr.io/pkg/nat/man/neuronlist.html) containing
the input neurons should be resident in memory i.e. not the
[`neuronlistfh`](https://rdrr.io/pkg/nat/man/neuronlistfh.html).

## Clustering

Once an all by all similarity score matrix is available it can be used
as the input to a variety of clustering algorithms.
[`nhclust`](https://natverse.org/nat.nblast/reference/nhclust.md)
provides a convenient wrapper for R's hierarchical clustering function
[`hclust`](https://rdrr.io/r/stats/hclust.html). If you wish to use
another clustering function, then you can use the
[`sub_dist_mat`](https://natverse.org/nat.nblast/reference/sub_dist_mat.md)
to convert a raw similarity score matrix into a normalised distance
matrix (or R [`dist`](https://rdrr.io/r/stats/dist.html) object)
suitable for clustering. If you need a similarity matrix or want to
modify the normalisation then you can use
[`sub_score_mat`](https://natverse.org/nat.nblast/reference/sub_score_mat.md).

Note that raw NBLAST scores are not symmetric (i.e. S(A,B) is not equal
to S(B,A)) so before clustering we construct a symmetric
similarity/distance matrix `1/2 * ( S(A,B)/S(A,A) + S(B,A)/S(B,B) )`.
See
[`sub_score_mat`](https://natverse.org/nat.nblast/reference/sub_score_mat.md)'s
documentation for details.

## Cached scores

Although NBLAST is fast and can be parallelised, it makes sense to cache
to disk all by all similarity scores for a group of neurons that will be
subject to repeated clustering or other analysis. The matrix can simply
be saved to disk and then reloaded using base R functions like
[`save`](https://rdrr.io/r/base/save.html) and
[`load`](https://rdrr.io/r/base/load.html).
[`sub_score_mat`](https://natverse.org/nat.nblast/reference/sub_score_mat.md)
and
[`sub_dist_mat`](https://natverse.org/nat.nblast/reference/sub_dist_mat.md)
can be used to extract a subset of scores from this raw score matrix.
For large matrices, the `bigmemory` or `ff` packages allow matrices to
be stored on disk and portions loaded into memory on demand.
[`sub_score_mat`](https://natverse.org/nat.nblast/reference/sub_score_mat.md)
and
[`sub_dist_mat`](https://natverse.org/nat.nblast/reference/sub_dist_mat.md)
work equally well for regular in-memory matrices and these disk-backed
matrices.

To give an example, for 16,129 neurons from the flycircuit.tw dataset,
the 260,144,641 comparisons took about 250 hours of compute time (half a
day on ~20 cores). When saved to disk as single precision (i.e. 4 bytes
per score) `ff` matrix they occupy just over 1Gb.

## Calculating scoring matrices

The NBLAST algorithm depends on appropriately calibrated scoring
matrices. These encapsulate the log odds ratio that a pair of segments
come from two structurally related neurons rather than two unrelated
neurons, given the observed distance and absolute dot product of the two
segments. Scoring matrices can be constructed using the
[`create_scoringmatrix`](https://natverse.org/nat.nblast/reference/create_scoringmatrix.md)
function, supplying a set of matching neurons and a set of non-matching
neurons. See the `create_scoringmatrix` documentation for links to
lower-level functions that provide finer control over construction of
the scoring matrix.

## Package Options

There is one package option `nat.nblast.defaultsmat` which is `NULL` by
default, but could for example be set to one of the scoring matrices
included with the package such as `"smat.fcwb"` or to a new
user-constructed matrix.

## References

Costa, M., Ostrovsky, A.D., Manton, J.D., Prohaska, S., and Jefferis,
G.S.X.E. (2014). NBLAST: Rapid, sensitive comparison of neuronal
structure and construction of neuron family databases. bioRxiv preprint.
[doi:10.1101/006346](https://doi.org/10.1101/006346) .

## See also

[`nblast`](https://natverse.org/nat.nblast/reference/nblast.md),
[`smat.fcwb`](https://natverse.org/nat.nblast/reference/smat.fcwb.md),
[`nhclust`](https://natverse.org/nat.nblast/reference/nhclust.md),
[`sub_dist_mat`](https://natverse.org/nat.nblast/reference/sub_dist_mat.md),
[`sub_score_mat`](https://natverse.org/nat.nblast/reference/sub_score_mat.md),
[`create_scoringmatrix`](https://natverse.org/nat.nblast/reference/create_scoringmatrix.md)

## Author

**Maintainer**: Gregory Jefferis <jefferis@gmail.com>
([ORCID](https://orcid.org/0000-0002-0587-9355))

Authors:

- James Manton <ajd.manton@googlemail.com>
  ([ORCID](https://orcid.org/0000-0001-9260-3156))
