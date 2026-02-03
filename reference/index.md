# Package index

## Package Overview

Provides an overview of key functions and classes

- [`nat.nblast-package`](https://natverse.org/nat.nblast/reference/nat.nblast-package.md)
  [`nat.nblast`](https://natverse.org/nat.nblast/reference/nat.nblast-package.md)
  : Neuron similarity, search and clustering tools

## Key functions

The main entry points to neuron search and clustering

- [`nblast()`](https://natverse.org/nat.nblast/reference/nblast.md) :
  Calculate similarity score for neuron morphologies
- [`nblast_allbyall()`](https://natverse.org/nat.nblast/reference/nblast_allbyall.md)
  : Wrapper function to compute all by all NBLAST scores for a set of
  neurons
- [`nhclust()`](https://natverse.org/nat.nblast/reference/nhclust.md) :
  Cluster a set of neurons

## Plotting results

Visualise pairwise neuron search or clustering results

- [`plot3d(`*`<hclust>`*`)`](https://natverse.org/nat.nblast/reference/plot3d.hclust.md)
  :

  Methods to identify and plot groups of neurons cut from an `hclust`
  object

- [`show_similarity()`](https://natverse.org/nat.nblast/reference/show_similarity.md)
  : Display two neurons with segments in the query coloured by
  similarity

- [`subset(`*`<hclust>`*`)`](https://natverse.org/nat.nblast/reference/subset.hclust.md)
  : Return the labels of items in 1 or more groups cut from hclust
  object

## Scoring Matrices

Calculate the scoring matrices that define NBLAST similarity

- [`create_scoringmatrix()`](https://natverse.org/nat.nblast/reference/create_scoringmatrix.md)
  : Create a scoring matrix given matching and non-matching sets of
  neurons
- [`calc_score_matrix()`](https://natverse.org/nat.nblast/reference/calc_score_matrix.md)
  : Calculate scoring matrix from probability matrices for matching and
  non-matching sets of neurons
- [`calc_prob_mat()`](https://natverse.org/nat.nblast/reference/calc_prob_mat.md)
  : Calculate probability matrix from distances and dot products between
  neuron segments
- [`calc_dists_dotprods()`](https://natverse.org/nat.nblast/reference/calc_dists_dotprods.md)
  : Calculate distances and dot products between two sets of neurons
- [`smat.fcwb`](https://natverse.org/nat.nblast/reference/smat.fcwb.md)
  [`smat_alpha.fcwb`](https://natverse.org/nat.nblast/reference/smat.fcwb.md)
  : Scoring matrices for neuron similarities in FCWB template brain

## Mid level functions for result matrices

Functions that you might use to manipulate matrices containing all by
all NBLAST results for use with other clustering strategies (besides
nhclust)

- [`sub_score_mat()`](https://natverse.org/nat.nblast/reference/sub_score_mat.md)
  : Return scores (or distances) for given query and target neurons
- [`sub_dist_mat()`](https://natverse.org/nat.nblast/reference/sub_dist_mat.md)
  : Convert (a subset of) a raw score matrix to a distance matrix
- [`sparse_score_mat()`](https://natverse.org/nat.nblast/reference/sparse_score_mat.md)
  : Convert a subset of a square score matrix to a sparse representation

## Low level search functions

Intended for developer use

- [`NeuriteBlast()`](https://natverse.org/nat.nblast/reference/NeuriteBlast.md)
  : Produce similarity score for neuron morphologies
- [`neuron_pairs()`](https://natverse.org/nat.nblast/reference/neuron_pairs.md)
  : Utility function to generate all or random pairs of neurons
- [`WeightedNNBasedLinesetMatching()`](https://natverse.org/nat.nblast/reference/WeightedNNBasedLinesetMatching.md)
  : Compute point & tangent vector similarity score between two linesets
- [`fctraces20`](https://natverse.org/nat.nblast/reference/fctraces20.md)
  : 20 traced Drosophila neurons from Chiang et al 2011

## Matrix utility functions

Intended for developer use

- [`diagonal()`](https://natverse.org/nat.nblast/reference/diagonal.md)
  : Extract diagonal terms from a variety of matrix types

- [`` `[`( ``*`<spam>`*`,`*`<character>`*`,`*`<character>`*`,`*`<logical>`*`)`](https://natverse.org/nat.nblast/reference/extract-methods.md)
  [`` `[`( ``*`<spam>`*`,`*`<character>`*`,`*`<character>`*`,`*`<missing>`*`)`](https://natverse.org/nat.nblast/reference/extract-methods.md)
  [`` `[`( ``*`<spam>`*`,`*`<character>`*`,`*`<missing>`*`,`*`<logical>`*`)`](https://natverse.org/nat.nblast/reference/extract-methods.md)
  [`` `[`( ``*`<spam>`*`,`*`<character>`*`,`*`<missing>`*`,`*`<missing>`*`)`](https://natverse.org/nat.nblast/reference/extract-methods.md)
  [`` `[`( ``*`<spam>`*`,`*`<missing>`*`,`*`<character>`*`,`*`<logical>`*`)`](https://natverse.org/nat.nblast/reference/extract-methods.md)
  [`` `[`( ``*`<spam>`*`,`*`<missing>`*`,`*`<character>`*`,`*`<missing>`*`)`](https://natverse.org/nat.nblast/reference/extract-methods.md)
  :

  Extract parts of a sparse `spam` matrix

- [`fill_in_sparse_score_mat()`](https://natverse.org/nat.nblast/reference/fill_in_sparse_score_mat.md)
  : Add one or more submatrices to a sparse score matrix

- [`fill_pairs_sparse_score_mat()`](https://natverse.org/nat.nblast/reference/fill_pairs_sparse_score_mat.md)
  : Add forwards, reverse and self scores for a pair of neurons to a
  sparse score matrix
