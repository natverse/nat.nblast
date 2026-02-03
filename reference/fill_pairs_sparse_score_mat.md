# Add forwards, reverse and self scores for a pair of neurons to a sparse score matrix

Add forwards, reverse and self scores for a pair of neurons to a sparse
score matrix

## Usage

``` r
fill_pairs_sparse_score_mat(
  sparse_matrix,
  n1,
  n2,
  dense_matrix,
  reverse = TRUE,
  self = TRUE,
  reverse_self = (reverse && self)
)
```

## Arguments

- sparse_matrix:

  the sparse matrix to fill in.

- n1:

  the name of the query neuron.

- n2:

  the name of the target neuron.

- dense_matrix:

  the score matrix from which to extract scores.

- reverse:

  logical indicating that the reverse score should also be filled in
  (default `TRUE`).

- self:

  logical indicating that the self-score of the query should also be
  filled in (used for normalised scores; default `TRUE`).

- reverse_self:

  logical indicating that the self-score of the target should also be
  filled in (used for mean scores; default `TRUE`).

## Value

A sparse matrix (of class
[`spam`](https://www.math.uzh.ch/pages/spam/reference/spam.html)) with
the specified score entries filled.
