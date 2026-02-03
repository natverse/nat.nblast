# Add one or more submatrices to a sparse score matrix

Add one or more submatrices to a sparse score matrix

## Usage

``` r
fill_in_sparse_score_mat(sparse_matrix, ..., diag = NULL)
```

## Arguments

- sparse_matrix:

  either an existing (square) sparse matrix or a character vector of
  names that will be used to define an empty sparse matrix.

- ...:

  Additional matrices to insert into `sparse_matrix`. Row and column
  names must have matches in `sparse_matrix`.

- diag:

  optional full diagonal for sparse matrix i.e. self-match scores.

## See also

sparse_score_mat
