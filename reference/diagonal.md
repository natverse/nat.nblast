# Extract diagonal terms from a variety of matrix types

Extract diagonal terms from a variety of matrix types

## Usage

``` r
diagonal(x, indices = NULL)

# Default S3 method
diagonal(x, indices = NULL)
```

## Arguments

- x:

  A square matrix

- indices:

  specifies a subset of the diagonal using a character vector of names,
  a logical vector or integer indices. The default (`NULL`) implies all
  elements.

## Value

a named vector containing the diagonal elements.

## Details

Insists that input matrix is square. Uses the `'diagonal'` attribute
when available and has specialised handling of `ff`, `big.matrix`,
`dgCMatrix` matrices. Does not check that row and column names are
identical for those matrix classes (unlike the base
[`diag`](https://rdrr.io/r/base/diag.html) function, but always uses
rownames.

## Examples

``` r
m=fill_in_sparse_score_mat(letters[1:5])
diagonal(m)
#> a b c d e 
#> 0 0 0 0 0 
```
