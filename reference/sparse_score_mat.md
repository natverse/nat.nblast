# Convert a subset of a square score matrix to a sparse representation

This can be useful for storing raw forwards and reverse NBLAST scores
for a set of neurons without having to store all the uncomputed elements
in the full score matrix.

## Usage

``` r
sparse_score_mat(neuron_names, dense_matrix)
```

## Arguments

- neuron_names:

  a character vector of neuron names to save scores for.

- dense_matrix:

  the original, dense version of the full score matrix.

## Value

A spare matrix, in compressed, column-oriented form, as an R object
inheriting from both
[`CsparseMatrix-class`](https://rdrr.io/pkg/Matrix/man/CsparseMatrix-class.html)
and
[`generalMatrix-class`](https://rdrr.io/pkg/Matrix/man/generalMatrix-class.html).

## See also

fill_in_sparse_score_mat

## Examples

``` r
data(kcs20, package = "nat")
scores=nblast_allbyall(kcs20)
scores.3.sparse=sparse_score_mat(names(kcs20)[3], scores)
scores.3.sparse
#>  [1] 3234.54125 -935.33772 3382.60124 -515.53376  125.68989  244.92594
#>  [7] 4259.57193  480.41458 2048.01759 2068.51808 1278.68688 1359.49507
#> [13]  -15.28677 3403.75462 -313.24728   73.74186 3277.87783 3122.69555
#> [19] 2656.70773 1487.45673  301.63902  360.64137   23.53454 2849.13647
#> [25]  309.92651 4043.17656 1962.12743 4202.62578   91.73056 4567.08113
#> [31] 1259.18024 4703.75189 1075.50543 4726.53035   62.48739 4760.69804
#> [37] 2889.29188 4965.70417 -410.27503 5159.32108 -377.93391 5204.87800
#> [43] 2751.04667 5227.65646 2847.10004 5204.87800 1635.40240 5478.21951
#> [49] 1731.55066 5512.38720 -514.09460 5637.66873 -204.79399 5432.66259
#> [55] -393.66139 5762.95025 1968.02090 5580.72258
#> Class 'spam' (32-bit)
# can also add additional submatrices
fill_in_sparse_score_mat(scores.3.sparse,scores[3:6,3:4])
#>  [1] 3234.54125 -935.33772 3382.60124 -515.53376  125.68989  244.92594
#>  [7] 4259.57193  480.41458 2048.01759 2068.51808 1278.68688 1359.49507
#> [13]  -15.28677 3403.75462 -313.24728   73.74186 3277.87783 3122.69555
#> [19] 2656.70773 1487.45673  301.63902  360.64137   23.53454 2849.13647
#> [25]  309.92651 4043.17656 1962.12743  254.75164 4202.62578   91.73056
#> [31] -520.25810 4567.08113 1259.18024 4703.75189 1075.50543 4726.53035
#> [37]   62.48739 4760.69804 2889.29188 4965.70417 -410.27503 5159.32108
#> [43] -377.93391 5204.87800 2751.04667 5227.65646 2847.10004 5204.87800
#> [49] 1635.40240 5478.21951 1731.55066 5512.38720 -514.09460 5637.66873
#> [55] -204.79399 5432.66259 -393.66139 5762.95025 1968.02090 5580.72258
#> Class 'spam' (32-bit)
```
