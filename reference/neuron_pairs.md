# Utility function to generate all or random pairs of neurons

Utility function to generate all or random pairs of neurons

## Usage

``` r
neuron_pairs(query, target, n = NA, ignoreSelf = TRUE)
```

## Arguments

- query, target:

  either [`neuronlist`](https://rdrr.io/pkg/nat/man/neuronlist.html)s or
  character vectors of names. If target is missing, query will be used
  as both query and target.

- n:

  number of random pairs to draw. When NA, the default, uses
  `expand.grid` to draw all pairs.

- ignoreSelf:

  Logical indicating whether to omit pairs consisting of the same neuron
  (default `TRUE`).

## Value

a data.frame with two character vector columns, query and target.

## See also

[`calc_score_matrix`](https://natverse.org/nat.nblast/reference/calc_score_matrix.md)`, `[`expand.grid`](https://rdrr.io/r/base/expand.grid.html)

## Examples

``` r
neuron_pairs(nat::kcs20, n=20)
#>                      query                  target
#> 1  FruMARCM-F000270_seg001 GadMARCM-F000122_seg001
#> 2  FruMARCM-F000270_seg001 FruMARCM-M000115_seg001
#> 3  FruMARCM-F001494_seg002 FruMARCM-M001205_seg002
#> 4  FruMARCM-F000706_seg001 FruMARCM-F001115_seg002
#> 5  GadMARCM-F000122_seg001 GadMARCM-F000050_seg001
#> 6  GadMARCM-F000476_seg001 FruMARCM-F001929_seg001
#> 7  GadMARCM-F000476_seg001 FruMARCM-F001494_seg002
#> 8  FruMARCM-F000270_seg001 FruMARCM-M001205_seg002
#> 9  FruMARCM-F000085_seg001 FruMARCM-F000188_seg001
#> 10 GadMARCM-F000122_seg001 GadMARCM-F000442_seg002
#> 11 FruMARCM-F001115_seg002 FruMARCM-F000270_seg001
#> 12 GadMARCM-F000142_seg002 ChaMARCM-F000586_seg002
#> 13 FruMARCM-F000188_seg001 FruMARCM-M001205_seg002
#> 14 GadMARCM-F000122_seg001 FruMARCM-F000706_seg001
#> 15 GadMARCM-F000050_seg001 FruMARCM-F001929_seg001
#> 16 GadMARCM-F000476_seg001 GadMARCM-F000071_seg001
#> 17 FruMARCM-M001051_seg002 FruMARCM-F000085_seg001
#> 18 FruMARCM-F001115_seg002 GadMARCM-F000050_seg001
#> 19 ChaMARCM-F000586_seg002 GadMARCM-F000122_seg001
#> 20 FruMARCM-M001339_seg001 GadMARCM-F000476_seg001
```
