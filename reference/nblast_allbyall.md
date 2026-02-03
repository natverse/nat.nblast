# Wrapper function to compute all by all NBLAST scores for a set of neurons

Calls `nblast` to compute the actual scores. Can accept either a
[`neuronlist`](https://rdrr.io/pkg/nat/man/neuronlist.html) or neuron
names as a character vector. This is a thin wrapper around `nblast` and
its main advantage is the option of "mean" normalisation for forward and
reverse scores, which is the most sensible input to give to a clustering
algorithm as well as the choice of returning a distance matrix.

## Usage

``` r
nblast_allbyall(x, ...)

# S3 method for class 'character'
nblast_allbyall(x, smat = NULL, db = getOption("nat.default.neuronlist"), ...)

# S3 method for class 'neuronlist'
nblast_allbyall(
  x,
  smat = NULL,
  distance = FALSE,
  normalisation = c("raw", "normalised", "mean"),
  ...
)
```

## Arguments

- x:

  Input neurons
  ([`neuronlist`](https://rdrr.io/pkg/nat/man/neuronlist.html) or
  character vector)

- ...:

  Additional arguments for methods or `nblast`

- smat:

  the scoring matrix to use (see details of
  [`nblast`](https://natverse.org/nat.nblast/reference/nblast.md) for
  meaning of default `NULL` value)

- db:

  A [`neuronlist`](https://rdrr.io/pkg/nat/man/neuronlist.html) or a
  character vector naming one. Defaults to value of
  `options("nat.default.neuronlist")`

- distance:

  logical indicating whether to return distances or scores.

- normalisation:

  the type of normalisation procedure that should be carried out,
  selected from `'raw'`, `'normalised'` or `'mean'` (i.e. the average of
  normalised scores in both directions). If `distance=TRUE` then this
  cannot be raw.

## Details

Note that `nat` already provides a function
[`nhclust`](https://natverse.org/nat.nblast/reference/nhclust.md) for
clustering, which is a wrapper for R's `hclust` function. `nhclust`
actually expects **raw** scores as input.

## TODO

It would be a good idea in the future to implement a parallel version of
this function.

## See also

[`nblast`](https://natverse.org/nat.nblast/reference/nblast.md)`, `[`sub_score_mat`](https://natverse.org/nat.nblast/reference/sub_score_mat.md)`, `[`nhclust`](https://natverse.org/nat.nblast/reference/nhclust.md)

## Examples

``` r
library(nat)
kcs20.scoremat=nblast_allbyall(kcs20)
kcs20.hclust=nhclust(scoremat=kcs20.scoremat)
#> The "ward" method has been renamed to "ward.D"; note new "ward.D2"
plot(kcs20.hclust)
```
