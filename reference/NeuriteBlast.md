# Produce similarity score for neuron morphologies

A low-level entry point to the NBLAST algorithm that compares the
morphology of a neuron with those of a list of other neurons. For most
use cases, one would probably wish to use
[`nblast`](https://natverse.org/nat.nblast/reference/nblast.md) instead.

## Usage

``` r
NeuriteBlast(
  query,
  target,
  targetBinds = NULL,
  normalised = FALSE,
  OmitFailures = NA,
  simplify = TRUE,
  ...
)
```

## Arguments

- query:

  either a single query neuron or a
  [`neuronlist`](https://rdrr.io/pkg/nat/man/neuronlist.html)

- target:

  a `neuronlist` to compare neuron against.

- targetBinds:

  numeric indices or names with which to subset `target`.

- normalised:

  whether to divide scores by the self-match score of the query

- OmitFailures:

  Whether to omit neurons for which `FUN` gives an error. The default
  value (`NA`) will result in `nblast` stopping with an error message
  the moment there is an error. For other values, see details.

- simplify:

  whether to simplify the scores from a list to a vector. `TRUE` by
  default. The only time you might want to set this false is if you are
  collecting something other than simple scores from the search
  function. See [`simplify2array`](https://rdrr.io/r/base/lapply.html)
  for further details.

- ...:

  extra arguments to pass to the distance function.

## Value

Named list of similarity scores.

## Details

For detailed description of the `OmitFailures` argument, see the details
section of
[`nblast`](https://natverse.org/nat.nblast/reference/nblast.md).

## See also

[`WeightedNNBasedLinesetMatching`](https://natverse.org/nat.nblast/reference/WeightedNNBasedLinesetMatching.md)
