# Calculate distances and dot products between two sets of neurons

Calculate distances and dot products between two sets of neurons

## Usage

``` r
calc_dists_dotprods(
  query_neurons,
  target_neurons,
  subset = NULL,
  ignoreSelf = TRUE,
  ...
)
```

## Arguments

- query_neurons:

  a [`neuronlist`](https://rdrr.io/pkg/nat/man/neuronlist.html) to use
  for calculating distances and dot products.

- target_neurons:

  a further [`neuronlist`](https://rdrr.io/pkg/nat/man/neuronlist.html)
  to use for calculating distances and dot products.

- subset:

  a [`data.frame`](https://rdrr.io/r/base/data.frame.html) specifying
  which neurons in `query_neurons` and `target_neurons` should be
  compared, with columns specifying query and target neurons by name,
  with one row for each pair. If unspecified, this defaults to an
  all-by-all comparison.

- ignoreSelf:

  a Boolean indicating whether to ignore comparisons of a neuron against
  itself (default `TRUE`).

- ...:

  extra arguments to pass to
  [`NeuriteBlast`](https://natverse.org/nat.nblast/reference/NeuriteBlast.md).

## Value

A list, one element for for pair of neurons with a 2 column data.frame
containing one column of distances and another of absolute dot products.

## Details

Distances and dot products are the raw inputs for constructing scoring
matrices for the NBLAST search algorithm.
