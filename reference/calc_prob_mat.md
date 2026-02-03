# Calculate probability matrix from distances and dot products between neuron segments

Calculate probability matrix from distances and dot products between
neuron segments

## Usage

``` r
calc_prob_mat(
  nndists,
  dotprods,
  distbreaks,
  dotprodbreaks = seq(0, 1, by = 0.1),
  ReturnCounts = FALSE
)
```

## Arguments

- nndists:

  a list of nearest-neighbour distances or a list of both
  nearest-neighbour distances and dot products.

- dotprods:

  a list of dot products.

- distbreaks:

  a vector specifying the breaks for distances in the probability
  matrix.

- dotprodbreaks:

  a vector specifying the breaks for dot products in the probability
  matrix.

- ReturnCounts:

  a Boolean indicating that counts should be returned instead of the
  default probabilities.

## Value

A matrix with columns as specified by `dotprodbreaks` and rows as
specified by `distbreaks`, containing probabilities (for default value
of `ReturnCounts=TRUE`) or counts (if `ReturnCounts=TRUE`) for finding
neuron segments with the given distance and dot product.
