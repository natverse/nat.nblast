# Calculate scoring matrix from probability matrices for matching and non-matching sets of neurons

Calculate scoring matrix from probability matrices for matching and
non-matching sets of neurons

## Usage

``` r
calc_score_matrix(matchmat, randmat, logbase = 2, epsilon = 1e-06)
```

## Arguments

- matchmat:

  a probability matrix given by considering 'matching' neurons.

- randmat:

  a probability matrix given by considering 'non-matching' or 'random'
  neurons.

- logbase:

  the base to which the logarithm should be taken to produce the final
  scores.

- epsilon:

  a pseudocount to prevent division by zero when constructing the log
  odds ratio in the probability matrix.

## Value

A matrix with with `class=c("scoringmatrix", "table")`, with columns as
specified by `dotprodbreaks` and rows as specified by `distbreaks`,
containing scores for neuron segments with the given distance and dot
product.
