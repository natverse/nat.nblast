# Return scores (or distances) for given query and target neurons

Scores can either be returned as raw numbers, normalised such that a
self-hit has score 1, or as the average of the normalised scores in both
the forwards & reverse directions (i.e.
`|query->target| + |target->query| / 2`). Distances are returned as
either `1 - normscore` in the forwards direction, or as
`1 - normscorebar`, where `normscorebar` is `normscore` averaged across
both directions.

## Usage

``` r
sub_score_mat(
  query,
  target,
  scoremat = NULL,
  distance = FALSE,
  normalisation = c("raw", "normalised", "mean")
)
```

## Arguments

- query, target:

  character vectors of neuron identifiers.

- scoremat:

  a matrix, ff matrix, `bigmatrix` or a character vector specifying the
  name of an ff matrix containing the all by all score matrix.

- distance:

  logical indicating whether to return distances or scores.

- normalisation:

  the type of normalisation procedure that should be carried out,
  selected from `'raw'`, `'normalised'` or `'mean'` (i.e. the average of
  normalised scores in both directions). If `distance=TRUE` then this
  cannot be raw.

## See also

[`sub_dist_mat`](https://natverse.org/nat.nblast/reference/sub_dist_mat.md)
