# Convert (a subset of) a raw score matrix to a distance matrix

This function can convert a raw score matrix returned by `nblast` into a
square distance matrix or `dist` object. It can be used with file-backed
matrices as well as regular R matrices resident in memory.

## Usage

``` r
sub_dist_mat(
  neuron_names,
  scoremat = NULL,
  form = c("matrix", "dist"),
  maxneurons = NA
)
```

## Arguments

- neuron_names:

  character vector of neuron identifiers.

- scoremat:

  score matrix to use (see `sub_score_mat` for details of default).

- form:

  the type of object to return.

- maxneurons:

  set this to a sensible value to avoid loading huge (order N^2)
  distances directly into memory.

## Value

return An object of class matrix or dist (as determined by the form
argument), corresponding to a subset of the distance matrix

## Details

Note that if `neuron_names` is missing then the rownames of `scoremat`
will be used i.e. every neuron in `scoremat` will be used.

## See also

Other scoremats:
[`nhclust()`](https://natverse.org/nat.nblast/reference/nhclust.md)
