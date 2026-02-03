# Compute point & tangent vector similarity score between two linesets

`WeightedNNBasedLinesetMatching` is a low level function that is called
by [`nblast`](https://natverse.org/nat.nblast/reference/nblast.md). Most
end users will not usually need to call it directly. It does allow the
results of an NBLAST comparison to be inspected in further detail (see
examples).

## Usage

``` r
WeightedNNBasedLinesetMatching(target, query, ...)

# S3 method for class 'dotprops'
WeightedNNBasedLinesetMatching(target, query, UseAlpha = FALSE, ...)

# S3 method for class 'neuron'
WeightedNNBasedLinesetMatching(
  target,
  query,
  UseAlpha = FALSE,
  OnlyClosestPoints = FALSE,
  ...
)
```

## Arguments

- target, query:

  [`dotprops`](https://rdrr.io/pkg/nat/man/dotprops.html) or
  [`neuron`](https://rdrr.io/pkg/nat/man/neuron.html) objects to compare
  (must be of the same class)

- ...:

  extra arguments to pass to the distance function.

- UseAlpha:

  Whether to scale dot product of tangent vectors (default=F)

- OnlyClosestPoints:

  Whether to restrict searches to the closest points in the target
  (default FALSE, only implemented for `dotprops`).

## Value

Value of `NNDistFun` passed to `WeightedNNBasedLinesetMatching`

## Details

`WeightedNNBasedLinesetMatching` will work with 2 objects of class
`dotprops` or `neuron`. The code to calculate scores directly for
`neuron` objects gives broadly comparable scores to that for `dotprops`
objects, but has been lightly tested. Furthermore only objects in
`dotprops` form were used in the construction of the scoring matrices
distributed in this package. It is therefore recommended to convert
`neuron` objects to `dotprops` objects using the
[`dotprops`](https://rdrr.io/pkg/nat/man/dotprops.html) function.

`UseAlpha` determines whether the alpha values `(eig1-eig2)/sum(eig1:3)`
are passed on to WeightedNNBasedLinesetMatching. These will be used to
scale the dot products of the direction vectors for nearest neighbour
pairs.

## See also

[`dotprops`](https://rdrr.io/pkg/nat/man/dotprops.html)

## Examples

``` r
# Retrieve per segment distances / absolute dot products
segvals=WeightedNNBasedLinesetMatching(kcs20[[1]], kcs20[[2]], NNDistFun=list)
names(segvals)=c("dist", "adotprod")
pairs(segvals)
```
