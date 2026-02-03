# Return the labels of items in 1 or more groups cut from hclust object

Return the labels of items in 1 or more groups cut from hclust object

## Usage

``` r
# S3 method for class 'hclust'
subset(x, k = NULL, h = NULL, groups = NULL, ...)
```

## Arguments

- x:

  tree like object

- k:

  an integer scalar with the desired number of groups

- h:

  numeric scalar with height where the tree should be cut

- groups:

  a vector of which groups to inspect.

- ...:

  Additional parameters passed to methods

## Value

A character vector of labels of selected items

## Details

Only one of `h` and `k` should be supplied.
