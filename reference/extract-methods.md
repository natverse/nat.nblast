# Extract parts of a sparse `spam` matrix

Extract parts of a sparse `spam` matrix

## Usage

``` r
# S4 method for class 'spam,character,character,logical'
x[i, j, ..., drop = TRUE]

# S4 method for class 'spam,character,character,missing'
x[i, j, ..., drop = TRUE]

# S4 method for class 'spam,character,missing,logical'
x[i, j, ..., drop = TRUE]

# S4 method for class 'spam,character,missing,missing'
x[i, j, ..., drop = TRUE]

# S4 method for class 'spam,missing,character,logical'
x[i, j, ..., drop = TRUE]

# S4 method for class 'spam,missing,character,missing'
x[i, j, ..., drop = TRUE]
```

## Arguments

- x:

  object to extract from.

- i:

  row identifiers.

- j:

  column identifiers.

- ...:

  additional arguments.

- drop:

  logical indicating that dimensions should be dropped.
