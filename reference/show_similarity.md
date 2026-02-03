# Display two neurons with segments in the query coloured by similarity

By default, the query neuron will be drawn with its segments shaded from
red to blue, with red indicating a poor match to the target segments,
and blue a good match.

## Usage

``` r
show_similarity(
  query,
  target,
  smat = NULL,
  cols = colorRampPalette(c("red", "yellow", "cyan", "navy")),
  col = "black",
  AbsoluteScale = FALSE,
  PlotVectors = TRUE,
  ...
)
```

## Arguments

- query:

  a neuron to compare and colour.

- target:

  the neuron to compare against.

- smat:

  a score matrix (if `NULL`, defaults to `smat.fcwb`).

- cols:

  the function to use to colour the segments (e.g.
  [`heat.colors`](https://rdrr.io/r/grDevices/palettes.html)).

- col:

  the colour with which to draw the target neuron.

- AbsoluteScale:

  logical indicating whether the colours should be calculated based on
  the minimum and maximum similarities for the neuron
  (`AbsoluteScale = FALSE`) or on the minimum and maximum possible for
  all neurons.

- PlotVectors:

  logical indicating whether the vectors of the `dotprops`
  representation should be plotted. If `FALSE`, only the points are
  plotted.

- ...:

  extra arguments to pass to
  [`plot3d`](https://dmurdoch.github.io/rgl/dev/reference/plot3d.html).

## Value

`show_similarity` is called for the side effect of drawing the plot; a
vector of object IDs is returned.

## See also

The low level function
[`WeightedNNBasedLinesetMatching`](https://natverse.org/nat.nblast/reference/WeightedNNBasedLinesetMatching.md)
is used to retrieve the scores.

## Examples

``` r
if (FALSE) { # \dontrun{
library(nat)

# Pull out gamma and alpha-beta neurons
gamma_neurons <- subset(kcs20, type=='gamma')
ab_neurons <- subset(kcs20, type=='ab')

# Compare two alpha-beta neurons with similar branching, but dissimilar arborisation
clear3d()
show_similarity(ab_neurons[[1]], ab_neurons[[2]])

# Compare an alpha-beta and a gamma neuron with some similarities and differences
clear3d()
show_similarity(ab_neurons[[1]], gamma_neurons[[3]])
} # }
```
