#' Display two neurons with segments in the query coloured by similarity
#'
#' By default, the query neuron will be drawn with its segments shaded from red
#' to blue, with red indicating a poor match to the target segments, and blue
#' a good match.
#'
#' @param target the neuron to compare against.
#' @param query a neuron to compare and colour.
#' @param smat a score matrix (if \code{NULL}, defaults to \code{smat.fcwb}).
#' @param cols the function to use to colour the segments (e.g. \code{\link{heat.colors}}).
#' @param col the colour with which to draw the comparison neuron.
#' @param AbsoluteScale logical indicating whether the colours should be calculated based on the minimum and maximum similarities for the neuron (\code{AbsoluteScale = FALSE}) or on the minimum and maximum possible for all neurons.
#' @param PlotVectors logical indicating whether the vectors of the \code{dotprops} representation should be plotted. If \code{FALSE}, only the points are plotted.
#' @param ... extra arguments to pass to \code{\link[rgl]{plot3d}}.
#' @return \code{show_similarity} is called for the side effect of drawing the plot; a vector of object IDs is returned.
#' @export
#' @importFrom rgl plot3d
#' @importFrom grDevices colorRampPalette
#' @examples
#' \dontrun{
#' library(nat)
#'
#' # Pull out gamma and alpha-beta neurons
#' gamma_neurons <- subset(kcs20, type=='gamma')
#' ab_neurons <- subset(kcs20, type=='ab')
#'
#' # Compare two alpha-beta neurons with similar branching, but dissimilar arborisation
#' clear3d()
#' show_similarity(ab_neurons[[1]], ab_neurons[[2]])
#'
#' # Compare an alpha-beta and a gamma neuron with some similarities and differences
#' clear3d()
#' show_similarity(ab_neurons[[1]], gamma_neurons[[3]])
#' }
show_similarity <- function(target, query, smat=NULL, cols=colorRampPalette(c('#0000FF', '#FF0000')), col='black', AbsoluteScale=FALSE, PlotVectors=TRUE, ...) {
  if(is.null(smat)) {
    smat=getOption("nat.nblast.defaultsmat")
    if(is.null(smat)) smat=smat.fcwb
  }
  if(is.character(smat)) smat=get(smat)

  res <- WeightedNNBasedLinesetMatching.dotprops(target, query, NNDistFun=lodsby2dhist, smat=smat, Return='elements')

  if(AbsoluteScale) {
    smat.unique.ordered <- unique(smat)[order(unique(smat))]
    coltable <- rev(cols(length(smat.unique.ordered)))
    segcols <- coltable[sapply(res, function(x) which(x == smat.unique.ordered))]
  } else {
    res.unique.ordered <- unique(res)[order(unique(res))]
    coltable <- rev(cols(length(res.unique.ordered)))
    segcols <- coltable[sapply(res, function(x) which(x == res.unique.ordered))]
  }

  if(PlotVectors) {
    # We need to duplicate each colour as we are drawing line segments, not points
    segcols <- c(sapply(segcols, function(x) c(x,x)))
    plot3d(target, col=col, PlotVectors=TRUE, PlotPoints=FALSE, ...)
    plot3d(query, col=segcols, PlotVectors=TRUE, PlotPoints=FALSE, ...)
  } else {
    plot3d(target, col=col, PlotVectors=FALSE, PlotPoints=TRUE, ...)
    plot3d(query, col=segcols, PlotVectors=FALSE, PlotPoints=TRUE, ...)
  }
}
