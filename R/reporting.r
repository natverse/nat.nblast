#' Display NBLAST hits for a neuron in 3D, given scores
#'
#' @param nblast_results a vector of NBLAST scores.
#' @param neurons a neuronlist of neurons to plot from.
#' @param threshold an optional number which, if given, prevents scores below
#'   this number from being displayed.
#' @param col the colour palette with which to plot the neurons.
#' @param ... extra parameters to pass to plot3d.
#'
#' @return A list of rgl IDs (invisibly).
#' @export
plot_hits_from_scores <- function(nblast_results, neurons, threshold=NULL, col=heat.colors, ...) {
  nblast_results <- sort(nblast_results, decreasing=TRUE)
  if(!is.null(threshold)) nblast_results <- nblast_results[nblast_results > threshold]
  plot3d(neurons[names(nblast_results)], col=col, ...)
}

