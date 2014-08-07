#' Display NBLAST hits for a neuron in 3D, given scores
#'
#' @param nblast_results a vector of NBLAST scores.
#' @param neurons a neuronlist of neurons to plot from.
#' @param threshold an optional number which, if given, prevents scores below
#'   this number from being displayed.
#' @param number an optional number of neurons to plot.
#' @param col the colour palette with which to plot the neurons.
#' @param ... extra parameters to pass to plot3d.
#'
#' @return A list of rgl IDs (invisibly).
#' @export
plot_hits_from_scores <- function(nblast_results, neurons, threshold=NULL, number=NULL, col=heat.colors, ...) {
  nblast_results <- sort(nblast_results, decreasing=TRUE)
  if(!is.null(threshold)) nblast_results <- nblast_results[nblast_results > threshold]
  if(!is.null(number)) nblast_results <- nblast_results[1:min(number, length(nblast_results))]
  plot3d(neurons[names(nblast_results)], col=col, ...)
}


#' Calculate and display NBLAST hits for a neuron in 3D
#'
#' @param query the query neuron.
#' @param target a \code{neuronlist} to compare against.
#' @param threshold an optional number which, if given, prevents scores below
#'   this number from being displayed.
#' @param number an optional number of neurons to plot.
#' @param col the colour palette with which to plot the neurons.
#' @param ... extra parameters to pass to plot3d.
#'
#' @return A list of rgl IDs (invisibly).
#' @importFrom digest digest
#' @export
plot_hits <- function(query, target, threshold=NULL, number=NULL, col=heat.colors, ...) {
  if(!all(sapply(c('cached_nblast_results', 'cached_query_digest', 'cached_target_digest'), exists, reporting.env)) || (get('cached_query_digest', reporting.env) != digest(query)) || (get('cached_target_digest', reporting.env) != digest(target))) {
    nblast_results <- nblast(query, target)
    assign('cached_nblast_results', nblast_results, reporting.env)
    assign('cached_query_digest', digest(query), reporting.env)
    assign('cached_target_digest', digest(target), reporting.env)
    message("Calculating new NBLAST results...")
  } else {
    nblast_results <- get('cached_nblast_results', reporting.env)
    message("Using cached NBLAST results...")
  }
  plot3d(query, col='black', lwd=2)
  plot_hits_from_scores(nblast_results, target, threshold=threshold, col=col, ...=...)
}


#' Display NBLAST hits for neurons in 3D, given scores
#'
#' @param nblast_results a matrix of NBLAST scores.
#' @param neurons a neuronlist of neurons to plot from.
#' @param threshold an optional number which, if given, prevents scores below
#'   this number from being displayed.
#' @param number an optional number of neurons to plot.
#' @param col the colour palette with which to plot the neurons.
#' @param ... extra parameters to pass to plot3d.
#'
#' @return \code{NULL}.
#' @export
plot_hits_from_matrix <- function(nblast_results, neurons, threshold=NULL, number=NULL, col=heat.colors, ...) {
  queries <- colnames(nblast_results)
  targets <- rownames(nblast_results)
  for(query in queries) {
    plot3d(neurons[query], col='black', lwd=2)
    scores <- nblast_results[, query]
    plot_hits_from_scores(scores, neurons=neurons, threshold=threshold, number=number, col=col, ...=...)
  }
}
