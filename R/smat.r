#' Create a scoring matrix given matching and non-matching sets of neurons
#'
#' @param matching_neurons a \code{\link[nat]{neuronlist}} of matching neurons.
#' @param nonmatching_neurons a \code{\link[nat]{neuronlist}} of non-matching
#'   neurons.
#' @param ignoreSelf a Boolean indicating whether to ignore comparisons of a
#'   neuron against itself (default \code{TRUE}).
#' @param distbreaks a vector specifying the breaks for distances in the
#'   probability matrix.
#' @param dotprodbreaks a vector specifying the breaks for dot products in the
#'   probability matrix.
#' @param logbase the base to which the logarithm should be taken to produce the
#'   final scores.
#' @param fudgefac an arbitrary, small 'fudge factor' to prevent division by
#'   zero.
#' @param ... extra arguments to pass to \code{\link{NeuriteBlast}}.
#'
#' @return A matrix with columns as specified by \code{dotprodbreaks} and rows
#'   as specified by \code{distbreaks}, containing log odd scores for neuron
#'   segments with the given distance and dot product.
#' @export
create_smat <- function(matching_neurons, nonmatching_neurons, ignoreSelf=TRUE,
                        distbreaks, dotprodbreaks=seq(0, 1, by=0.1), logbase=2,
                        fudgefac=1e-6, ...) {
  matching_dists_dotprods <- calc_dists_dotprods(matching_neurons, matching_neurons,
                                                 ignoreSelf=ignoreSelf, ...)
  nonmatching_dists_dotprods <- calc_dists_dotprods(nonmatching_neurons,
                                                    nonmatching_neurons,
                                                    ignoreSelf=ignoreSelf, ...)
  matching_prob_mat <- calc_prob_mat(matching_dists_dotprods,
                                     distbreaks=distbreaks,
                                     dotprodbreaks=dotprodbreaks,
                                     ReturnCounts=FALSE)
  nonmatching_prob_mat <- calc_prob_mat(nonmatching_dists_dotprods,
                                        distbreaks=distbreaks,
                                        dotprodbreaks=dotprodbreaks,
                                        ReturnCounts=FALSE)
  calc_score_matrix(matching_prob_mat, nonmatching_prob_mat,
                                 logbase=logbase, fudgefac=fudgefac)
}


#' Calculate distances and dot products between two sets of neurons
#'
#' @details Distances and dot products are the raw inputs for constructing
#'   scoring matrices for the nblast search algorithm.
#' @param query_neurons a \code{\link[nat]{neuronlist}} to use for calculating
#'   distances and dot products.
#' @param target_neurons a further \code{\link[nat]{neuronlist}} to use for
#'   calculating distances and dot products.
#' @param subset a \code{\link{data.frame}} specifying which neurons in
#'   \code{query_neurons} and \code{target_neurons} should be compared, with
#'   columns specifying query and target neurons by name, with one row for each
#'   pair. If unspecified, this defaults to an all-by-all comparison.
#' @param ignoreSelf a Boolean indicating whether to ignore comparisons of a
#'   neuron against itself (default \code{TRUE}).
#' @param ... extra arguments to pass to \code{\link{NeuriteBlast}}.
#'
#' @return A list, for each query neuron, of vectors containing distances and
#'   dot products for each target neuron.
#' @importFrom plyr mlply
#' @export
calc_dists_dotprods <- function(query_neurons, target_neurons, subset, ignoreSelf=TRUE, ...) {
  if(missing(target_neurons)) target_neurons <- query_neurons
  if(missing(subset)) {
    subset <- expand.grid(query=names(query_neurons), target=names(target_neurons), stringsAsFactors=FALSE)
  } else {
    if(!is.data.frame(subset) || !all(sapply(subset, is.character)))
      stop("Subset must be a data.frame with two character columns specifying query and target neurons by name, with one row for each pair.")
  }
  if(ignoreSelf)
    subset <- subset[subset$target != subset$query, ]
  collect_one_pair <- function(query, target, ...)
    NeuriteBlast(query_neurons[[query]], target_neurons[target], NNDistFun=list, ...)
  mlply(subset, collect_one_pair, ...)
}


#' Calculate probability matrix from distances and dot products between neuron
#' segments
#'
#' @param nndists a list of nearest-neighbour distances or a list of both
#'   nearest-neighbour distances and dot products.
#' @param dotprods a list of dot products.
#' @param distbreaks a vector specifying the breaks for distances in the
#'   probability matrix.
#' @param dotprodbreaks a vector specifying the breaks for dot products in the
#'   probability matrix.
#' @param ReturnCounts a Boolean indicating that counts should be returned
#'   instead of the default probabilities.
#'
#' @return A matrix with columns as specified by \code{dotprodbreaks} and rows
#'   as specified by \code{distbreaks}, containing probabilities (for default
#'   value of \code{ReturnCounts=TRUE}) or counts (if \code{ReturnCounts=TRUE})
#'   for finding neuron segments with the given distance and dot product.
#' @export
calc_prob_mat <- function(nndists, dotprods, distbreaks, dotprodbreaks=seq(0, 1, by=0.1), ReturnCounts=FALSE) {
  if(missing(distbreaks)) distbreaks <- c(0, 0.75, 1.5, 2, 2.5, 3, 3.5, 4, 5, 6, 7, 8, 9, 10, 12, 14, 16, 20, 25, 30, 40, 500)
  if(missing(dotprods)) {
    if(is.list(nndists) && length(nndists[[1]])==2) {
      dotprods <- sapply(nndists, "[[", 2)
      nndists <- sapply(nndists, "[[", 1)
    } else
      stop("Dot products missing and unable to parse nearest-neighbour distances as combined list.")
  }

  countmat=table(cut(unlist(nndists), br=distbreaks), cut(unlist(dotprods), dotprodbreaks))
  attr(countmat,"distbreaks") <- distbreaks
  attr(countmat,"dotprodbreaks") <- dotprodbreaks
  if(!ReturnCounts) countmat / sum(countmat)
  else countmat
}


#' Calculate score matrix from probability matrices for matching and
#' non-matching sets of neurons
#'
#' @param matchmat a probability matrix given by considering 'matching' neurons.
#' @param randmat a probability matrix given by considering 'non-matching' or
#'   'random' neurons.
#' @param logbase the base to which the logarithm should be taken to produce the
#'   final scores.
#' @param fudgefac an arbitrary, small 'fudge factor' to prevent division by
#'   zero.
#'
#' @return A matrix with columns as specified by \code{dotprodbreaks} and rows
#'   as specified by \code{distbreaks}, containing scores for neuron segments
#'   with the given distance and dot product.
#' @export
calc_score_matrix <- function(matchmat, randmat, logbase=2, fudgefac=1e-6) {
  distbreaks <- attr(matchmat, "distbreaks")
  ndistbreaks <- length(distbreaks)
  dotprodbreaks <- attr(matchmat, "dotprodbreaks")
  ndpbins <- length(dotprodbreaks) - 1

  if(!isTRUE(all.equal(dim(randmat),dim(matchmat))))
    stop("Mismatch between match and mismatch dimensions.")
  if(!isTRUE(all.equal(
    distbreaks[-ndistbreaks], distbreaks[-ndistbreaks], check.attributes=FALSE)))
    stop("Mismatch between distance breaks used for match and null models.")

  log((matchmat + fudgefac) / (randmat + fudgefac), logbase)
}
