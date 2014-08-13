#' Create a scoring matrix given matching and non-matching sets of neurons
#'
#' @description Calculate a scoring matrix embodying the logarithm of the odds
#'   that a matching pair of neurite segments come from a structurally related
#'   rather than random pair of neurons. This function embodies sensible default
#'   behaviours. More control is available by using the individual functions
#'   listed in \bold{seealso}.
#'
#' @details By default \code{create_smat} will use all neurons in
#'   \code{matching_neurons} to create the matching distribution. This is
#'   appropriate if all of these neurons are of a single type. If you wish to
#'   use multiple types of neurons then you will need to specify a
#'   \code{matching_subset} to indicate which pairs of neurons are of the same
#'   type.
#'
#'   By default \code{create_smat} will use a random set of pairs from
#'   \code{non_matching_neurons} to create the null distribution. The number of
#'   random pairs will be equal to the number of matching pairs defined by
#'   \code{matching_neurons} This is appropriate if non_matching_neurons
#'   contains a large collection of neurons of different types. You may wish to
#'   set the random seed using \code{\link{set.seed}} if you want to ensure that
#'   exactly the same (pseudo-)random pairs of neurons are used in subsequent
#'   calls.
#' @param matching_neurons a \code{\link[nat]{neuronlist}} of matching neurons.
#' @param nonmatching_neurons a \code{\link[nat]{neuronlist}} of non-matching
#'   neurons.
#' @param matching_subset,non_matching_subset data.frames indicating which pairs
#'   of neurons in the two input neuron lists should be used to generate the
#'   matching and null distributions. See details for the default behaviour when
#'   \code{NULL}.
#' @param ignoreSelf a Boolean indicating whether to ignore comparisons of a
#'   neuron against itself (default \code{TRUE}).
#' @param distbreaks a vector specifying the breaks for distances in the
#'   probability matrix.
#' @param dotprodbreaks a vector specifying the breaks for dot products in the
#'   probability matrix.
#' @param logbase the base to which the logarithm should be taken to produce the
#'   final scores.
#' @param ... extra arguments to pass to \code{\link{NeuriteBlast}}.
#' @inheritParams calc_score_matrix
#'
#' @return A matrix with columns as specified by \code{dotprodbreaks} and rows
#'   as specified by \code{distbreaks}, containing log odd scores for neuron
#'   segments with the given distance and dot product.
#' @export
create_smat <- function(matching_neurons, nonmatching_neurons,
                        matching_subset=NULL, non_matching_subset=NULL,
                        ignoreSelf=TRUE, distbreaks,
                        dotprodbreaks=seq(0, 1, by=0.1), logbase=2,
                        epsilon=1e-6, ...) {

  match.dd <- calc_dists_dotprods(matching_neurons, subset=matching_subset,
                                  ignoreSelf=ignoreSelf, ...)
  # generate random set of neuron pairs of same length as the matching set
  if(is.null(non_matching_subset))
    rand.subset = neuron_pairs(nonmatching_neurons, length(match.dd))
  rand.dd <- calc_dists_dotprods(nonmatching_neurons, subset=non_matching_subset,
                                 ignoreSelf=ignoreSelf, ...)

  match.prob <- calc_prob_mat(match.dd, distbreaks=distbreaks,
                              dotprodbreaks=dotprodbreaks, ReturnCounts=FALSE)

  rand.prob <- calc_prob_mat(rand.dd, distbreaks=distbreaks,
                             dotprodbreaks=dotprodbreaks, ReturnCounts=FALSE)
  calc_score_matrix(match.prob, rand.prob, logbase=logbase, epsilon=epsilon)
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
#' @return A list, one element for for pair of neurons with a 2 column
#'   data.frame containing one column of distances and s secon of absolute dot
#'   products.
#' @importFrom plyr mlply
#' @export
calc_dists_dotprods <- function(query_neurons, target_neurons, subset=NULL, ignoreSelf=TRUE, ...) {
  if(missing(target_neurons)) target_neurons <- query_neurons
  if(is.null(subset)) {
    subset <- expand.grid(query=names(query_neurons), target=names(target_neurons),
                          stringsAsFactors=FALSE, KEEP.OUT.ATTRS = FALSE)
  } else {
    if(!is.data.frame(subset) || !all(sapply(subset, is.character)))
      stop("Subset must be a data.frame with two character columns specifying query and target neurons by name, with one row for each pair.")
  }
  if(ignoreSelf)
    subset <- subset[subset$target != subset$query, ]
  # simple function to collect dists/dot products for one pair
  collect_one_pair <- function(query, target, ...){
    cop=NeuriteBlast(query_neurons[[query]], target_neurons[target],
                     NNDistFun=data.frame, simplify=FALSE, ...)
    # comes back as list of length 1
    cop=cop[[1]]
    names(cop)=c("nndists","dps")
    cop
  }
  mlply(subset, collect_one_pair, ...)
}

#' Utility function to generate all or random pairs of neurons
#'
#' @param x neuronlist or character vector of names
#' @param n number of random pairs to draw. When NA, the default, uses
#'   \code{expand.grid} to draw all pairs.
#' @return a data.frame with two character vector columns, query and target.
#' @export
#' @seealso \code{\link{calc_score_matrix}, \link{expand.grid}}
#' @examples
#' neuron_pairs(nat::kcs20, n=20)
neuron_pairs<-function(x, n=NA){
  if(!is.character(x)) x=names(x)
  if(is.na(n)) {
    return(expand.grid(query=x, target=x, stringsAsFactors=FALSE, KEEP.OUT.ATTRS = FALSE))
  }
  q=sample.int(length(x), n, replace=T)
  t=sapply(q, function(z) sample(seq_along(x)[-z], 1))
  data.frame(query=x[q], target=x[t], stringsAsFactors = F)
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
#' @param epsilon a pseudocount to prevent division by zero when constructing
#'   the log odds ratio in the probability matrix.
#'
#' @return A matrix with columns as specified by \code{dotprodbreaks} and rows
#'   as specified by \code{distbreaks}, containing scores for neuron segments
#'   with the given distance and dot product.
#' @export
calc_score_matrix <- function(matchmat, randmat, logbase=2, epsilon=1e-6) {
  distbreaks <- attr(matchmat, "distbreaks")
  ndistbreaks <- length(distbreaks)
  dotprodbreaks <- attr(matchmat, "dotprodbreaks")
  ndpbins <- length(dotprodbreaks) - 1

  if(!isTRUE(all.equal(dim(randmat),dim(matchmat))))
    stop("Mismatch between match and mismatch dimensions.")
  if(!isTRUE(all.equal(
    distbreaks[-ndistbreaks], distbreaks[-ndistbreaks], check.attributes=FALSE)))
    stop("Mismatch between distance breaks used for match and null models.")

  log((matchmat + epsilon) / (randmat + epsilon), logbase)
}
