#' Return scores (or distances) for given query and target neurons
#'
#' Scores can either be returned as raw numbers, normalised such that a self-hit
#' has score 1, or as the average of the normalised scores in both the forwards
#' & reverse directions (i.e. \code{|query->target| + |target->query| / 2}).
#' Distances are returned as either \code{1 - normscore} in the forwards
#' direction, or as \code{1 - normscorebar}, where \code{normscorebar} is
#' \code{normscore} averaged across both directions.
#' @param query,target character vectors of neuron identifiers.
#' @param scoremat a matrix, ff matrix, bigmatrix or a character vector
#'   specifiying the name of an ff matrix containing the all by all score
#'   matrix.
#' @param distance logical indicating whether to return distances or scores.
#' @param normalisation the type of normalisation procedure that should be
#'   carried out, selected from  \code{'raw'}, \code{'normalised'} or
#'   \code{'mean'} (i.e. the average of normalised scores in both directions).
#'   If \code{distance=TRUE} then this cannot be raw.
#' @export
#' @seealso \code{\link{sub_dist_mat}}
sub_score_mat <- function(query, target, scoremat=NULL, distance=FALSE, normalisation=c('raw', 'normalised', 'mean')) {
  # Check arguments
  normalisation <- match.arg(normalisation)
  if(distance && normalisation == 'raw') stop("Raw scores are always similarity scores.")
  if(is.null(scoremat)) stop("A score matrix must be provided!")
  if(!identical(length(dim(scoremat)),2L)) stop("scoremat must be a matrix!")

  # Check for missing query and target neurons
  available_neuron_names <- rownames(scoremat)
  if(missing(target)) target <- available_neuron_names
  else {
    target_missing <- setdiff(target, available_neuron_names)
    if(length(target_missing) > 0) {
      warning("Dropping ", length(target_missing), " target neurons.")
      target <- intersect(target, available_neuron_names)
    }
  }
  if(missing(query)) query <- rownames(scoremat)
  else {
    query_missing <- setdiff(query, available_neuron_names)
    if(length(query_missing) > 0) {
      warning("Dropping ", length(query_missing), " query neurons.")
      query <- intersect(query, available_neuron_names)
    }
  }

  # Subsetting large matrices by name is slow, so pre-calculate indices
  qidxs <- match(query, available_neuron_names)
  tidxs <- match(target, available_neuron_names)
  fwd_scores <- scoremat[tidxs, qidxs, drop=FALSE]

  # Check if we have been asked to provide a square matrix
  square_mat <- length(qidxs) == length(tidxs) && all(qidxs==tidxs)

  x <- if(normalisation %in% c('mean', 'normalised')) {
    # Normalise forward scores
    self_matches <- if(square_mat && !inherits(scoremat, "dgCMatrix")) diag(fwd_scores) else diagonal(scoremat, qidxs)
    fwd_scores <- scale(fwd_scores, center=FALSE, scale=self_matches)

    if(normalisation == 'mean') {
      if(square_mat) {
        (fwd_scores + t(fwd_scores)) / 2
      } else {
        rev_scores <- scoremat[qidxs, tidxs, drop=FALSE]
        self_matches <- diagonal(scoremat, tidxs)
        rev_scores <- scale(rev_scores, center=FALSE, scale=self_matches)
        (fwd_scores + t(rev_scores)) / 2
      }
    } else {
      fwd_scores
    }
  } else {
    fwd_scores
  }

  # Drop dimensions in the standard R way (including names, etc.)
  if(nrow(x) == 1 || ncol(x) == 1) x <- x[seq_len(nrow(x)), seq_len(ncol(x))]
  if(distance) 1-x else x
}


#' Convert (a subset of) a raw score matrix to a distance matrix
#'
#' @description This function can convert a raw score matrix returned by nblast
#'   into a square distance matrix or \code{dist} object. It can be used with
#'   file-backed matrices as well as regular R matrices resident in memory.
#'
#' @details Note that if \code{neuron_names} is missing then the rownames of
#'   \code{scoremat} will be used i.e. all neuron in scoremat will be used.
#'
#' @inheritParams nhclust
#' @param form the type of object to return.
#' @param maxneurons set this to a sensible value to avoid loading huge (order
#'   N^2) distances directly into memory.
#' @return return An object of class matrix or dist (as determined by the form
#'   argument), corresponding to a subset of the distance matrix
#' @export
#' @family scoremats
sub_dist_mat <- function(neuron_names, scoremat=NULL, form=c('matrix', 'dist'), maxneurons=NA){
  form <- match.arg(form)
  if(missing(neuron_names)){
    if(nrow(scoremat)!=ncol(scoremat))
      stop("scoremat must be square if neuron_names is missing")
    neuron_names=rownames(scoremat)
  }
  if(!is.na(maxneurons) && length(neuron_names) > maxneurons) {
    stop("Too many neurons! Use maxneurons to override if you're sure.")
  }
  d <- sub_score_mat(neuron_names, neuron_names, scoremat=scoremat, distance=TRUE, normalisation='mean')
  if(form=='matrix') d
  else as.dist(d)
}


# Utility function to extract diagonal terms from matrices
# uses the 'diagonal' attribute when available
diagonal <- function(x, indices=NULL) {
  if(!isTRUE(nrow(x)==ncol(x))) stop("x is not a square matrix!")

  if(is.character(indices)) indices=match(indices,rownames(x))
  if(!is.null(xdiag<-attr(x,'diagonal'))){
    return(if(is.null(indices)) xdiag else xdiag[indices])
  }

  if(is.logical(indices)) indices=which(indices)

  if(inherits(x,"ff")){
    # convert array indices to vector indices
    if(is.null(indices)) indices=seq.int(nrow(x))
    vidxs=ff::arrayIndex2vectorIndex(cbind(indices,indices),dim=dim(x))
    x[vidxs]
  } else if(inherits(x,"big.matrix")) {
    if(is.null(indices))
      indices=seq_len(nrow(x))

    ndiags <-length(indices)
    diags=rep(NA_real_,ndiags)
    for(i in seq_len(ndiags)){
      idx=indices[i]
      diags[i]=x[idx, idx]
    }
    diags
  } else {
    if(inherits(x, "dgCMatrix")) {
      if(is.null(indices)) x[sapply(1:nrow(x), function(y) (y-1)*ncol(x) + y)]
      else x[sapply(1:nrow(x), function(y) (y-1)*ncol(x) + y)[indices]]
    } else {
      if(is.null(indices)) diag(x) else diag(x)[indices]
    }
  }
}


#' Convert a subset of a score matrix to a sparse representation
#'
#' This can be useful for storing raw forwards and reverse NBLAST scores for a
#' set of neurons without having to store all the uncomputed elements in the
#' full score matrix.
#'
#' @param neuron_names a character vector of neuron names to save scores for.
#' @param dense_matrix the original, dense version of the full score matrix.
#'
#' @return A spare matrix, in compressed, column-oriented form, as an R object
#'   inheriting from both \code{\link[Matrix]{CsparseMatrix-class}} and
#'   \code{\link[Matrix]{generalMatrix-class}}.
#' @export
#' @importFrom Matrix sparseMatrix
sparse_score_mat <- function(neuron_names, dense_matrix) {
  col_num <- which(colnames(dense_matrix) %in% neuron_names)
  row_num <- which(rownames(dense_matrix) %in% neuron_names)
  spmat <- sparseMatrix(i=1, j=1, x=0, dims=dim(dense_matrix), dimnames=dimnames(dense_matrix))
  spmat[row_num, 1:ncol(spmat)] <- dense_matrix[row_num, 1:ncol(dense_matrix)]
  spmat[1:nrow(spmat), col_num] <- dense_matrix[1:nrow(dense_matrix), col_num]
  spmat[sapply(1:nrow(spmat), function(y) (y-1)*ncol(spmat) + y)] <- diag(dense_matrix)
  spmat
}
