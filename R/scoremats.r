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


#' Extract diagonal terms from a variety of matrix types
#'
#' @details Insists that input matrix is square. Uses the \code{'diagonal'}
#'   attribute when available and has specialised handling of \code{ff},
#'   \code{big.matrix}, \code{dgCMatrix} matrices. Does not check that row and
#'   colunm names are identical for those matrix clases (unlike the base
#'   \code{\link{diag}} function, but always uses rownames.
#' @param x A sqaure matrix
#' @param indices specifies a subset of the diagonal using a character vector of
#'   names, a logical vector or integer indices. The default (\code{NULL})
#'   implies all elements.
#' @return a named vector containing the diagonal elements.
#' @export
#' @examples
#' m=fill_in_sparse_score_mat(letters[1:5])
#' diagonal(m)
diagonal <- function(x, indices=NULL) UseMethod('diagonal')

#' @rdname diagonal
#' @export
diagonal.default <- function(x, indices=NULL) {
  if(!isTRUE(nrow(x)==ncol(x))) stop("x is not a square matrix!")

  if(is.character(indices)) indices=match(indices,rownames(x))
  if(!is.null(xdiag<-attr(x,'diagonal'))){
    return(if(is.null(indices)) xdiag else xdiag[indices])
  }

  if(is.logical(indices)) indices=which(indices)

  if(inherits(x,"ff")){
    # convert array indices to vector indices
    if(is.null(indices)) indices=seq_len(nrow(x))
    vidxs=ff::arrayIndex2vectorIndex(cbind(indices,indices),dim=dim(x))
    # by default we don't get the names back
    structure(x[vidxs], .Names=rownames(x)[indices])
  } else if(inherits(x,"big.matrix")) {
    fast_disk_diag(x, indices, use.names=TRUE)
  } else if(inherits(x, "dgCMatrix")) {
    diag_inds <- seq.int(from=1, by = nrow(x)+1, length.out=ncol(x))
    diags=structure(x[diag_inds], .Names=rownames(x))
    if(is.null(indices)) diags else diags[indices]
  } else {
    if(is.null(indices)) diag(x) else diag(x)[indices]
  }
}


# Utility function to get diagonal of on disk faster than it can manage by itself
# by reading in reasonble size chunks
fast_disk_diag<-function(x, indices=NULL, chunksize=300, use.names=TRUE) {
  if(is.null(indices)) indices=seq_len(nrow(x))
  ninds=length(indices)
  diags=rep(NA,ninds)
  if(ninds>chunksize){
    for(i in seq.int(from=0,by=chunksize,to=ninds-chunksize)) {
      sq=x[indices[i+1:chunksize],indices[i+1:chunksize]]
      diags[i+1:chunksize]=diag(sq)
    }
    # next index will be
    i=i+chunksize+1
  } else {
    # we'll be starting from scratch
    i=1
  }

  if(i<=ninds){
    sq=x[indices[i:ninds],indices[i:ninds]]
    diags[i:ninds]=diag(sq)
  }
  if(use.names) names(diags)=rownames(x)[indices]
  diags
}

#' Convert a subset of a square score matrix to a sparse representation
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
#' @seealso fill_in_sparse_score_mat
#' @examples
#' data(kcs20, package = "nat")
#' scores=nblast_allbyall(kcs20)
#' scores.3.sparse=sparse_score_mat(names(kcs20)[3], scores)
#' scores.3.sparse
#' # can also add additional submatrices
#' fill_in_sparse_score_mat(scores.3.sparse,scores[3:6,3:4])
sparse_score_mat <- function(neuron_names, dense_matrix) {
  col_num <- which(colnames(dense_matrix) %in% neuron_names)
  row_num <- which(rownames(dense_matrix) %in% neuron_names)
  spmat <- sparseMatrix(i=1, j=1, x=0, dims=dim(dense_matrix), dimnames=dimnames(dense_matrix))
  spmat[row_num, ] <- dense_matrix[row_num, ]
  spmat[, col_num] <- dense_matrix[, col_num]
  diag_inds <- seq.int(from=1, by = nrow(spmat)+1, length.out=ncol(spmat))
  spmat[diag_inds] <- diagonal(dense_matrix)
  spmat
}

# Utility function to convert a vector of scores into named row or column matrix
neuron_scores_to_mat <- function(scores, query, target) {
  if(!missing(query)){
    matrix(scores, nrow=length(scores), dimnames=c(names(scores), query))
  } else {
    matrix(scores, ncol=length(scores), dimnames=c(target, names(scores)))
  }
}

#' Add one or more submatrices to a sparse score matrix
#'
#' @param sparse_matrix either an existing (square) sparse matrix or a character
#'   vector of names that will be used to define an empty sparse matrix.
#' @param diag optional full diagonal for sparse matrix i.e. self-match scores.
#' @param ... Additional matrices to insert into \code{sparse_matrix}. Row and
#'   column names must have matches in \code{sparse_matrix}.
#' @seealso sparse_score_mat
#' @export
fill_in_sparse_score_mat <- function(sparse_matrix, ..., diag=NULL) {
  if(is.character(sparse_matrix)) {
    sparse_matrix <- sparseMatrix(i=1, j=1, x=0, dims=rep(length(sparse_matrix), 2), dimnames=list(sparse_matrix, sparse_matrix))
  }

  if(!is.null(diag)) {
    diag_inds <- seq.int(from=1, by = nrow(sparse_matrix)+1, length.out=ncol(sparse_matrix))
    sparse_matrix[diag_inds] <- diag
  }
  dense_matrices <- list(...)
  for(dense_matrix in dense_matrices) {
    if(!is.matrix(dense_matrix)) stop("I only work with matrices.\\nMaybe you need to use neuron_scores_to_mat to convert a vector of scores to a matrix!")
    sparse_matrix[rownames(dense_matrix), colnames(dense_matrix)] <- dense_matrix
  }

  sparse_matrix
}
