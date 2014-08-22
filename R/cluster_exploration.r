#' Plot a scree plot for NBLAST scores
#'
#' This plots the number of considered clusters for a k-means clustering
#' (x-axis) against the within-cluster sum-of-squares (y-axis). As the number of
#' considered clusters increases the within-cluster sum-of-squares drops,
#' typically fairly steeply at first and then followed by an elbow to a more
#' gradual, almost level descent. Cattell's scree test states that the maximum
#' number of 'proper' clusters is the one at which this elbow occurs.
#'
#' If a number is passed in as \code{perm}, the within-cluster sum-of-squares
#' for random permutations of data will also be displayed. If a dataset has
#' 'prominent' clusters then one would expect that the within-cluster
#' sum-of-squares for the real data would decrease faster than those of the
#' permuted versions. A message indicating the maximum difference in
#' within-cluster sum-of-squares for original and permuted data is also
#' displayed.
#'
#' @param scores a matrix of NBLAST scores.
#' @param max_k the maximum number of clusters to consider.
#' @param num_runs the number of times to repeat each k-means clustering for a
#'   given number of clusters.
#' @param perm either \code{NULL}, indicating no permutations should be
#'   considered, or the number of times the data should be permuted.
#' @param ... extra arguments to pass to \code{\link{plot}}.
#'
#' @return (Invisibly) A list of within-cluster sum-of-squares for each number of clusters
#'   tested, for both real and (if \code{perm} is not \code{NULL}) permuted
#'   data.
#' @export
scree_plot <- function(scores, max_k, num_runs=1, perm=NULL, ...) {
  k <- seq(1, max_k)
  message("Calculating for real data...")
  wss <- sapply(k, function(x) sum(kmeans(scores, centers=x, nstart=num_runs)$withinss))
  return_wss <- list(real=wss)

  plot(k, wss, type="b", xlab="Number of clusters", ylab="Sum of within-clusters sum-of-squares", ylim=c(0, max(wss)), ...)

  if(!is.null(perm)) {
    message("Calculating for permuted data...")
    wss_perm <- lapply(1:perm, function(x) sapply(k, function(y) sum(kmeans(apply(scores, 2, sample), centers=y, nstart=num_runs)$withinss)))
    lapply(wss_perm, function(x) { par(new=TRUE); plot(k, x, col=rgb(1, 0, 0, 0.3), type='l', axes=FALSE, xlab="", ylab="", ylim=c(0, max(wss))) })

    wss_perm_mat <- do.call(rbind, wss_perm)
    mean_wss_perm <- apply(wss_perm_mat, 2, mean)
    par(new=TRUE)
    plot(k, mean_wss_perm, col=rgb(1, 0, 0, 1), lwd=2, type='l', axes=FALSE, xlab="", ylab="", ylim=c(0, max(wss)))

    wss_diff <- abs(wss - mean_wss_perm)
    message("Greatest difference between real data and permutated data at: ", which(wss_diff == max(wss_diff)), " clusters.")
    return_wss <- list(real=wss, perm=mean_wss_perm)
  }

  invisible(return_wss)
}
