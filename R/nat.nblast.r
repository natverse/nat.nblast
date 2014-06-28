#' Neuron similarity, search and clustering tools
#'
#' @section Search:
#'
#'   The main entry point for similarity and search functions is
#'   \code{\link{nblast}}. The current version (2) depends on a scoring matrix.
#'   Default matrices trained using Drosophila neurons are distributed with this
#'   package (see \code{\link{smat.fcwb}})
#'
#' @section Package Options:
#'
#'   There is one package option \code{nat.nblast.defaultsmat} which is
#'   \code{NULL} by default.
#'
#' @references Costa, M., Ostrovsky, A.D., Manton, J.D., Prohaska, S., and
#'   Jefferis, G.S.X.E. (2014). NBLAST: Rapid, sensitive comparison of neuronal
#'   structure and construction of neuron family databases. Biorxiv preprint.
#'   \href{http://dx.doi.org/10.1101/006346}{doi: 10.1101/006346}.
#'
#' @name nat.nblast-package
#' @aliases nat.nblast
#' @docType package
#' @keywords package
#' @seealso \code{\link{nblast}}, \code{\link{smat.fcwb}}
NULL
