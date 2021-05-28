# utils

get_dist_to_soma <- function(nrn) {
  gw <- as.ngraph(nrn, weights=TRUE)
  dst <- igraph::distances(gw, v = rootpoints(nrn))
  as.numeric(dst)
}

make_topo_dotprops <- function(nrn, resample = 1, k = 5) {
  tdps <- nat::dotprops(nrn, resample = resample, k = k, .parallel=TRUE)
  tdps$alpha <- get_dist_to_soma(nrn)
  tdps
}

make_sotopo_dotprops <- function(nrn, resample = 1, k = 5) {
  tdps <- nat::dotprops(nrn, resample = resample, k = k, .parallel=TRUE)
  tdps$alpha <- list()
  tdps$alpha$distance <- get_dist_to_soma(nrn)
  so <- strahler_order(nrn)
  tdps$alpha$so <- abs(so$points-max(so$points)) # normalizing so the main branch is always 0
  tdps
}

encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}
