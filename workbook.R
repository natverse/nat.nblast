library(nat)
library(catmaid)

conn <- catmaid_login(server="https://neuropil.janelia.org/tracing/fafb/v14/",
                      authname="fly", authpassword="superfly",
                      token = "c3990b12d59fb66d59107fb16e0540c4f3e91aa5")
catmaid_login(conn)

neurons <- read_catmaid_selection("../tnblast/data/DA1s.json", readNeurons = TRUE)
neurons <- resample(prune_twigs(neurons/1e3, twig_length = 2), stepsize=1)

dps <- nat::dotprops(neurons)

#somaid(neurons) # resampled neurons don't have soma
#igraph::topo_sort(as.ngraph(nrn4))
#max(igraph::diameter(gw1), igraph::diameter(gw2))

get_dist_to_soma <- function(nrn) {
  gw <- as.ngraph(nrn, weights=TRUE)
  dst <- igraph::distances(gw, v=as.character(rootpoints(nrn)))
  as.numeric(dst)
}

make_topo_dotprops <- function(nrn) {
  tdps <- nat::dotprops(nrn)
  tdps$alpha <- get_dist_to_soma(nrn)
  tdps
}

dps_list <- nlapply(neurons, make_topo_dotprops)

devtools::load_all()

dps_aba1 <- nblast_allbyall(dps_list, normalisation = "raw")
dps_aba2 <- nblast_allbyall(dps_list, UseAlpha = T, normalisation = "raw")

par(mfrow = c(1,2))
image(dps_aba1)
title("NBLAST")
image(dps_aba2)
title("TNBLAST")

par(mfrow = c(1,2))
image(dps_aba1 / diag(dps_aba1))
title("NBLAST")
image(dps_aba2 / diag(dps_aba2))
title("TNBLAST")
