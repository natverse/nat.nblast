library(nat)
library(catmaid)

conn <- catmaid_login(server="https://neuropil.janelia.org/tracing/fafb/v14/",
                      authname="fly", authpassword="superfly",
                      token = "c3990b12d59fb66d59107fb16e0540c4f3e91aa5")
catmaid_login(conn)

neurons <- read_catmaid_selection("../tnblast/data/DA1s.json", readNeurons = TRUE)
neurons <- resample(prune_twigs(neurons/1e3, twig_length = 2), stepsize=1)

dps <- nat::dotprops(neurons)

nrn1 <- neurons[[1]]
nrn2 <- neurons[[2]]
nrn3 <- neurons[[3]]
nrn4 <- neurons[[4]]

#somaid(nns) # resampled neurons don't have soma

dps1 <- dps[[1]]
dps2 <- dps[[2]]
dps3 <- dps[[3]]
dps4 <- dps[[4]]

gw1 <- as.ngraph(nrn1, weights=TRUE)
gw2 <- as.ngraph(nrn2, weights=TRUE)
gw3 <- as.ngraph(nrn3, weights=TRUE)
gw4 <- as.ngraph(nrn4, weights=TRUE)

igraph::topo_sort(as.ngraph(nrn4))

max(igraph::diameter(gw1), igraph::diameter(gw2))

dst1 <- igraph::distances(gw1, v=as.character(rootpoints(nrn1)))

get_dist_to_soma <- function(nrn) {
  gw <- as.ngraph(nrn, weights=TRUE)
  dst <- igraph::distances(gw, v=as.character(rootpoints(nrn)))
  as.numeric(dst)
}

dps1$alpha <- get_dist_to_soma(nrn1)
dps2$alpha <- get_dist_to_soma(nrn2)
dps3$alpha <- get_dist_to_soma(nrn3)
dps4$alpha <- get_dist_to_soma(nrn4)

dps_list <- neuronlist(dps1, dps2, dps3, dps4)

devtools::load_all()

dps_aba1 <- nblast_allbyall(dps_list, normalisation = "raw")
dps_aba2 <- nblast_allbyall(dps_list, UseAlpha = T, normalisation = "raw")
par(mfrow=c(1,2))
heatmap(dps_aba1)
heatmap(dps_aba2)

