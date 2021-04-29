library(nat)
library(fafbseg)
library(dendroextras)
library(elmr)
library('fossil')

devtools::load_all()

SKEL_DIR <- "/Users/dominik/projects/nblast_scoring_data/datasets/FAFB_RHS_uPNs"
nrns <- read.neurons(SKEL_DIR)
plot3d(nrns, soma=T)

lin_type <- unlist(nlapply(nrns, function(n) strsplit(n$NeuronName, "PN_")[[1]][[1]]))

nrns <- nrns/1e3
nrns_s <- nlapply(nrns, function(x) elmr::unspike(x, threshold=5))
nrns_s <- nlapply(nrns_s, function(x) smooth_neuron(x, sigma=1))
nrns_cl <- prune_twigs(nrns_s, twig_length=5, OmitFailures=T)

plot3d(nrns_cl, soma=T)

dps_list <- nlapply(nrns_cl, make_topo_dotprops)

plot3d(dps_list)

dps_aba1 <- nblast_allbyall(dps_list, normalisation = "normalised")
dps_aba2 <- nblast_allbyall(dps_list, UseAlpha = T, normalisation = "normalised")

dps_list_so <- nlapply(nrns_cl, make_sotopo_dotprops)

dps_aba3 <- nblast_allbyall(dps_list_so, UseAlpha = T, normalisation = "normalised")

par(mfrow = c(1,2))
image(dps_aba1, zlim= c(-1,1), yaxt='n', xaxt='n', col = hcl.colors(12, "viridis", rev = TRUE))
title("NBLAST")
image(dps_aba2, zlim= c(-1,1), yaxt='n', xaxt='n', col = hcl.colors(12, "viridis", rev = TRUE))
title("TNBLAST")

par(mfrow = c(1,2))
image(dps_aba1, zlim= c(-1,1), yaxt='n', xaxt='n', col = hcl.colors(12, "viridis", rev = TRUE))
title("NBLAST")
image(dps_aba3, zlim= c(-1,1), yaxt='n', xaxt='n', col = hcl.colors(12, "viridis", rev = TRUE))
title("TNBLAST+SO")
dev.off()

hcpn1 <- nhclust(scoremat=dps_aba1)
hcpn2 <- nhclust(scoremat=dps_aba2)
hcpn3 <- nhclust(scoremat=dps_aba3)

k = length(unique(lin_type))

dkcs1 = colour_clusters(hcpn1, k=k)
labels(dkcs1) <- lin_type
plot(dkcs1)

plot3d(hcpn1, k=k, db=dps_list)

dkcs2 = colour_clusters(hcpn2, k=k)
labels(dkcs2) <- lin_type
plot(dkcs2)

dkcs3 = colour_clusters(hcpn3, k=k)
labels(dkcs2) <- lin_type
plot(dkcs3)

cutk1 <- cutree(hcpn1, k = k)
rand.index(cutk1, encode_ordinal(lin_type))

cutk2 <- cutree(hcpn2, k = k)
rand.index(cutk2, encode_ordinal(lin_type))

cutk3 <- cutree(hcpn3, k = k)
rand.index(cutk3, encode_ordinal(lin_type))

dunn((dps_aba1-1)*-1, cutk1)

dunn((dps_aba2-1)*-1, cutk2)

dunn((dps_aba3-1)*-1, cutk3)
