library(nat)
library(fafbseg)
library(dendroextras)

devtools::load_all()

OUT_DIR <- "meshes"

fw_pair <- flywire_fetch("https://globalv1.flywire-daf.com/nglstate/5478428614590464", return="text")
fw_scene <- flywire_fetch("https://globalv1.flywire-daf.com/nglstate/6012751403024384", return="text")

fw_nids1 <- ngl_segments(ngl_decode_scene(fw_pair), as_character = TRUE)
fw_nids2 <- ngl_segments(ngl_decode_scene(fw_scene), as_character = TRUE)
#save_cloudvolume_meshes(fw_nids, savedir = OUT_DIR, format = 'obj')

nids <- c(fw_nids1, fw_nids2)

fw_nrns <- skeletor(nids, method = "wavefront")

saveRDS(fw_nrns, "fw_neurons.rds")
fw_nrns <- readRDS("fw_neurons.rds")

fw_nrns_so <- nlapply(fw_nrns, function(x) reroot_hairball(x, k.soma.search = 50, radius.soma.search = 2500))

fw_nrns_cl <- nlapply(fw_nrns_so/1e3, stitch_neurons_mst, OmitFailures=T) %>%
  prune_twigs(twig_length=2, OmitFailures=T)

fw_nrns_sp <- nlapply(fw_nrns_cl, function(x) simplify_neuron(x, n=2))

plot3d(fw_nrns_sp, soma = T)

dps_list <- nlapply(fw_nrns_sp, make_topo_dotprops)

dps_aba1 <- nblast_allbyall(dps_list, normalisation = "normalised")
dps_aba2 <- nblast_allbyall(dps_list, UseAlpha = T, normalisation = "normalised")

par(mfrow = c(1,2))
image(dps_aba1, zlim= c(-1,1), yaxt='n', xaxt='n')
title("NBLAST")
image(dps_aba2, zlim= c(-1,1), yaxt='n', xaxt='n')
title("TNBLAST")

hckcs1 <- nhclust(scoremat=dps_aba1)
dev.off()
dkcs1 <- colour_clusters(hckcs1, k=3)
plot(dkcs1)

hckcs2 <- nhclust(scoremat=dps_aba2)
dkcs2 <- colour_clusters(hckcs2, k=3)
plot(dkcs2)

