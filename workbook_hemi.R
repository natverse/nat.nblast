library(nat)
library(fafbseg)
library(hemibrainr)
library(dendroextras)

SKEL_DIR <- "/Users/dominik/projects/nblast_scoring_data/datasets/hemibrain_mismatch"
hemi_nrns <- read.neurons(SKEL_DIR)
plot3d(hemi_nrns, soma=T)

sc_pairs <- read.csv(paste(SKEL_DIR, "../hemibrain_mismatch_pairs.csv", sep="/"))

hemi_types <- unlist(lapply(hemi_nrns, function(nn) strsplit(nn$NeuronName,"_")[[1]][[2]]))
names(hemi_nrns) <- nlapply(hemi_nrns, function(nn) strsplit(nn$NeuronName,"_")[[1]][[3]])

hemi_nrns_raw <- hemi_nrns
hemi_nrns <- hemi_nrns/125
hemi_nrns <- nlapply(hemi_nrns,stitch_neurons_mst)

htst <- make_topo_dotprops(hemi_nrns[[1]], resample = 1, k=5)
plot3d(htst)

library(doMC)
registerDoMC(cores=4)
hemi_dps <- nlapply(hemi_nrns, make_topo_dotprops)
hemi_dps_so <- nlapply(hemi_nrns, make_sotopo_dotprops)

saveRDS(hemi_dps, "hemi_dotprops.rds")
hemi_dps <- readRDS("hemi_dotprops.rds")

dps_aba1 <- nblast_allbyall(hemi_dps, normalisation = "normalised")
dps_aba2 <- nblast_allbyall(hemi_dps, UseAlpha = T, normalisation = "normalised")
dps_aba3 <- nblast_allbyall(hemi_dps_so, UseAlpha = T, normalisation = "normalised")

par(mfrow = c(1,3))
image(dps_aba1, zlim= c(-1,1), yaxt='n', xaxt='n', col = hcl.colors(12, "viridis", rev = TRUE))
title("NBLAST")
image(dps_aba2, zlim= c(-1,1), yaxt='n', xaxt='n', col = hcl.colors(12, "viridis", rev = TRUE))
title("TNBLAST")
image(dps_aba3, zlim= c(-1,1), yaxt='n', xaxt='n', col = hcl.colors(12, "viridis", rev = TRUE))
title("TNBLAST+SO")
dev.off()

hcpn1 <- nhclust(scoremat=dps_aba1)
hcpn2 <- nhclust(scoremat=dps_aba2)
hcpn3 <- nhclust(scoremat=dps_aba3)

plot3d(hemi_nrns["5813027132"], soma=T)
plot3d(hemi_nrns["2009184596"], soma=T, add=T, color="blue")

val1 <- c()
val2 <- c()
val3 <- c()
for (i in 1:dim(sc_pairs)[[1]]) {
  sc1 <- dps_aba1[as.character(sc_pairs[i,]$query_id), as.character(sc_pairs[i,]$target_id)]
  sc2 <- dps_aba2[as.character(sc_pairs[i,]$query_id), as.character(sc_pairs[i,]$target_id)]
  sc3 <- dps_aba3[as.character(sc_pairs[i,]$query_id), as.character(sc_pairs[i,]$target_id)]
  val1 <- c(val1, sc1)
  val2 <- c(val2, sc2)
  val3 <- c(val3,sc3)
}

ii<-5
plot3d(hemi_nrns[as.character(sc_pairs[ii,]$query_id)], soma=T)
plot3d(hemi_nrns[as.character(sc_pairs[ii,]$target_id)], soma=T, add=T, color="blue")


tanglegram(dkcs1,dkcs2)

library('fossil')
library('clValid')

k = length(unique(hemi_types))

cutk1 <- cutree(hcpn1, k = k)
rand.index(cutk1, encode_ordinal(hemi_types))

cutk2 <- cutree(hcpn2, k = k)
rand.index(cutk2, encode_ordinal(hemi_types))

cutk3 <- cutree(hcpn3, k = k)
rand.index(cutk3, encode_ordinal(hemi_types))

# test on neurons represented in multiple types

nrns_types <- data.frame(type=hemi_types)
multi_types <- nrns_types %>% group_by(type) %>% count() %>% filter(n>1)

m_hemi_dps <- hemi_dps[nrns_types$type %in% multi_types$type]
m_hemi_dps_so <- hemi_dps_so[nrns_types$type %in% multi_types$type]

nrns_types_m <- hemi_types[nrns_types$type %in% multi_types$type]

dps_aba1 <- nblast_allbyall(m_hemi_dps, normalisation = "normalised")
dps_aba2 <- nblast_allbyall(m_hemi_dps, UseAlpha = T, normalisation = "normalised")
dps_aba3 <- nblast_allbyall(m_hemi_dps_so, UseAlpha = T, normalisation = "normalised")

hc1 <- nhclust(scoremat=dps_aba1)
hc2 <- nhclust(scoremat=dps_aba2)
hc3 <- nhclust(scoremat=dps_aba3)

k = length(multi_types$type)

dkcs1 = colour_clusters(hc1, k=k)
labels(dkcs1) <- nrns_types_m
plot(dkcs1)
dkcs2 <- colour_clusters(hc2, k=k)
labels(dkcs2) <- nrns_types_m
plot(dkcs2)
dkcs3 <- colour_clusters(hc3, k=k)
labels(dkcs3) <- nrns_types_m
plot(dkcs3)

cutk1 <- cutree(hc1, k = k)
rand.index(cutk1, encode_ordinal(nrns_types_m))

cutk2 <- cutree(hc2, k = k)
rand.index(cutk2, encode_ordinal(nrns_types_m))

cutk3 <- cutree(hc3, k = k)
rand.index(cutk3, encode_ordinal(nrns_types_m))

dunn((dps_aba1-1)*-1, cutk1)

dunn((dps_aba2-1)*-1, cutk2)

dunn((dps_aba3-1)*-1, cutk3)
