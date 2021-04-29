library(nat)
library(fafbseg)
library(hemibrainr)
library(dendroextras)

SKEL_DIR <- "/Users/dominik/projects/nblast_scoring_data/datasets/hemibrain_mismatch"
hemi_nrns <- read.neurons(SKEL_DIR)
plot3d(hemi_nrns, soma=T)

sc_pairs <- read.csv(paste(SKEL_DIR, "../hemibrain_mismatch_pairs.csv", sep="/"))

names(hemi_nrns) <- nlapply(hemi_nrns, function(nn) strsplit(nn$NeuronName,"_")[[1]][[3]])

