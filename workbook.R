library(nat)
library(catmaid)
library(dendroextras)

devtools::load_all()

catmaid_login()

neurons <- read_catmaid_selection("../tnblast/data/DA1s.json", readNeurons = TRUE)
neurons <- resample(prune_twigs(neurons/1e3, twig_length = 2), stepsize=1)

plot3d(neurons, soma = TRUE)

#somaid(neurons) # resampled neurons don't have soma
#igraph::topo_sort(as.ngraph(nrn4))
#max(igraph::diameter(gw1), igraph::diameter(gw2))

dps_list <- nlapply(neurons, make_topo_dotprops)

dps_aba1 <- nblast_allbyall(dps_list, normalisation = "raw")
dps_aba2 <- nblast_allbyall(dps_list, UseAlpha = T, normalisation = "raw")

par(mfrow = c(1,2))
image(dps_aba1 / diag(dps_aba1), zlim= c(0,1), yaxt='n', xaxt='n')
title("NBLAST")
image(dps_aba2 / diag(dps_aba2), zlim= c(0,1), yaxt='n', xaxt='n')
title("TNBLAST")

dps_list2 <- nlapply(neurons, make_sotopo_dotprops)

dps_aba21 <- nblast_allbyall(dps_list2, normalisation = "raw")
dps_aba22 <- nblast_allbyall(dps_list2, UseAlpha = T, normalisation = "raw")

par(mfrow = c(1,2))
image(dps_aba21 / diag(dps_aba21), zlim= c(0,1), yaxt='n', xaxt='n')
title("NBLAST")
image(dps_aba22 / diag(dps_aba22), zlim= c(0,1), yaxt='n', xaxt='n')
title("TNBLAST + SO")

# ---------------------- artificial neurons
nrn3x <- neurons[[3]]
nrn3x$StartPoint <- endpoints(neurons[[3]])[length(endpoints(neurons[[3]]))]

nrn3x <- neurons[[3]]
nrn3x$StartPoint <- endpoints(neurons[[3]])[106]
nrn4x <- neurons[[4]]
nrn4x$StartPoint <- endpoints(neurons[[4]])[length(endpoints(neurons[[4]]))]

newnrns <- neuronlist(neurons[[1]], neurons[[2]], nrn3x, nrn4x)

plot(newnrns,soma = T)

dps_listT1 <- nlapply(newnrns, make_topo_dotprops)

dps_aba1 <- nblast_allbyall(dps_listT1, normalisation = "raw")
dps_aba2 <- nblast_allbyall(dps_listT1, UseAlpha = T, normalisation = "raw")

par(mfrow = c(1,2))
image(dps_aba1 / diag(dps_aba1), zlim= c(0,1), yaxt='n', xaxt='n')
title("NBLAST")
image(dps_aba2 / diag(dps_aba2), zlim= c(0,1), yaxt='n', xaxt='n')
title("TNBLAST")

dps_listT2 <- nlapply(newnrns, make_sotopo_dotprops)

dps_aba3 <- nblast_allbyall(dps_listT2, UseAlpha = T, normalisation = "raw")

par(mfrow = c(1,2))
image(dps_aba1 / diag(dps_aba1), zlim= c(0,1), yaxt='n', xaxt='n')
title("NBLAST")
image(dps_aba3 / diag(dps_aba2), zlim= c(0,1), yaxt='n', xaxt='n')
title("TNBLAST + SO")
