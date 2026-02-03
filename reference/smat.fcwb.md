# Scoring matrices for neuron similarities in FCWB template brain

Scoring matrices quantify the log2 odds ratio that a segment pair with a
given distance and absolute dot product come from a pair of neurons of
the same type, rather than unrelated neurons.

## Details

These scoring matrices were generated using all by all pairs from 150
DL2 antennal lobe projection neurons from the FlyCircuit dataset and
5000 random pairs from the same dataset.

- `smat.fcwb` was trained using nearest-neighbour distance and the
  tangent vector defined by the first eigen vector of the k=5 nearest
  neighbours.

- `smat_alpha.fcwb` was defined as for `smat.fcwb` but weighted by the
  factor `alpha` defined as (l1-l2)/(l1+l2+l3) where l1,l2,l3 are the
  three eigen values.

Most work on the flycircuit dataset has been carried out using the
`smat.fcwb` scoring matrix although the `smat_alpha.fcwb` matrix which
emphasises the significance of matches between linear regions of the
neuron (such as axons) may have some advantages.
