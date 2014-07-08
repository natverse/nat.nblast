context("Clustering")

testneurons <- readRDS('testdata/testneurons.rds')
test_score_mat <- nblast(testneurons, testneurons)

test_that("nhclust with default arguments clusters appropriately", {
  hc <- nhclust(names(testneurons), scoremat=test_score_mat)
  hc.expected.1to5 <- structure(list(merge = structure(c(-3L, -1L, -2L, 1L, -5L, -4L, 2L, 3L), .Dim = c(4L, 2L)), height = c(0.371447118771884, 1.25254918497012, 1.7418094977434, 2.50152423662556), order = c(3L, 5L, 2L, 1L, 4L), labels = c("5HT1bMARCM-F000001_seg001", "5HT1bMARCM-F000002_seg001", "5HT1bMARCM-F000003_seg001", "5HT1bMARCM-F000004_seg001", "5HT1bMARCM-F000005_seg001")), .Names = c("merge", "height", "order", "labels"))

  expect_equal(hc[1:4], hc.expected.1to5)
})

test_that("plot3d.hclust", {
  hc <- nhclust(names(testneurons), scoremat=test_score_mat)
  open3d()

  expect_equal(attr(plot3d(hc, k=3, db=testneurons),'df')$col,
               c("#FF0000FF", "#FF0000FF", "#00FF00FF", "#0000FFFF", "#0000FFFF"))
  clear3d()
  plot3d(hc, h=2, db=testneurons)
  clear3d()
  expect_equal(attr(plot3d(hc, h=2, db=testneurons, col='red'),'df')$col,
               rep("red", 5))
  rgl.close()
})

test_that('we can use diagonal attribute on a score matrix',{
  library(ff)
  library(nat)
  scores=nblast(kcs20, kcs20)
  scoresff=as.ff(scores)
  scoresffd=scoresff
  attr(scoresffd,'diagonal')=diag(scores)
  expect_equal(diag(scores), diagonal(scoresffd))
  expect_error(diagonal(scores[1:10,1:8]))
})
