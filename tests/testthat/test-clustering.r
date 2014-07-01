context("Clustering")

testneurons <- readRDS('testdata/testneurons.rds')
test_score_mat <- nblast(testneurons, testneurons)

test_that("hclustn with default arguments clusters appropriately", {
  hc <- hclustn(names(testneurons), scoremat=test_score_mat)
  hc.expected.1to5 <- structure(list(merge = structure(c(-3L, -1L, -2L, 1L, -5L, -4L, 2L, 3L), .Dim = c(4L, 2L)), height = c(0.371447118771884, 1.25254918497012, 1.7418094977434, 2.50152423662556), order = c(3L, 5L, 2L, 1L, 4L), labels = c("5HT1bMARCM-F000001_seg001", "5HT1bMARCM-F000002_seg001", "5HT1bMARCM-F000003_seg001", "5HT1bMARCM-F000004_seg001", "5HT1bMARCM-F000005_seg001"), method = "ward"), .Names = c("merge", "height", "order", "labels", "method"))

  expect_equal(hc[1:5], hc.expected.1to5)
})
