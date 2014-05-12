context("NBLAST v2")

testneurons <- readRDS('testdata/testneurons.rds')

test_that("nblast v2 produces expected scores", {
  scores <- nblast(testneurons[[1]], testneurons, version=2)
  scores.expected <- structure(c(31524.5213308596, -31627.7351772194, -34222.9007476271, -17201.5091863945, -34665.5897659894), .Names = c("5HT1bMARCM-F000001_seg001", "5HT1bMARCM-F000002_seg001", "5HT1bMARCM-F000003_seg001", "5HT1bMARCM-F000004_seg001", "5HT1bMARCM-F000005_seg001"))

  expect_equal(scores, scores.expected)
})

test_that("nblast v2 with alpha produces expected scores", {
  scores <- nblast(testneurons[[1]], testneurons, version=2, UseAlpha=TRUE)
  scores.expected <- structure(c(22393.1456610767, -33103.620929615, -35603.5457297204, -18717.4612913414, -35799.9999942706), .Names = c("5HT1bMARCM-F000001_seg001", "5HT1bMARCM-F000002_seg001", "5HT1bMARCM-F000003_seg001", "5HT1bMARCM-F000004_seg001", "5HT1bMARCM-F000005_seg001"))
  expect_equal(scores, scores.expected)
})

test_that("nblast v2 handles a neuronlist as query", {
  scores <- nblast(testneurons[1:2], testneurons, version=2)
  scores.expected <- structure(c(31524.5213308596, -31627.7351772194, -34222.9007476271, -17201.5091863945, -34665.5897659894, -14151.9043135782, 22201.6453549707, -20996.9159401486, -21406.6691003314, -21534.3940349266), .Dim = c(5L, 2L), .Dimnames = list(c("5HT1bMARCM-F000001_seg001", "5HT1bMARCM-F000002_seg001", "5HT1bMARCM-F000003_seg001", "5HT1bMARCM-F000004_seg001", "5HT1bMARCM-F000005_seg001"), c("5HT1bMARCM-F000001_seg001", "5HT1bMARCM-F000002_seg001")))
  expect_equal(scores, scores.expected)
})
