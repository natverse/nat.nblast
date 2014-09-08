context("Sparse score matrix functions")

testneurons <- readRDS('testdata/testneurons.rds')

dense_smat <- nblast(testneurons, testneurons)
sparse_smat <- sparse_score_mat(names(testneurons)[2:4], dense_smat)

test_that("conversion to sparse matrix representation is correct", {
  expect_equal(0, sparse_smat[1, 5])
  expect_equal(diagonal(dense_smat), diagonal(sparse_smat))
})

test_that("dimnames for sparse matrix are correct", {
  expect_equal(dimnames(sparse_smat), dimnames(dense_smat))
})

test_that("subsetting sparse matrix by characters works", {
  expect_equal(sparse_smat["5HT1bMARCM-F000001_seg001", "5HT1bMARCM-F000002_seg001"], sparse_smat[1, 2])
  expect_equal(sparse_smat[, "5HT1bMARCM-F000002_seg001"], sparse_smat[, 2])
  # spam's dropping behaviour is suboptimal...
  expect_equal(sparse_smat["5HT1bMARCM-F000001_seg001", ], sparse_smat[1, , drop=TRUE])
})
