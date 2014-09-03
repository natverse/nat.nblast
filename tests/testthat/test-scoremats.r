context("Score matrix functions")

testneurons <- readRDS('testdata/testneurons.rds')

test_that("Conversion to sparse matrix representation is correct", {
  dense_smat <- nblast(testneurons, testneurons)
  sparse_smat <- sparse_score_mat(names(testneurons)[2:4], dense_smat)
  expect_true(all(sparse_smat[c(2, 3), c(3, 4)] == dense_smat[c(2, 3), c(3, 4)]))
  expect_equal(0, sparse_smat[1, 1])
})
