mat <- make_matrix_with_all_features(nrow=10, ncol=6)
sp_mat <- as(mat, "dgCMatrix")



test_that("colSums works", {
  expect_equal(colSums2(sp_mat), matrixStats::colSums2(mat))
  expect_equal(colSums2(sp_mat, na.rm=TRUE), matrixStats::colSums2(mat, na.rm=TRUE))
})

