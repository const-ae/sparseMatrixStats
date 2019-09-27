mat <- make_matrix_with_all_features(nrow=10, ncol=6)
sp_mat <- as(mat, "dgCMatrix")



test_that("colSums works", {
  expect_equal(colSums2(sp_mat), matrixStats::colSums2(mat))
  expect_equal(colSums2(sp_mat, na.rm=TRUE), matrixStats::colSums2(mat, na.rm=TRUE))
})


test_that("colMeans works", {
  expect_equal(colMeans2(sp_mat), matrixStats::colMeans2(mat))
  expect_equal(colMeans2(sp_mat, na.rm=TRUE), matrixStats::colMeans2(mat, na.rm=TRUE))
})



test_that("colVars works", {
  expect_equal(colVars(sp_mat), matrixStats::colVars(mat))
  expect_equal(colVars(sp_mat, na.rm=TRUE), matrixStats::colVars(mat, na.rm=TRUE))
})



test_that("colMins works", {
  expect_equal(colMins(sp_mat), matrixStats::colMins(mat))
  expect_equal(colMins(sp_mat, na.rm=TRUE), matrixStats::colMins(mat, na.rm=TRUE))
})


test_that("colMaxs works", {
  expect_equal(colMaxs(sp_mat), matrixStats::colMaxs(mat))
  expect_equal(colMaxs(sp_mat, na.rm=TRUE), matrixStats::colMaxs(mat, na.rm=TRUE))
})



test_that("colCounts works", {
  expect_equal(colCounts(sp_mat, value=0), matrixStats::colCounts(mat, value=0))
  expect_equal(colCounts(sp_mat, na.rm=TRUE, value=0), matrixStats::colCounts(mat, na.rm=TRUE, value = 0))
  expect_equal(colCounts(sp_mat, value = tail(sp_mat@x, n=1)), matrixStats::colCounts(mat, value = tail(sp_mat@x, n=1)))
  expect_equal(colCounts(sp_mat, na.rm=TRUE, value = tail(sp_mat@x, n=1)), matrixStats::colCounts(mat, na.rm=TRUE, value = tail(sp_mat@x, n=1)))
})
