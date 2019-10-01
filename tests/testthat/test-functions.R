# source("tests/testthat/setup.R")
mat <- make_matrix_with_all_features(nrow=10, ncol=6)
sp_mat <- as(mat, "dgCMatrix")
mat2 <- t(mat)
sp_mat2 <- t(sp_mat)



test_that("colSums works", {
  expect_equal(colSums2(sp_mat), matrixStats::colSums2(mat))
  expect_equal(colSums2(sp_mat, na.rm=TRUE), matrixStats::colSums2(mat, na.rm=TRUE))
})


test_that("colMeans works", {
  expect_equal(colMeans2(sp_mat), matrixStats::colMeans2(mat))
  expect_equal(colMeans2(sp_mat, na.rm=TRUE), matrixStats::colMeans2(mat, na.rm=TRUE))
})


test_that("colMedians works", {
  expect_equal(colMedians(sp_mat), matrixStats::colMedians(mat))
  expect_equal(colMedians(sp_mat, na.rm=TRUE), matrixStats::colMedians(mat, na.rm=TRUE))
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


test_that("colAnyNAs works", {
  empty_mat <- matrix(numeric(0), nrow=0, ncol=5)
  sp_empty_mat <- as(empty_mat, "dgCMatrix")
  expect_equal(colAnyNAs(sp_mat), matrixStats::colAnyNAs(mat))
  expect_equal(colAnyNAs(sp_empty_mat), matrixStats::colAnyNAs(empty_mat))
})


test_that("colAnys works", {
  empty_mat <- matrix(numeric(0), nrow=0, ncol=5)
  sp_empty_mat <- as(empty_mat, "dgCMatrix")
  expect_equal(colAnys(sp_mat, value=0), matrixStats::colAnys(mat, value=0))
  expect_equal(colAnys(sp_mat, na.rm=TRUE, value=0), matrixStats::colAnys(mat, na.rm=TRUE, value = 0))
  # expect_equal(colAnys(sp_mat, value=NA), matrixStats::colAnys(mat, value=NA))
  # expect_equal(colAnys(sp_mat, na.rm=TRUE, value=NA), matrixStats::colAnys(mat, na.rm=TRUE, value = NA))
  expect_equal(colAnys(sp_mat, value = tail(sp_mat@x, n=1)), matrixStats::colAnys(mat, value = tail(sp_mat@x, n=1)))
  expect_equal(colAnys(sp_mat, na.rm=TRUE, value = tail(sp_mat@x, n=1)), matrixStats::colAnys(mat, na.rm=TRUE, value = tail(sp_mat@x, n=1)))
})


test_that("colAlls works", {
  empty_mat <- matrix(numeric(0), nrow=0, ncol=5)
  sp_empty_mat <- as(empty_mat, "dgCMatrix")
  expect_equal(colAlls(sp_mat, value=0), matrixStats::colAlls(mat, value=0))
  expect_equal(colAlls(sp_mat, na.rm=TRUE, value=0), matrixStats::colAlls(mat, na.rm=TRUE, value = 0))
  # expect_equal(colAnys(sp_mat, value=NA), matrixStats::colAnys(mat, value=NA))
  # expect_equal(colAnys(sp_mat, na.rm=TRUE, value=NA), matrixStats::colAnys(mat, na.rm=TRUE, value = NA))
  expect_equal(colAlls(sp_mat, value = tail(sp_mat@x, n=1)), matrixStats::colAlls(mat, value = tail(sp_mat@x, n=1)))
  expect_equal(colAlls(sp_mat, na.rm=TRUE, value = tail(sp_mat@x, n=1)), matrixStats::colAlls(mat, na.rm=TRUE, value = tail(sp_mat@x, n=1)))
})


test_that("colLogSumExps works", {
  expect_equal(colLogSumExps(sp_mat), matrixStats::colLogSumExps(mat))
  expect_equal(colLogSumExps(sp_mat, na.rm=TRUE), matrixStats::colLogSumExps(mat, na.rm=TRUE))
})


test_that("colProds works", {
  expect_equal(colProds(sp_mat), matrixStats::colProds(mat))
  expect_equal(colProds(sp_mat, na.rm=TRUE), matrixStats::colProds(mat, na.rm=TRUE))
})

test_that("colQuantiles works", {
  expect_equal(colQuantiles(sp_mat), matrixStats::colQuantiles(mat))
  expect_equal(colQuantiles(sp_mat, na.rm=TRUE), matrixStats::colQuantiles(mat, na.rm=TRUE))
})



test_that("cumulative functions work", {
  expect_equal(colCumsums(sp_mat), matrixStats::colCumsums(mat))
  expect_equal(colCumprods(sp_mat), matrixStats::colCumprods(mat))
  expect_equal(colCummins(sp_mat), matrixStats::colCummins(mat))
  expect_equal(colCummaxs(sp_mat), matrixStats::colCummaxs(mat))
  # There is no na.rm version
})

