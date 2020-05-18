set.seed(1)
source("~/prog/r_packages/sparseMatrixStats/tests/testthat/setup.R")
mat <- make_matrix_with_all_features(nrow = 15, ncol=10) < 0
sp_mat <- as(mat, "lgCMatrix")


test_that("colSums works", {
  expect_equal(colSums2(sp_mat), matrixStats::colSums2(mat))
  expect_equal(colSums2(sp_mat, na.rm=TRUE), matrixStats::colSums2(mat, na.rm=TRUE))
})

test_that("rowSums works", {
  sp_mat2 <- t(sp_mat)
  expect_equal(rowSums2(sp_mat2), matrixStats::colSums2(mat))
  expect_equal(rowSums2(sp_mat2, na.rm=TRUE), matrixStats::colSums2(mat, na.rm=TRUE))
})



test_that("colMeans works", {
  expect_equal(colMeans2(sp_mat), matrixStats::colMeans2(mat))
  expect_equal(colMeans2(sp_mat, na.rm=TRUE), matrixStats::colMeans2(mat, na.rm=TRUE))
})

test_that("rowMeans works", {
  sp_mat2 <- t(sp_mat)
  expect_equal(rowMeans2(sp_mat2), matrixStats::colMeans2(mat))
  expect_equal(rowMeans2(sp_mat2, na.rm=TRUE), matrixStats::colMeans2(mat, na.rm=TRUE))
})



test_that("colCounts works", {
  expect_equal(colCounts(sp_mat, value=0), matrixStats::colCounts(mat, value=0))
  expect_equal(colCounts(sp_mat, value = TRUE), matrixStats::colCounts(mat, value = TRUE))
  expect_equal(colCounts(sp_mat, value = FALSE), matrixStats::colCounts(mat, value = FALSE))
  expect_equal(colCounts(sp_mat, value = 42), matrixStats::colCounts(mat, value = 42))
  expect_equal(colCounts(sp_mat, na.rm=TRUE, value = 0), matrixStats::colCounts(mat, na.rm=TRUE, value = 0))
  expect_equal(colCounts(sp_mat, na.rm=TRUE, value = 42), matrixStats::colCounts(mat, na.rm=TRUE, value = 42))
})


test_that("colAnyNAs works", {
  expect_equal(colAnyNAs(sp_mat), matrixStats::colAnyNAs(mat))
})


test_that("colAnys works", {
  expect_equal(colAnys(sp_mat), matrixStats::colAnys(mat))
  expect_equal(colAnys(sp_mat, na.rm=TRUE), matrixStats::colAnys(mat, na.rm=TRUE))
  expect_equal(colAnys(sp_mat, value = FALSE), matrixStats::colAnys(mat, value = FALSE))
  expect_equal(colAnys(sp_mat, na.rm=TRUE, value = FALSE), matrixStats::colAnys(mat, na.rm=TRUE, value = FALSE))
  expect_equal(colAnys(sp_mat, value=0), matrixStats::colAnys(mat, value=0))
  expect_equal(colAnys(sp_mat, na.rm=TRUE, value=0), matrixStats::colAnys(mat, na.rm=TRUE, value = 0))
  expect_equal(colAnys(sp_mat, value = 42), matrixStats::colAnys(mat, value = 42))
  expect_equal(colAnys(sp_mat, na.rm=TRUE, value = 42), matrixStats::colAnys(mat, na.rm=TRUE, value = 42))
})


test_that("colAlls works", {
  expect_equal(colAlls(sp_mat), matrixStats::colAlls(mat))
  expect_equal(colAlls(sp_mat, na.rm=TRUE), matrixStats::colAlls(mat, na.rm=TRUE))
  expect_equal(colAlls(sp_mat, value = FALSE), matrixStats::colAlls(array(as.logical(mat), dim = dim(mat)), value = FALSE))
  expect_equal(colAlls(sp_mat, na.rm=TRUE, value = FALSE), matrixStats::colAlls(array(as.logical(mat), dim = dim(mat)), na.rm=TRUE, value = FALSE))
  expect_equal(colAlls(sp_mat, value=0), matrixStats::colAlls(mat, value=0))
  expect_equal(colAlls(sp_mat, na.rm=TRUE, value=0), matrixStats::colAlls(mat, na.rm=TRUE, value = 0))
  expect_equal(colAlls(sp_mat, value = 42), matrixStats::colAlls(mat, value = 42))
  expect_equal(colAlls(sp_mat, na.rm=TRUE, value = 42), matrixStats::colAlls(mat, na.rm=TRUE, value = 42))
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



test_that("colTabulates works", {
  expect_equal(colTabulates(sp_mat), matrixStats::colTabulates(mat))
  expect_equal(colTabulates(sp_mat, values = integer(0L)), matrixStats::colTabulates(mat, values = integer(0L)))
  values <- c(FALSE, TRUE, TRUE, NA)
  expect_equal(colTabulates(sp_mat, values = values), matrixStats::colTabulates(mat, values = values))
  expect_equal(colTabulates(sp_mat, values = c(TRUE, values)), matrixStats::colTabulates(mat, values = c(TRUE, values)))
  expect_equal(colTabulates(sp_mat, values = values[-1]), matrixStats::colTabulates(mat, values = values[-1]))
  skip("matrixStats doesn't convert values to logical if mat is logical?!")
  expect_equal(colTabulates(sp_mat, values = c(1, values)), matrixStats::colTabulates(mat, values = c(1, values)))
})


test_that("cumulative functions work", {
  expect_equal(colCumsums(sp_mat), matrixStats::colCumsums(mat))
})


test_that("colIQRs works", {
  expect_equal(colIQRs(sp_mat), matrixStats::colIQRs(mat))
})


test_that("colWeightedMeans works", {
  weights <- rnorm(nrow(sp_mat), mean=4, sd=0.1)
  expect_equal(colWeightedMeans(sp_mat, w=weights), matrixStats::colWeightedMeans(mat, w=weights))
  expect_equal(colWeightedMeans(sp_mat, na.rm=TRUE, w=weights), matrixStats::colWeightedMeans(mat, na.rm=TRUE, w=weights))
})


test_that("colWeightedVars works", {
  weights <- rnorm(nrow(sp_mat), mean=4, sd=0.1)
  expect_equal(colWeightedVars(sp_mat, w=weights), matrixStats::colWeightedVars(mat, w=weights))
  expect_equal(colWeightedVars(sp_mat, na.rm=TRUE), matrixStats::colWeightedVars(mat, na.rm=TRUE))
})


test_that("colWeightedSds works", {
  weights <- rnorm(nrow(sp_mat), mean=4, sd=0.1)
  expect_equal(colWeightedSds(sp_mat, w=weights), matrixStats::colWeightedSds(mat, w=weights))
  expect_equal(colWeightedSds(sp_mat, na.rm=TRUE), matrixStats::colWeightedSds(mat, na.rm=TRUE))
})


test_that("colCollapse works", {
  expect_equal(colCollapse(sp_mat, idxs = 1), matrixStats::colCollapse(mat, idxs = 1))
  expect_equal(colCollapse(sp_mat, idxs = c(1,3)), matrixStats::colCollapse(mat, idxs = c(1,3)))
  expect_equal(colCollapse(sp_mat, idxs = 1:5, cols = min(ncol(mat), 3)), matrixStats::colCollapse(mat, idxs = 1:5, cols = min(ncol(mat), 3)))
})

test_that("colAvgsPerRowSet works", {
  S <-  suppressWarnings(matrix(seq_len(nrow(mat)), ncol = 2))
  expect_equal(colAvgsPerRowSet(sp_mat, S = S, na.rm = TRUE), matrixStats::colAvgsPerRowSet(mat, S = S))
})
