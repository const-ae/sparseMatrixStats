set.seed(1)
# source("tests/testthat/setup.R")
mat <- t(make_matrix_with_all_features(nrow=15, ncol=10))
sp_mat <- as(mat, "dgCMatrix")


test_that("rowSums works", {
  expect_equal(rowSums2(sp_mat), matrixStats::rowSums2(mat))
  expect_equal(rowSums2(sp_mat, na.rm=TRUE), matrixStats::rowSums2(mat, na.rm=TRUE))
})


test_that("rowMeans works", {
  expect_equal(rowMeans2(sp_mat), matrixStats::rowMeans2(mat))
  expect_equal(rowMeans2(sp_mat, na.rm=TRUE), matrixStats::rowMeans2(mat, na.rm=TRUE))
})


test_that("rowMedians works", {
  expect_equal(rowMedians(sp_mat), matrixStats::rowMedians(mat))
  expect_equal(rowMedians(sp_mat, na.rm=TRUE), matrixStats::rowMedians(mat, na.rm=TRUE))
})



test_that("rowVars works", {
  expect_equal(rowVars(sp_mat), matrixStats::rowVars(mat))
  expect_equal(rowVars(sp_mat, na.rm=TRUE), matrixStats::rowVars(mat, na.rm=TRUE))
})


test_that("rowMads works", {
  expect_equal(rowMads(sp_mat), matrixStats::rowMads(mat))
  expect_equal(rowMads(sp_mat, na.rm=TRUE), matrixStats::rowMads(mat, na.rm=TRUE))
})

test_that("rowMins works", {
  expect_equal(rowMins(sp_mat), matrixStats::rowMins(mat))
  expect_equal(rowMins(sp_mat, na.rm=TRUE), matrixStats::rowMins(mat, na.rm=TRUE))
})


test_that("rowMaxs works", {
  expect_equal(rowMaxs(sp_mat), matrixStats::rowMaxs(mat))
  expect_equal(rowMaxs(sp_mat, na.rm=TRUE), matrixStats::rowMaxs(mat, na.rm=TRUE))
})



test_that("rowCounts works", {
  expect_equal(rowCounts(sp_mat, value=0), matrixStats::rowCounts(mat, value=0))
  expect_equal(rowCounts(sp_mat, na.rm=TRUE, value=0), matrixStats::rowCounts(mat, na.rm=TRUE, value = 0))
  expect_equal(rowCounts(sp_mat, value = tail(t(sp_mat)@x, n=1)), matrixStats::rowCounts(mat, value = tail(t(sp_mat)@x, n=1)))
  expect_equal(rowCounts(sp_mat, na.rm=TRUE, value = tail(t(sp_mat)@x, n=1)), matrixStats::rowCounts(mat, na.rm=TRUE, value = tail(t(sp_mat)@x, n=1)))
})


test_that("rowAnyNAs works", {
  empty_mat <- matrix(numeric(0), ncol=0, nrow=5)
  sp_empty_mat <- as(empty_mat, "dgCMatrix")
  expect_equal(rowAnyNAs(sp_mat), matrixStats::rowAnyNAs(mat))
  expect_equal(rowAnyNAs(sp_empty_mat), matrixStats::rowAnyNAs(empty_mat))
})


test_that("rowAnys works", {
  empty_mat <- matrix(numeric(0), ncol=0, nrow=5)
  sp_empty_mat <- as(empty_mat, "dgCMatrix")
  expect_equal(rowAnys(sp_mat, value=0), matrixStats::rowAnys(mat, value=0))
  expect_equal(rowAnys(sp_mat, na.rm=TRUE, value=0), matrixStats::rowAnys(mat, na.rm=TRUE, value = 0))
  # expect_equal(rowAnys(sp_mat, value=NA), matrixStats::rowAnys(mat, value=NA))
  # expect_equal(rowAnys(sp_mat, na.rm=TRUE, value=NA), matrixStats::rowAnys(mat, na.rm=TRUE, value = NA))
  expect_equal(rowAnys(sp_mat, value = tail(t(sp_mat)@x, n=1)), matrixStats::rowAnys(mat, value = tail(t(sp_mat)@x, n=1)))
  expect_equal(rowAnys(sp_mat, na.rm=TRUE, value = tail(t(sp_mat)@x, n=1)), matrixStats::rowAnys(mat, na.rm=TRUE, value = tail(t(sp_mat)@x, n=1)))
})


test_that("rowAlls works", {
  empty_mat <- matrix(numeric(0), nrow=5, ncol=0)
  sp_empty_mat <- as(empty_mat, "dgCMatrix")
  expect_equal(rowAlls(sp_mat, value=0), matrixStats::rowAlls(mat, value=0))
  expect_equal(rowAlls(sp_mat, na.rm=TRUE, value=0), matrixStats::rowAlls(mat, na.rm=TRUE, value = 0))
  # expect_equal(rowAnys(sp_mat, value=NA), matrixStats::rowAnys(mat, value=NA))
  # expect_equal(rowAnys(sp_mat, na.rm=TRUE, value=NA), matrixStats::rowAnys(mat, na.rm=TRUE, value = NA))
  expect_equal(rowAlls(sp_mat, value = tail(t(sp_mat)@x, n=1)), matrixStats::rowAlls(mat, value = tail(t(sp_mat)@x, n=1)))
  expect_equal(rowAlls(sp_mat, na.rm=TRUE, value = tail(t(sp_mat)@x, n=1)), matrixStats::rowAlls(mat, na.rm=TRUE, value = tail(t(sp_mat)@x, n=1)))
})


test_that("rowLogSumExps works", {
  expect_equal(rowLogSumExps(sp_mat), matrixStats::rowLogSumExps(mat))
  expect_equal(rowLogSumExps(sp_mat, na.rm=TRUE), matrixStats::rowLogSumExps(mat, na.rm=TRUE))
})


test_that("rowProds works", {
  expect_equal(rowProds(sp_mat), matrixStats::rowProds(mat))
  expect_equal(rowProds(sp_mat, na.rm=TRUE), matrixStats::rowProds(mat, na.rm=TRUE))
})

test_that("rowQuantiles works", {
  expect_equal(rowQuantiles(sp_mat), matrixStats::rowQuantiles(mat))
  expect_equal(rowQuantiles(sp_mat, na.rm=TRUE), matrixStats::rowQuantiles(mat, na.rm=TRUE))
})


test_that("rowTabulates works", {
  suppressWarnings({ # Suppress warning of Inf -> NA
    int_mat <- matrix(as.integer(mat), nrow = nrow(mat), ncol = ncol(mat))
  })
  int_sp_mat <- as(int_mat, "dgCMatrix")
  expect_equal(rowTabulates(int_sp_mat), matrixStats::rowTabulates(int_mat))
  values <- c(0, -2, NA, 3, 17)
  expect_equal(rowTabulates(int_sp_mat, values = values), matrixStats::rowTabulates(int_mat, values = values))
})


test_that("rowOrderStats works", {
  no_na_mat <- mat
  no_na_mat[is.na(no_na_mat)] <- 99
  no_na_sp_mat <- as(no_na_mat, "dgCMatrix")

  expect_equal(rowOrderStats(no_na_sp_mat, which = 1), matrixStats::rowOrderStats(no_na_mat, which = 1))
  expect_equal(rowOrderStats(no_na_sp_mat, which = 6), matrixStats::rowOrderStats(no_na_mat, which = 6))
  expect_error(rowOrderStats(no_na_sp_mat, which = 110)) # which should be larger than nrow(no_na_mat)
  expect_error(matrixStats::rowOrderStats(no_na_mat, which = 110))
  skip("matrixStats::xxxOrderStats() does not support missing values")
  expect_equal(rowOrderStats(sp_mat, which = 6), matrixStats::rowOrderStats(mat, which = 6))
  expect_equal(rowOrderStats(sp_mat, which = 10, na.rm=TRUE), matrixStats::rowOrderStats(mat, which = 6, na.rm=TRUE))
})



test_that("cumulative functions work", {
  expect_equal(rowCumsums(sp_mat), matrixStats::rowCumsums(mat))
  expect_equal(rowCumprods(sp_mat), matrixStats::rowCumprods(mat))
  expect_equal(rowCummins(sp_mat), matrixStats::rowCummins(mat))
  expect_equal(rowCummaxs(sp_mat), matrixStats::rowCummaxs(mat))
  # There is no na.rm version
})



test_that("rowRanks works", {
  expect_equal(rowRanks(sp_mat), matrixStats::rowRanks(mat))
  expect_equal(rowRanks(sp_mat, ties.method = "average"), matrixStats::rowRanks(mat, ties.method = "average"))

  expect_equal(rowRanks(sp_mat), matrixStats::rowRanks(mat))
  expect_equal(rowRanks(sp_mat, ties.method = "average"), matrixStats::rowRanks(mat, ties.method = "average"))
})



test_that("rowWeightedMeans works", {
  weights <- rnorm(ncol(sp_mat), mean=4, sd=0.1)
  expect_equal(rowWeightedMeans(sp_mat, w=weights), matrixStats::rowWeightedMeans(mat, w=weights))
  expect_equal(rowWeightedMeans(sp_mat, na.rm=TRUE, w=weights), matrixStats::rowWeightedMeans(mat, na.rm=TRUE, w=weights))
})


test_that("rowWeightedMedians works", {
  weights <- rnorm(ncol(sp_mat), mean=4, sd=0.1)
  expect_equal(rowWeightedMedians(sp_mat, w=weights), matrixStats::rowWeightedMedians(mat, w=weights, interpolate = FALSE))
  expect_equal(rowWeightedMedians(sp_mat, na.rm=TRUE, w=weights), matrixStats::rowWeightedMedians(mat, w=weights, na.rm=TRUE, interpolate = FALSE))
})


test_that("rowWeightedMads works", {
  skip("different result than matrixStats version, because sparseMatrixStats uses `interpolate=FALSE`.")
  weights <- rnorm(ncol(sp_mat), mean=4, sd=0.1)
  expect_equal(rowWeightedMads(sp_mat, w=weights), matrixStats::rowWeightedMads(mat, w=weights))
  expect_equal(rowWeightedMads(sp_mat, na.rm=TRUE, w=weights), matrixStats::rowWeightedMads(mat, w=weights, na.rm=TRUE))
})


test_that("rowWeightedVars works", {
  weights <- rnorm(ncol(sp_mat), mean=4, sd=0.1)
  expect_equal(rowWeightedVars(sp_mat, w=weights), matrixStats::rowWeightedVars(mat, w=weights))
  expect_equal(rowWeightedVars(sp_mat, na.rm=TRUE), matrixStats::rowWeightedVars(mat, na.rm=TRUE))
})


test_that("rowWeightedSds works", {
  weights <- rnorm(ncol(sp_mat), mean=4, sd=0.1)
  expect_equal(rowWeightedSds(sp_mat, w=weights), matrixStats::rowWeightedSds(mat, w=weights))
  expect_equal(rowWeightedSds(sp_mat, na.rm=TRUE), matrixStats::rowWeightedSds(mat, na.rm=TRUE))
})



test_that("rowXXDiffs work", {

  expect_equal(rowDiffs(sp_mat, diff = 1), matrixStats::rowDiffs(mat, diff = 1))
  expect_equal(rowDiffs(sp_mat, diff = 3), matrixStats::rowDiffs(mat, diff = 3))
  expect_equal(rowDiffs(sp_mat, diff = 3, lag= 2), matrixStats::rowDiffs(mat, diff = 3, lag = 2))

  expect_equal(rowVarDiffs(sp_mat, diff = 0), matrixStats::rowVarDiffs(mat, diff = 0))
  expect_equal(rowVarDiffs(sp_mat, diff = 1), matrixStats::rowVarDiffs(mat, diff = 1))
  expect_equal(rowVarDiffs(sp_mat, diff = 3), matrixStats::rowVarDiffs(mat, diff = 3))
  expect_equal(rowVarDiffs(sp_mat, na.rm=TRUE), matrixStats::rowVarDiffs(mat, na.rm=TRUE))

  expect_equal(rowSdDiffs(sp_mat, diff = 0), matrixStats::rowSdDiffs(mat, diff = 0))
  expect_equal(rowSdDiffs(sp_mat, diff = 1), matrixStats::rowSdDiffs(mat, diff = 1))
  expect_equal(rowSdDiffs(sp_mat, diff = 3), matrixStats::rowSdDiffs(mat, diff = 3))
  expect_equal(rowSdDiffs(sp_mat, na.rm=TRUE), matrixStats::rowSdDiffs(mat, na.rm=TRUE))

  expect_equal(rowMadDiffs(sp_mat, diff = 0), matrixStats::rowMadDiffs(mat, diff = 0))
  expect_equal(rowMadDiffs(sp_mat, diff = 1), matrixStats::rowMadDiffs(mat, diff = 1))
  expect_equal(rowMadDiffs(sp_mat, diff = 3), matrixStats::rowMadDiffs(mat, diff = 3))
  expect_equal(rowMadDiffs(sp_mat, na.rm=TRUE), matrixStats::rowMadDiffs(mat, na.rm=TRUE))

  expect_equal(rowIQRDiffs(sp_mat, diff = 0), matrixStats::rowIQRDiffs(mat, diff = 0))
  expect_equal(rowIQRDiffs(sp_mat, diff = 1), matrixStats::rowIQRDiffs(mat, diff = 1))
  expect_equal(rowIQRDiffs(sp_mat, diff = 3), matrixStats::rowIQRDiffs(mat, diff = 3))
  expect_equal(rowIQRDiffs(sp_mat, na.rm=TRUE), matrixStats::rowIQRDiffs(mat, na.rm=TRUE))
})



