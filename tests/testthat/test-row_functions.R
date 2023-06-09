set.seed(1)
# source("tests/testthat/setup.R")

mat <- t(make_matrix_with_all_features(nrow=15, ncol=10))
rownames(mat) <- letters[seq_len(10)]
sp_mat <- as(mat, "dgCMatrix")
row_subset <- 1:5
col_subset <- c(7, 9, 2)

test_that("rowSums works", {
  expect_equal(rowSums2(sp_mat), MatrixGenerics::rowSums2(mat))
  expect_equal(rowSums2(sp_mat, na.rm=TRUE), MatrixGenerics::rowSums2(mat, na.rm=TRUE))
  expect_equal(rowSums2(sp_mat, rows = row_subset, cols = col_subset), MatrixGenerics::rowSums2(mat, rows = row_subset, cols = col_subset))
})


test_that("rowMeans works", {
  expect_equal(rowMeans2(sp_mat), MatrixGenerics::rowMeans2(mat))
  expect_equal(rowMeans2(sp_mat, na.rm=TRUE), MatrixGenerics::rowMeans2(mat, na.rm=TRUE))
  expect_equal(rowMeans2(sp_mat, rows = row_subset, cols = col_subset), MatrixGenerics::rowMeans2(mat, rows = row_subset, cols = col_subset))
})


test_that("rowMedians works", {
  expect_equal(rowMedians(sp_mat), MatrixGenerics::rowMedians(mat))
  expect_equal(rowMedians(sp_mat, na.rm=TRUE), MatrixGenerics::rowMedians(mat, na.rm=TRUE))
  expect_equal(rowMedians(sp_mat, rows = row_subset, cols = col_subset), MatrixGenerics::rowMedians(mat, rows = row_subset, cols = col_subset))
})



test_that("rowVars works", {
  expect_equal(rowVars(sp_mat), MatrixGenerics::rowVars(mat))
  expect_equal(rowVars(sp_mat, na.rm=TRUE), MatrixGenerics::rowVars(mat, na.rm=TRUE))
  expect_equal(rowVars(sp_mat, rows = row_subset, cols = col_subset), MatrixGenerics::rowVars(mat, rows = row_subset, cols = col_subset))

  center <- rowMeans2(sp_mat)
  expect_equal(rowVars(sp_mat, center = center), MatrixGenerics::rowVars(mat, center = center))
})

test_that("rowSds works", {
  expect_equal(rowSds(sp_mat), MatrixGenerics::rowSds(mat))
  expect_equal(rowSds(sp_mat, na.rm=TRUE), MatrixGenerics::rowSds(mat, na.rm=TRUE))
  expect_equal(rowSds(sp_mat, rows = row_subset, cols = col_subset), MatrixGenerics::rowSds(mat, rows = row_subset, cols = col_subset))
})


test_that("rowMads works", {
  expect_equal(rowMads(sp_mat), MatrixGenerics::rowMads(mat))
  expect_equal(rowMads(sp_mat, na.rm=TRUE), MatrixGenerics::rowMads(mat, na.rm=TRUE))
  expect_equal(rowMads(sp_mat, rows = row_subset, cols = col_subset), MatrixGenerics::rowMads(mat, rows = row_subset, cols = col_subset))

  center <- rowMeans2(sp_mat)
  expect_equal(rowMads(sp_mat, center = center), MatrixGenerics::rowMads(mat, center = center))
})

test_that("rowMins works", {
  expect_equal(rowMins(sp_mat), MatrixGenerics::rowMins(mat))
  expect_equal(rowMins(sp_mat, na.rm=TRUE), MatrixGenerics::rowMins(mat, na.rm=TRUE))
  expect_equal(rowMins(sp_mat, rows = row_subset, cols = col_subset), MatrixGenerics::rowMins(mat, rows = row_subset, cols = col_subset))
})


test_that("rowMaxs works", {
  expect_equal(rowMaxs(sp_mat), MatrixGenerics::rowMaxs(mat))
  expect_equal(rowMaxs(sp_mat, na.rm=TRUE), MatrixGenerics::rowMaxs(mat, na.rm=TRUE))
  expect_equal(rowMaxs(sp_mat, rows = row_subset, cols = col_subset), MatrixGenerics::rowMaxs(mat, rows = row_subset, cols = col_subset))
})



test_that("rowCounts works", {
  expect_equal(rowCounts(sp_mat, value=0), MatrixGenerics::rowCounts(mat, value=0))
  expect_equal(rowCounts(sp_mat, na.rm=TRUE, value=0), MatrixGenerics::rowCounts(mat, na.rm=TRUE, value = 0))
  expect_equal(rowCounts(sp_mat, value = tail(t(sp_mat)@x, n=1)), MatrixGenerics::rowCounts(mat, value = tail(t(sp_mat)@x, n=1)))
  expect_equal(rowCounts(sp_mat, na.rm=TRUE, value = tail(t(sp_mat)@x, n=1)), MatrixGenerics::rowCounts(mat, na.rm=TRUE, value = tail(t(sp_mat)@x, n=1)))
  expect_equal(rowCounts(sp_mat, value=0, rows = row_subset, cols = col_subset), MatrixGenerics::rowCounts(mat, value=0, rows = row_subset, cols = col_subset))
})


test_that("rowAnyNAs works", {
  empty_mat <- matrix(numeric(0), ncol=0, nrow=5)
  sp_empty_mat <- as(empty_mat, "dgCMatrix")
  expect_equal(rowAnyNAs(sp_mat), MatrixGenerics::rowAnyNAs(mat))
  expect_equal(rowAnyNAs(sp_empty_mat), MatrixGenerics::rowAnyNAs(empty_mat))
  expect_equal(rowAnyNAs(sp_mat, rows = row_subset, cols = col_subset), MatrixGenerics::rowAnyNAs(mat, rows = row_subset, cols = col_subset))
})


test_that("rowAnys works", {
  empty_mat <- matrix(numeric(0), ncol=0, nrow=5)
  sp_empty_mat <- as(empty_mat, "dgCMatrix")
  expect_equal(rowAnys(sp_mat, value=0), MatrixGenerics::rowAnys(mat, value=0))
  expect_equal(rowAnys(sp_mat, na.rm=TRUE, value=0), MatrixGenerics::rowAnys(mat, na.rm=TRUE, value = 0))
  # expect_equal(rowAnys(sp_mat, value=NA), MatrixGenerics::rowAnys(mat, value=NA))
  # expect_equal(rowAnys(sp_mat, na.rm=TRUE, value=NA), MatrixGenerics::rowAnys(mat, na.rm=TRUE, value = NA))
  expect_equal(rowAnys(sp_mat, value = tail(t(sp_mat)@x, n=1)), MatrixGenerics::rowAnys(mat, value = tail(t(sp_mat)@x, n=1)))
  expect_equal(rowAnys(sp_mat, na.rm=TRUE, value = tail(t(sp_mat)@x, n=1)), MatrixGenerics::rowAnys(mat, na.rm=TRUE, value = tail(t(sp_mat)@x, n=1)))
  expect_equal(rowAnys(sp_mat, value=0, rows = row_subset, cols = col_subset), MatrixGenerics::rowAnys(mat, value=0, rows = row_subset, cols = col_subset))
})


test_that("rowAlls works", {
  empty_mat <- matrix(numeric(0), nrow=5, ncol=0)
  sp_empty_mat <- as(empty_mat, "dgCMatrix")
  expect_equal(rowAlls(sp_mat, value=0), MatrixGenerics::rowAlls(mat, value=0))
  expect_equal(rowAlls(sp_mat, na.rm=TRUE, value=0), MatrixGenerics::rowAlls(mat, na.rm=TRUE, value = 0))
  # expect_equal(rowAnys(sp_mat, value=NA), MatrixGenerics::rowAnys(mat, value=NA))
  # expect_equal(rowAnys(sp_mat, na.rm=TRUE, value=NA), MatrixGenerics::rowAnys(mat, na.rm=TRUE, value = NA))
  expect_equal(rowAlls(sp_mat, value = tail(t(sp_mat)@x, n=1)), MatrixGenerics::rowAlls(mat, value = tail(t(sp_mat)@x, n=1)))
  expect_equal(rowAlls(sp_mat, na.rm=TRUE, value = tail(t(sp_mat)@x, n=1)), MatrixGenerics::rowAlls(mat, na.rm=TRUE, value = tail(t(sp_mat)@x, n=1)))
  expect_equal(rowAlls(sp_mat, value=0, rows = row_subset, cols = col_subset), MatrixGenerics::rowAlls(mat, value=0, rows = row_subset, cols = col_subset))
})


test_that("rowLogSumExps works", {
  expect_equal(rowLogSumExps(sp_mat), MatrixGenerics::rowLogSumExps(mat))
  expect_equal(rowLogSumExps(sp_mat, na.rm=TRUE), MatrixGenerics::rowLogSumExps(mat, na.rm=TRUE))
  expect_equal(rowLogSumExps(sp_mat, rows = row_subset, cols = col_subset), MatrixGenerics::rowLogSumExps(mat, rows = row_subset, cols = col_subset))
})


test_that("rowProds works", {
  expect_equal(rowProds(sp_mat), MatrixGenerics::rowProds(mat))
  expect_equal(rowProds(sp_mat, na.rm=TRUE), MatrixGenerics::rowProds(mat, na.rm=TRUE))
  expect_equal(rowProds(sp_mat, rows = row_subset, cols = col_subset), MatrixGenerics::rowProds(mat, rows = row_subset, cols = col_subset))
})

test_that("rowQuantiles works", {
  expect_equal(rowQuantiles(sp_mat), MatrixGenerics::rowQuantiles(mat))
  expect_equal(rowQuantiles(sp_mat, na.rm=TRUE), MatrixGenerics::rowQuantiles(mat, na.rm=TRUE))
  expect_equal(rowQuantiles(sp_mat, rows = row_subset, cols = col_subset), MatrixGenerics::rowQuantiles(mat, rows = row_subset, cols = col_subset))

  # different quantile algorithms work
  y <- rpois(n = 21, lambda = 1.5)
  for(t in 1:9){
    expect_equal(rowQuantiles(matrix(y, nrow = 1), type = t, probs = seq(0, 1, length.out = 13), drop = TRUE),
                 rowQuantiles(as(matrix(y, nrow = 1), "dgCMatrix"), type = t, probs = seq(0, 1, length.out = 13), drop = TRUE))
  }

})


test_that("rowTabulates works", {
  suppressWarnings({ # Suppress warning of Inf -> NA
    int_mat <- matrix(as.integer(mat), nrow = nrow(mat), ncol = ncol(mat))
  })
  int_sp_mat <- as(int_mat, "dgCMatrix")
  expect_equal(rowTabulates(int_sp_mat), MatrixGenerics::rowTabulates(int_mat))
  values <- c(0, -2, NA, 3, 17)
  expect_equal(rowTabulates(int_sp_mat, values = values), MatrixGenerics::rowTabulates(int_mat, values = values))
  expect_equal(rowTabulates(int_sp_mat, values = values, rows = row_subset, cols = col_subset), MatrixGenerics::rowTabulates(int_mat, values = values, rows = row_subset, cols = col_subset))
})


test_that("rowOrderStats works", {
  no_na_mat <- mat
  no_na_mat[is.na(no_na_mat)] <- 99
  no_na_sp_mat <- as(no_na_mat, "dgCMatrix")

  expect_equal(rowOrderStats(no_na_sp_mat, which = 1), MatrixGenerics::rowOrderStats(no_na_mat, which = 1))
  expect_equal(rowOrderStats(no_na_sp_mat, which = 6), MatrixGenerics::rowOrderStats(no_na_mat, which = 6))
  expect_error(rowOrderStats(no_na_sp_mat, which = 110)) # which should be larger than nrow(no_na_mat)
  expect_error(MatrixGenerics::rowOrderStats(no_na_mat, which = 110))
  expect_equal(rowOrderStats(no_na_sp_mat, which = 1, rows = row_subset, cols = col_subset), MatrixGenerics::rowOrderStats(no_na_mat, which = 1, rows = row_subset, cols = col_subset))
  skip("MatrixGenerics::xxxOrderStats() does not support missing values")
  expect_equal(rowOrderStats(sp_mat, which = 6), MatrixGenerics::rowOrderStats(mat, which = 6))
  expect_equal(rowOrderStats(sp_mat, which = 10, na.rm=TRUE), MatrixGenerics::rowOrderStats(mat, which = 6, na.rm=TRUE))
})



test_that("cumulative functions work", {
  expect_equal(rowCumsums(sp_mat), MatrixGenerics::rowCumsums(mat))
  expect_equal(rowCumprods(sp_mat), MatrixGenerics::rowCumprods(mat))
  expect_equal(rowCummins(sp_mat), MatrixGenerics::rowCummins(mat))
  expect_equal(rowCummaxs(sp_mat), MatrixGenerics::rowCummaxs(mat))

  expect_equal(rowCumsums(sp_mat, useNames = TRUE), MatrixGenerics::rowCumsums(mat, useNames = TRUE))
  expect_equal(rowCumprods(sp_mat, useNames = TRUE), MatrixGenerics::rowCumprods(mat, useNames = TRUE))
  expect_equal(rowCummins(sp_mat, useNames = TRUE), MatrixGenerics::rowCummins(mat, useNames = TRUE))
  expect_equal(rowCummaxs(sp_mat, useNames = TRUE), MatrixGenerics::rowCummaxs(mat, useNames = TRUE))


  expect_equal(rowCumsums(sp_mat, rows = row_subset, cols = col_subset), MatrixGenerics::rowCumsums(mat, rows = row_subset, cols = col_subset))
  expect_equal(rowCumprods(sp_mat, rows = row_subset, cols = col_subset), MatrixGenerics::rowCumprods(mat, rows = row_subset, cols = col_subset))
  expect_equal(rowCummins(sp_mat, rows = row_subset, cols = col_subset), MatrixGenerics::rowCummins(mat, rows = row_subset, cols = col_subset))
  expect_equal(rowCummaxs(sp_mat, rows = row_subset, cols = col_subset), MatrixGenerics::rowCummaxs(mat, rows = row_subset, cols = col_subset))


  # There is no na.rm version
})

test_that("rowIQRs works", {
  expect_equal(rowIQRs(sp_mat), MatrixGenerics::rowIQRs(mat))
  expect_equal(rowIQRs(sp_mat, rows = row_subset, cols = col_subset), MatrixGenerics::rowIQRs(mat, rows = row_subset, cols = col_subset))
})

test_that("rowRanges works", {
  expect_equal(rowRanges(sp_mat), MatrixGenerics::rowRanges(mat))
  expect_equal(rowRanges(sp_mat, rows = row_subset, cols = col_subset), MatrixGenerics::rowRanges(mat, rows = row_subset, cols = col_subset))
})


test_that("rowRanks works", {
  expect_equal(rowRanks(sp_mat), MatrixGenerics::rowRanks(mat))
  expect_equal(rowRanks(sp_mat, ties.method = "average"), MatrixGenerics::rowRanks(mat, ties.method = "average"))

  expect_equal(rowRanks(sp_mat, rows = row_subset, cols = col_subset), MatrixGenerics::rowRanks(mat, rows = row_subset, cols = col_subset))
})



test_that("rowWeightedMeans works", {
  # matrixStats has a bug (#175) that rowWeightedMeans returns a vector
  # without names if w != NULL
  # As a work around, I set the names of my result to NULL as well
  weights <- rnorm(ncol(sp_mat), mean=4, sd=0.1)
  expect_equal(rowWeightedMeans(sp_mat, w=NULL), MatrixGenerics::rowWeightedMeans(mat, w=NULL))
  expect_equal(unname(rowWeightedMeans(sp_mat, w=weights)), MatrixGenerics::rowWeightedMeans(mat, w=weights))
  expect_equal(rowWeightedMeans(sp_mat, na.rm=TRUE, w=weights), MatrixGenerics::rowWeightedMeans(mat, na.rm=TRUE, w=weights))
  expect_equal(unname(rowWeightedMeans(sp_mat, w=weights, rows = row_subset, cols = col_subset)), MatrixGenerics::rowWeightedMeans(mat, w=weights, rows = row_subset, cols = col_subset))
})


test_that("rowWeightedMedians works", {
  weights <- rnorm(ncol(sp_mat), mean=4, sd=0.1)
  expect_equal(rowWeightedMedians(sp_mat, w=weights), MatrixGenerics::rowWeightedMedians(mat, w=weights, interpolate = FALSE))
  expect_equal(rowWeightedMedians(sp_mat, na.rm=TRUE, w=weights), MatrixGenerics::rowWeightedMedians(mat, w=weights, na.rm=TRUE, interpolate = FALSE))
  expect_equal(rowWeightedMedians(sp_mat, w=weights, rows = row_subset, cols = col_subset), MatrixGenerics::rowWeightedMedians(mat, w=weights, interpolate = FALSE, rows = row_subset, cols = col_subset))
})


test_that("rowWeightedMads works", {
  skip("different result than matrixStats version, because sparseMatrixStats uses `interpolate=FALSE`.")
  weights <- rnorm(ncol(sp_mat), mean=4, sd=0.1)
  expect_equal(rowWeightedMads(sp_mat, w=weights), MatrixGenerics::rowWeightedMads(mat, w=weights))
  expect_equal(rowWeightedMads(sp_mat, na.rm=TRUE, w=weights), MatrixGenerics::rowWeightedMads(mat, w=weights, na.rm=TRUE))
  expect_equal(rowWeightedMads(sp_mat, w=weights, rows = row_subset, cols = col_subset), MatrixGenerics::rowWeightedMads(mat, w=weights, rows = row_subset, cols = col_subset))
})


test_that("rowWeightedVars works", {
  weights <- rnorm(ncol(sp_mat), mean=4, sd=0.1)
  expect_equal(rowWeightedVars(sp_mat, w=weights), MatrixGenerics::rowWeightedVars(mat, w=weights))
  expect_equal(rowWeightedVars(sp_mat, na.rm=TRUE), MatrixGenerics::rowWeightedVars(mat, na.rm=TRUE))
  expect_equal(rowWeightedVars(sp_mat, w=weights, rows = row_subset, cols = col_subset), MatrixGenerics::rowWeightedVars(mat, w=weights, rows = row_subset, cols = col_subset))
})


test_that("rowWeightedSds works", {
  weights <- rnorm(ncol(sp_mat), mean=4, sd=0.1)
  expect_equal(rowWeightedSds(sp_mat, w=weights), MatrixGenerics::rowWeightedSds(mat, w=weights))
  expect_equal(rowWeightedSds(sp_mat, na.rm=TRUE), MatrixGenerics::rowWeightedSds(mat, na.rm=TRUE))
  expect_equal(rowWeightedSds(sp_mat, w=weights, rows = row_subset, cols = col_subset), MatrixGenerics::rowWeightedSds(mat, w=weights, rows = row_subset, cols = col_subset))
})



test_that("rowXXDiffs work", {

  expect_equal(rowDiffs(sp_mat, diff = 1), MatrixGenerics::rowDiffs(mat, diff = 1))
  expect_equal(rowDiffs(sp_mat, diff = 3), MatrixGenerics::rowDiffs(mat, diff = 3))
  expect_equal(rowDiffs(sp_mat, diff = 3, lag= 2), MatrixGenerics::rowDiffs(mat, diff = 3, lag = 2))
  expect_equal(rowDiffs(sp_mat, diff = 1, rows = row_subset, cols = col_subset), MatrixGenerics::rowDiffs(mat, diff = 1, rows = row_subset, cols = col_subset))

  expect_equal(rowVarDiffs(sp_mat, diff = 0), MatrixGenerics::rowVarDiffs(mat, diff = 0))
  expect_equal(rowVarDiffs(sp_mat, diff = 1), MatrixGenerics::rowVarDiffs(mat, diff = 1))
  expect_equal(rowVarDiffs(sp_mat, diff = 3), MatrixGenerics::rowVarDiffs(mat, diff = 3))
  expect_equal(rowVarDiffs(sp_mat, na.rm=TRUE), MatrixGenerics::rowVarDiffs(mat, na.rm=TRUE))
  expect_equal(rowVarDiffs(sp_mat, diff = 0, rows = row_subset, cols = col_subset), MatrixGenerics::rowVarDiffs(mat, diff = 0, rows = row_subset, cols = col_subset))

  expect_equal(rowSdDiffs(sp_mat, diff = 0), MatrixGenerics::rowSdDiffs(mat, diff = 0))
  expect_equal(rowSdDiffs(sp_mat, diff = 1), MatrixGenerics::rowSdDiffs(mat, diff = 1))
  expect_equal(rowSdDiffs(sp_mat, diff = 3), MatrixGenerics::rowSdDiffs(mat, diff = 3))
  expect_equal(rowSdDiffs(sp_mat, na.rm=TRUE), MatrixGenerics::rowSdDiffs(mat, na.rm=TRUE))
  expect_equal(rowSdDiffs(sp_mat, diff = 0, rows = row_subset, cols = col_subset), MatrixGenerics::rowSdDiffs(mat, diff = 0, rows = row_subset, cols = col_subset))

  expect_equal(rowMadDiffs(sp_mat, diff = 0), MatrixGenerics::rowMadDiffs(mat, diff = 0))
  expect_equal(rowMadDiffs(sp_mat, diff = 1), MatrixGenerics::rowMadDiffs(mat, diff = 1))
  expect_equal(rowMadDiffs(sp_mat, diff = 3), MatrixGenerics::rowMadDiffs(mat, diff = 3))
  expect_equal(rowMadDiffs(sp_mat, na.rm=TRUE), MatrixGenerics::rowMadDiffs(mat, na.rm=TRUE))
  expect_equal(rowMadDiffs(sp_mat, diff = 0, rows = row_subset, cols = col_subset), MatrixGenerics::rowMadDiffs(mat, diff = 0, rows = row_subset, cols = col_subset))

  expect_equal(rowIQRDiffs(sp_mat, diff = 0), MatrixGenerics::rowIQRDiffs(mat, diff = 0))
  expect_equal(rowIQRDiffs(sp_mat, diff = 1), MatrixGenerics::rowIQRDiffs(mat, diff = 1))
  expect_equal(rowIQRDiffs(sp_mat, diff = 3), MatrixGenerics::rowIQRDiffs(mat, diff = 3))
  expect_equal(rowIQRDiffs(sp_mat, na.rm=TRUE), MatrixGenerics::rowIQRDiffs(mat, na.rm=TRUE))
  expect_equal(rowIQRDiffs(sp_mat, diff = 0, rows = row_subset, cols = col_subset), MatrixGenerics::rowIQRDiffs(mat, diff = 0, rows = row_subset, cols = col_subset))
})


test_that("rowCollapse works", {

  expect_equal(rowCollapse(sp_mat, idxs = 1), MatrixGenerics::rowCollapse(mat, idxs = 1))
  expect_equal(rowCollapse(sp_mat, idxs = c(1,3)), MatrixGenerics::rowCollapse(mat, idxs = c(1,3)))
  expect_equal(rowCollapse(sp_mat, idxs = 1:5, rows = 3), MatrixGenerics::rowCollapse(mat, idxs = 1:5, rows = 3))
  expect_equal(rowCollapse(sp_mat, idxs = 1, rows = row_subset), unname(mat[row_subset, 1]))
  expect_equal(rowCollapse(sp_mat, idxs = 1, rows = row_subset), MatrixGenerics::rowCollapse(mat, idxs = 1, rows = row_subset))

})


test_that("rowAvgsPerColSet works", {
  S <-  suppressWarnings(matrix(seq_len(ncol(mat)), ncol = 2))
  expect_equal(rowAvgsPerColSet(sp_mat, S = S, na.rm = TRUE), MatrixGenerics::rowAvgsPerColSet(mat, S = S, na.rm = TRUE))
  expect_equal(rowAvgsPerColSet(sp_mat, S = S, FUN = rowVarDiffs, na.rm = FALSE), MatrixGenerics::rowAvgsPerColSet(mat, S = S, FUN = rowVarDiffs, na.rm = FALSE))
  expect_equal(rowAvgsPerColSet(sp_mat, S = S, na.rm = FALSE, rows = row_subset), MatrixGenerics::rowAvgsPerColSet(mat, S = S, na.rm = FALSE, rows = row_subset))
})


