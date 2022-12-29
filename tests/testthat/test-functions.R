set.seed(1)
# source("~/prog/r_packages/sparseMatrixStats/tests/testthat/setup.R")
diverse_mat <- make_matrix_with_all_features(nrow = 15, ncol=10)
zero_row_mat <- matrix(numeric(0), nrow = 0, ncol = 5)
zero_col_mat <- matrix(numeric(0), nrow = 5, ncol = 0)
empty_mat <- matrix(numeric(0), nrow=0, ncol=5)
matrix_with_zeros_only <- matrix(0, nrow = 15, ncol=10)
matrix_with_large_numbers <- make_matrix_with_all_features(nrow = 15, ncol=10)
matrix_with_large_numbers[5,] <- 1e10
matrix_with_large_numbers[6,] <- 1e10 + 3
matrix_with_large_numbers[7,] <- 1e10 - 2
matrix_with_large_numbers[8,] <- 1e10 - 10
matrix_with_large_numbers[,7] <- 1:15 + 1e10
dense_mat <- make_matrix_with_all_features(nrow = 15, ncol = 10) + 1
dense_mat <- rbind(dense_mat, 4)
all_inf_mat <-  matrix(c(Inf, -Inf,  Inf, -Inf, -Inf,  Inf,
                         -Inf,  Inf, Inf, -Inf,  Inf, -Inf ),
                       ncol=4)
mat_with_explicit_zeros_sp <- make_matrix_with_all_features_with_explicit_zeros(nrow = 15, ncol = 10)

colnames(diverse_mat) <- LETTERS[seq_len(ncol(diverse_mat))]
colnames(zero_row_mat) <- LETTERS[seq_len(ncol(zero_row_mat))]
colnames(zero_col_mat) <- LETTERS[seq_len(ncol(zero_col_mat))]
colnames(empty_mat) <- LETTERS[seq_len(ncol(empty_mat))]
colnames(matrix_with_zeros_only) <- LETTERS[seq_len(ncol(matrix_with_zeros_only))]
colnames(matrix_with_large_numbers) <- LETTERS[seq_len(ncol(matrix_with_large_numbers))]
colnames(dense_mat) <- LETTERS[seq_len(ncol(dense_mat))]
colnames(all_inf_mat) <- LETTERS[seq_len(ncol(all_inf_mat))]
colnames(mat_with_explicit_zeros_sp) <- LETTERS[seq_len(ncol(mat_with_explicit_zeros_sp))]


matrix_list <- list(diverse_mat,
                    zero_row_mat,
                    zero_col_mat,
                    empty_mat,
                    matrix_with_zeros_only,
                    matrix_with_large_numbers,
                    dense_mat,
                    all_inf_mat,
                    as.matrix(mat_with_explicit_zeros_sp))
sp_matrix_list <- list(as(diverse_mat, "dgCMatrix"),
                       as(zero_row_mat, "dgCMatrix"),
                       as(zero_col_mat, "dgCMatrix"),
                       as(empty_mat, "dgCMatrix"),
                       as(matrix_with_zeros_only, "dgCMatrix"),
                       as(matrix_with_large_numbers, "dgCMatrix"),
                       as(dense_mat, "dgCMatrix"),
                       as(all_inf_mat, "dgCMatrix"),
                       mat_with_explicit_zeros_sp)
row_subset_list <- list(1:5, NULL, 1:2, NULL, c(3,7, 1), 1:15, 3:16, c(1,3), NULL)
col_subset_list <- list(c(7, 9, 2), 1:4, NULL, NULL, 3, 1:10, NULL, NULL, 2)
use_names_list <- list(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE)
descriptions <- list("diverse",
                     "zero row",
                     "zero col",
                     "empty",
                     "only zeros inside",
                     "numerical precision challenge",
                     "dense matrix",
                     "plus/minus Inf",
                     "explicit zeros")


for(idx in seq_along(matrix_list)){

  testthat::context(descriptions[[idx]])
  mat <- matrix_list[[idx]]
  sp_mat <- sp_matrix_list[[idx]]
  row_subset <- row_subset_list[[idx]]
  col_subset <- col_subset_list[[idx]]
  use_names_opt <- use_names_list[[idx]]

  test_that("colSums works", {
    expect_equal(colSums2(sp_mat), matrixStats::colSums2(mat))
    expect_equal(colSums2(sp_mat, na.rm=TRUE), matrixStats::colSums2(mat, na.rm=TRUE))
    expect_equal(colSums2(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt), matrixStats::colSums2(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })

  test_that("rowSums works", {
    sp_mat2 <- t(sp_mat)
    expect_equal(rowSums2(sp_mat2), matrixStats::colSums2(mat))
    expect_equal(rowSums2(sp_mat2, na.rm=TRUE), matrixStats::colSums2(mat, na.rm=TRUE))
    expect_equal(rowSums2(sp_mat2, cols = row_subset, rows = col_subset, useNames = use_names_opt), matrixStats::colSums2(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })



  test_that("colMeans works", {
    expect_equal(colMeans2(sp_mat), matrixStats::colMeans2(mat))
    expect_equal(colMeans2(sp_mat, na.rm=TRUE), matrixStats::colMeans2(mat, na.rm=TRUE))
    expect_equal(colMeans2(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colMeans2(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })

  test_that("rowMeans works", {
    sp_mat2 <- t(sp_mat)
    expect_equal(rowMeans2(sp_mat2), matrixStats::colMeans2(mat))
    expect_equal(rowMeans2(sp_mat2, na.rm=TRUE), matrixStats::colMeans2(mat, na.rm=TRUE))
    expect_equal(rowMeans2(sp_mat2, cols = row_subset, rows = col_subset, useNames = use_names_opt),
                 matrixStats::colMeans2(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })

  test_that("colMedians works", {
    expect_equal(colMedians(sp_mat), matrixStats::colMedians(mat))
    expect_equal(colMedians(sp_mat, na.rm=TRUE), matrixStats::colMedians(mat, na.rm=TRUE))
    expect_equal(colMedians(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colMedians(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })



  test_that("colVars works", {
    expect_equal(colVars(sp_mat), matrixStats::colVars(mat))
    expect_equal(colVars(sp_mat, na.rm=TRUE), matrixStats::colVars(mat, na.rm=TRUE))
    expect_equal(colVars(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colVars(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))

    center <- colMeans2(sp_mat)
    expect_equal(colVars(sp_mat, center = center), matrixStats::colVars(mat, center = center))
  })

  test_that("rowVars works", {
    sp_mat2 <- t(sp_mat)
    expect_equal(rowVars(sp_mat2), matrixStats::colVars(mat))
    expect_equal(rowVars(sp_mat2, na.rm=TRUE), matrixStats::colVars(mat, na.rm=TRUE))
    expect_equal(rowVars(sp_mat2, cols = row_subset, rows = col_subset, useNames = use_names_opt),
                 matrixStats::colVars(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })

  test_that("colSds works", {
    expect_equal(colSds(sp_mat), matrixStats::colSds(mat))
    expect_equal(colSds(sp_mat, na.rm=TRUE), matrixStats::colSds(mat, na.rm=TRUE))
    expect_equal(colSds(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colSds(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))

    center <- colMeans2(sp_mat)
    expect_equal(colSds(sp_mat, center = center), matrixStats::colSds(mat, center = center))
  })

  test_that("colMads works", {
    expect_equal(colMads(sp_mat), matrixStats::colMads(mat))
    expect_equal(colMads(sp_mat, na.rm=TRUE), matrixStats::colMads(mat, na.rm=TRUE))
    expect_equal(colMads(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colMads(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))

    center <- colMeans2(sp_mat)
    expect_equal(colMads(sp_mat, center = center), matrixStats::colMads(mat, center = center))
  })

  test_that("colMins works", {
    expect_equal(colMins(sp_mat), matrixStats::colMins(mat))
    expect_equal(colMins(sp_mat, na.rm=TRUE), matrixStats::colMins(mat, na.rm=TRUE))
    expect_equal(colMins(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colMins(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })


  test_that("colMaxs works", {
    expect_equal(colMaxs(sp_mat), matrixStats::colMaxs(mat))
    expect_equal(colMaxs(sp_mat, na.rm=TRUE), matrixStats::colMaxs(mat, na.rm=TRUE))
    expect_equal(colMaxs(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colMaxs(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })



  test_that("colCounts works", {
    expect_equal(colCounts(sp_mat, value=0), matrixStats::colCounts(mat, value=0))
    expect_equal(colCounts(sp_mat, na.rm=TRUE, value=0), matrixStats::colCounts(mat, na.rm=TRUE, value = 0))
    expect_equal(colCounts(sp_mat, value = 42), matrixStats::colCounts(mat, value = 42))
    expect_equal(colCounts(sp_mat, na.rm=TRUE, value = 42), matrixStats::colCounts(mat, na.rm=TRUE, value = 42))
    expect_equal(colCounts(sp_mat, value=0, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colCounts(mat, value=0, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })


  test_that("colAnyNAs works", {
    expect_equal(colAnyNAs(sp_mat), matrixStats::colAnyNAs(mat))
    expect_equal(colAnyNAs(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colAnyNAs(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })


  test_that("colAnys works", {
    expect_equal(colAnys(sp_mat), matrixStats::colAnys(mat))
    expect_equal(colAnys(sp_mat, na.rm=TRUE), matrixStats::colAnys(mat, na.rm=TRUE))
    # as.logical(mat) is necessary, because matrixStats has a bug for this function
    # expect_equal(colAnys(sp_mat, value = FALSE), matrixStats::colAnys(array(as.logical(mat), dim = dim(mat)), value = FALSE))
    expect_equal(colAnys(sp_mat, na.rm=TRUE, value = FALSE), matrixStats::colAnys(array(as.logical(mat), dim = dim(mat)), na.rm=TRUE, value = FALSE))
    expect_equal(colAnys(sp_mat, value=0), matrixStats::colAnys(mat, value=0))
    expect_equal(colAnys(sp_mat, na.rm=TRUE, value=0), matrixStats::colAnys(mat, na.rm=TRUE, value = 0))
    # expect_equal(colAnys(sp_mat, value=NA), matrixStats::colAnys(mat, value=NA))
    # expect_equal(colAnys(sp_mat, na.rm=TRUE, value=NA), matrixStats::colAnys(mat, na.rm=TRUE, value = NA))
    expect_equal(colAnys(sp_mat, value = 42), matrixStats::colAnys(mat, value = 42))
    expect_equal(colAnys(sp_mat, na.rm=TRUE, value = 42), matrixStats::colAnys(mat, na.rm=TRUE, value = 42))
    expect_equal(colAnys(sp_mat, value=0, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colAnys(mat, value=0, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })


  test_that("colAlls works", {
    expect_equal(colAlls(sp_mat), matrixStats::colAlls(mat))
    expect_equal(colAlls(sp_mat, na.rm=TRUE), matrixStats::colAlls(mat, na.rm=TRUE))
    # expect_equal(colAlls(sp_mat, value = FALSE), matrixStats::colAlls(array(as.logical(mat), dim = dim(mat)), value = FALSE))
    expect_equal(colAlls(sp_mat, na.rm=TRUE, value = FALSE), matrixStats::colAlls(array(as.logical(mat), dim = dim(mat)), na.rm=TRUE, value = FALSE))
    expect_equal(colAlls(sp_mat, value=0), matrixStats::colAlls(mat, value=0))
    expect_equal(colAlls(sp_mat, na.rm=TRUE, value=0), matrixStats::colAlls(mat, na.rm=TRUE, value = 0))
    # expect_equal(colAnys(sp_mat, value=NA), matrixStats::colAnys(mat, value=NA))
    # expect_equal(colAnys(sp_mat, na.rm=TRUE, value=NA), matrixStats::colAnys(mat, na.rm=TRUE, value = NA))
    expect_equal(colAlls(sp_mat, value = 42), matrixStats::colAlls(mat, value = 42))
    expect_equal(colAlls(sp_mat, na.rm=TRUE, value = 42), matrixStats::colAlls(mat, na.rm=TRUE, value = 42))
    expect_equal(colAlls(sp_mat, value=0, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colAlls(mat, value=0, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })


  test_that("colLogSumExps works", {
    expect_equal(colLogSumExps(sp_mat), matrixStats::colLogSumExps(mat))
    expect_equal(colLogSumExps(sp_mat, na.rm=TRUE), matrixStats::colLogSumExps(mat, na.rm=TRUE))
    expect_equal(colLogSumExps(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colLogSumExps(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })


  test_that("colProds works", {
    expect_equal(colProds(sp_mat), matrixStats::colProds(mat))
    expect_equal(colProds(sp_mat, na.rm=TRUE), matrixStats::colProds(mat, na.rm=TRUE))
    expect_equal(colProds(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colProds(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })

  test_that("colQuantiles works", {
    expect_equal(colQuantiles(sp_mat), matrixStats::colQuantiles(mat))
    expect_equal(colQuantiles(sp_mat, na.rm=TRUE), matrixStats::colQuantiles(mat, na.rm=TRUE))
    expect_equal(colQuantiles(sp_mat, prob = 0.3, na.rm=TRUE), matrixStats::colQuantiles(mat, prob = 0.3, na.rm=TRUE))
    expect_equal(colQuantiles(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colQuantiles(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))

    expect_equal(colQuantiles(sp_mat, type = 1L), matrixStats::colQuantiles(mat, type = 1L))
    expect_equal(colQuantiles(sp_mat, type = 2L), matrixStats::colQuantiles(mat, type = 2L))
    expect_equal(colQuantiles(sp_mat, type = 3L), matrixStats::colQuantiles(mat, type = 3L))

    expect_equal(colQuantiles(sp_mat, type = 4L), matrixStats::colQuantiles(mat, type = 4L))
    expect_equal(colQuantiles(sp_mat, type = 5L), matrixStats::colQuantiles(mat, type = 5L))
    expect_equal(colQuantiles(sp_mat, type = 6L), matrixStats::colQuantiles(mat, type = 6L))
    expect_equal(colQuantiles(sp_mat, type = 7L), matrixStats::colQuantiles(mat, type = 7L))
    expect_equal(colQuantiles(sp_mat, type = 8L), matrixStats::colQuantiles(mat, type = 8L))
    expect_equal(colQuantiles(sp_mat, type = 9L), matrixStats::colQuantiles(mat, type = 9L))
  })


  test_that("colTabulates works", {
    suppressWarnings({ # Suppress warning of Inf -> NA
      int_mat <- matrix(as.integer(mat), nrow = nrow(mat), ncol = ncol(mat))
    })
    int_sp_mat <- as(int_mat, "dgCMatrix")
    expect_equal(colTabulates(int_sp_mat), matrixStats::colTabulates(int_mat))
    expect_equal(colTabulates(int_sp_mat, values = integer(0L)), matrixStats::colTabulates(int_mat, values = integer(0L)))
    values <- c(0, -2, NA, 3, 17)
    expect_equal(colTabulates(int_sp_mat, values = values), matrixStats::colTabulates(int_mat, values = values))
    expect_equal(colTabulates(int_sp_mat, values = c(1, values)), matrixStats::colTabulates(int_mat, values = c(1, values)))
    expect_equal(colTabulates(int_sp_mat, values = c(1, 1, values)), matrixStats::colTabulates(int_mat, values = c(1, 1, values)))
    expect_equal(colTabulates(int_sp_mat, values = values[-1]), matrixStats::colTabulates(int_mat, values = values[-1]))
    expect_equal(colTabulates(int_sp_mat, values = values, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colTabulates(int_mat, values = values, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })


  test_that("colOrderStats works", {
    no_na_mat <- mat
    no_na_mat[is.na(no_na_mat)] <- 99
    no_na_sp_mat <- as(no_na_mat, "dgCMatrix")

    if(nrow(no_na_mat) >= 6){
      expect_equal(colOrderStats(no_na_sp_mat, which = 1), matrixStats::colOrderStats(no_na_mat, which = 1))
      expect_equal(colOrderStats(no_na_sp_mat, which = 6), matrixStats::colOrderStats(no_na_mat, which = 6))
      expect_equal(colOrderStats(no_na_sp_mat, which = 1, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                   matrixStats::colOrderStats(no_na_mat, which = 1, rows = row_subset, cols = col_subset, useNames = use_names_opt))
    }
    expect_error(colOrderStats(no_na_sp_mat, which = 110)) # which should be larger than nrow(no_na_mat)
    expect_error(matrixStats::colOrderStats(no_na_mat, which = 110))
    skip("matrixStats::xxxOrderStats() does not support missing values")
    expect_equal(colOrderStats(sp_mat, which = 6), matrixStats::colOrderStats(mat, which = 6))
    expect_equal(colOrderStats(sp_mat, which = 10, na.rm=TRUE), matrixStats::colOrderStats(mat, which = 6, na.rm=TRUE))
  })



  test_that("cumulative functions work", {
    expect_equal(colCumsums(sp_mat), matrixStats::colCumsums(mat))
    expect_equal(colCumprods(sp_mat), matrixStats::colCumprods(mat))
    expect_equal(colCummins(sp_mat), matrixStats::colCummins(mat))
    expect_equal(colCummaxs(sp_mat), matrixStats::colCummaxs(mat))

    expect_equal(colCumsums(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colCumsums(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
    expect_equal(colCumprods(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colCumprods(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
    expect_equal(colCummins(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colCummins(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
    expect_equal(colCummaxs(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colCummaxs(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
    # There is no na.rm version
  })


  test_that("colIQRs works", {
    expect_equal(colIQRs(sp_mat), matrixStats::colIQRs(mat))
    expect_equal(colIQRs(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colIQRs(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })

  test_that("colRanges works", {
    expect_equal(colRanges(sp_mat), matrixStats::colRanges(mat))
    expect_equal(colRanges(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colRanges(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })

  test_that("colRanks works", {
    expect_equal(colRanks(sp_mat), matrixStats::colRanks(mat))
    expect_equal(colRanks(sp_mat, ties.method = "average"), matrixStats::colRanks(mat, ties.method = "average"))
    expect_equal(colRanks(sp_mat, ties.method = "min"), matrixStats::colRanks(mat, ties.method = "min"))
    expect_equal(colRanks(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colRanks(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })



  test_that("colWeightedMeans works", {
    weights <- rnorm(nrow(sp_mat), mean=4, sd=0.1)
    expect_equal(colWeightedMeans(sp_mat, w=weights), matrixStats::colWeightedMeans(mat, w=weights))
    expect_equal(colWeightedMeans(sp_mat, na.rm=TRUE, w=weights), matrixStats::colWeightedMeans(mat, na.rm=TRUE, w=weights))
    expect_equal(colWeightedMeans(sp_mat, w=weights, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colWeightedMeans(mat, w=weights, rows = row_subset, cols = col_subset, useNames = use_names_opt))

    # Test check for length of w
    expect_error(colWeightedMeans(sp_mat, w=1:42))
    expect_error(matrixStats::colWeightedMeans(mat, w=1:42))
  })


  test_that("colWeightedMedians works", {
    weights <- rnorm(nrow(sp_mat), mean=4, sd=0.1)
    expect_equal(colWeightedMedians(sp_mat), matrixStats::colWeightedMedians(mat, interpolate = FALSE))
    expect_equal(colWeightedMedians(sp_mat, w=weights), matrixStats::colWeightedMedians(mat, w=weights, interpolate = FALSE))
    expect_equal(colWeightedMedians(sp_mat, na.rm=TRUE, w=weights), matrixStats::colWeightedMedians(mat, w=weights, na.rm=TRUE, interpolate = FALSE))
    expect_equal(colWeightedMedians(sp_mat, w=weights, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colWeightedMedians(mat, w=weights, interpolate = FALSE, rows = row_subset, cols = col_subset, useNames = use_names_opt))
    # The presence of the weight argument affects the default naming of the result
    expect_equal(colWeightedMedians(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colWeightedMedians(mat, interpolate = FALSE, rows = row_subset, cols = col_subset, useNames = use_names_opt))

    # Test check for length of w
    expect_error(colWeightedMeans(sp_mat, w=1:42))
    expect_error(matrixStats::colWeightedMeans(mat, w=1:42))
  })


  test_that("colWeightedMads works", {
    expect_equal(colWeightedMads(sp_mat), matrixStats::colWeightedMads(mat))
    expect_equal(colWeightedMads(sp_mat, na.rm=TRUE), matrixStats::colWeightedMads(mat, na.rm=TRUE))

    weights <- rep(1, nrow(sp_mat))
    expect_equal(colWeightedMads(sp_mat, w=weights), matrixStats::colWeightedMads(mat, w=weights))
    expect_equal(colWeightedMads(sp_mat, na.rm=TRUE, w=weights), matrixStats::colWeightedMads(mat, w=weights, na.rm=TRUE))

    # Test check for length of w
    expect_error(colWeightedMeans(sp_mat, w=1:42))
    expect_error(matrixStats::colWeightedMeans(mat, w=1:42))

    skip("different result than matrixStats version, because sparseMatrixStats uses `interpolate=FALSE`.")
    weights <- rnorm(nrow(sp_mat), mean=4, sd=0.1)
    expect_equal(colWeightedMads(sp_mat, w=weights), matrixStats::colWeightedMads(mat, w=weights))
    expect_equal(colWeightedMads(sp_mat, na.rm=TRUE, w=weights), matrixStats::colWeightedMads(mat, w=weights, na.rm=TRUE))
  })


  test_that("colWeightedVars works", {
    weights <- rnorm(nrow(sp_mat), mean=4, sd=0.1)
    expect_equal(colWeightedVars(sp_mat, w=weights), matrixStats::colWeightedVars(mat, w=weights))
    expect_equal(colWeightedVars(sp_mat, na.rm=TRUE), matrixStats::colWeightedVars(mat, na.rm=TRUE))
    expect_equal(colWeightedVars(sp_mat, w=weights, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colWeightedVars(mat, w=weights, rows = row_subset, cols = col_subset, useNames = use_names_opt))

    # Test check for length of w
    expect_error(colWeightedMeans(sp_mat, w=1:42))
    expect_error(matrixStats::colWeightedMeans(mat, w=1:42))
  })


  test_that("colWeightedSds works", {
    weights <- rnorm(nrow(sp_mat), mean=4, sd=0.1)
    expect_equal(colWeightedSds(sp_mat, w=weights), matrixStats::colWeightedSds(mat, w=weights))
    expect_equal(colWeightedSds(sp_mat, na.rm=TRUE), matrixStats::colWeightedSds(mat, na.rm=TRUE))
    expect_equal(colWeightedSds(sp_mat, w=weights, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colWeightedSds(mat, w=weights, rows = row_subset, cols = col_subset, useNames = use_names_opt))

    # Test check for length of w
    expect_error(colWeightedMeans(sp_mat, w=1:42))
    expect_error(matrixStats::colWeightedMeans(mat, w=1:42))
  })





  test_that("colXXdiff methods works", {
    expect_equal(colDiffs(sp_mat, diff = 1), matrixStats::colDiffs(mat, diff = 1))
    expect_equal(colDiffs(sp_mat, diff = 3), matrixStats::colDiffs(mat, diff = 3))
    expect_equal(colDiffs(sp_mat, diff = 3, lag= 2), matrixStats::colDiffs(mat, diff = 3, lag = 2))
    expect_equal(colDiffs(sp_mat, diff = 1, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colDiffs(mat, diff = 1, rows = row_subset, cols = col_subset, useNames = use_names_opt))

    expect_equal(colVarDiffs(sp_mat, diff = 0), matrixStats::colVarDiffs(mat, diff = 0))
    expect_equal(colVarDiffs(sp_mat, diff = 1), matrixStats::colVarDiffs(mat, diff = 1))
    expect_equal(colVarDiffs(sp_mat, diff = 3), matrixStats::colVarDiffs(mat, diff = 3))
    expect_equal(colVarDiffs(sp_mat, diff = 0, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colVarDiffs(mat, diff = 0, rows = row_subset, cols = col_subset, useNames = use_names_opt))

    expect_equal(colSdDiffs(sp_mat, diff = 0), matrixStats::colSdDiffs(mat, diff = 0))
    expect_equal(colSdDiffs(sp_mat, diff = 1), matrixStats::colSdDiffs(mat, diff = 1))
    expect_equal(colSdDiffs(sp_mat, diff = 3), matrixStats::colSdDiffs(mat, diff = 3))
    expect_equal(colSdDiffs(sp_mat, na.rm=TRUE), matrixStats::colSdDiffs(mat, na.rm=TRUE))
    expect_equal(colSdDiffs(sp_mat, diff = 0, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colSdDiffs(mat, diff = 0, rows = row_subset, cols = col_subset, useNames = use_names_opt))

    expect_equal(colMadDiffs(sp_mat, diff = 0), matrixStats::colMadDiffs(mat, diff = 0))
    expect_equal(colMadDiffs(sp_mat, diff = 1), matrixStats::colMadDiffs(mat, diff = 1))
    expect_equal(colMadDiffs(sp_mat, diff = 3), matrixStats::colMadDiffs(mat, diff = 3))
    expect_equal(colMadDiffs(sp_mat, na.rm=TRUE), matrixStats::colMadDiffs(mat, na.rm=TRUE))
    expect_equal(colMadDiffs(sp_mat, diff = 0, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colMadDiffs(mat, diff = 0, rows = row_subset, cols = col_subset, useNames = use_names_opt))

    expect_equal(colIQRDiffs(sp_mat, diff = 0), matrixStats::colIQRDiffs(mat, diff = 0))
    if(descriptions[[idx]] != "plus/minus Inf"){
      # This might be a bug in matrixStats. It should probably return NA's
      expect_equal(colIQRDiffs(sp_mat, diff = 1), matrixStats::colIQRDiffs(mat, diff = 1))
      expect_equal(colIQRDiffs(sp_mat, na.rm=TRUE), matrixStats::colIQRDiffs(mat, na.rm=TRUE))
    }
    expect_equal(colIQRDiffs(sp_mat, diff = 3), matrixStats::colIQRDiffs(mat, diff = 3))
    expect_equal(colIQRDiffs(sp_mat, diff = 0, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colIQRDiffs(mat, diff = 0, rows = row_subset, cols = col_subset, useNames = use_names_opt))

  })


  test_that("colCollapse works", {

    expect_equal(colCollapse(sp_mat, idxs = 1), matrixStats::colCollapse(mat, idxs = 1))
    expect_equal(colCollapse(sp_mat, idxs = c(1,3)), matrixStats::colCollapse(mat, idxs = c(1,3)))
    expect_equal(colCollapse(sp_mat, idxs = 1:5, cols = min(ncol(mat), 3)), matrixStats::colCollapse(mat, idxs = 1:5, cols = min(ncol(mat), 3)))
    if(nrow(sp_mat) > 0 && ! is.null(col_subset)){
      expect_equal(colCollapse(sp_mat, idxs = 1, cols = col_subset), unname(sp_mat[1, col_subset]))
    }
    expect_equal(colCollapse(sp_mat, idxs = 1, cols = col_subset, useNames = use_names_opt),
                 matrixStats::colCollapse(mat, idxs = 1, cols = col_subset, useNames = use_names_opt))
  })


  test_that("colAvgsPerRowSet works", {
    S <-  suppressWarnings(matrix(seq_len(nrow(mat)), ncol = 2))
    expect_equal(colAvgsPerRowSet(sp_mat, S = S), matrixStats::colAvgsPerRowSet(mat, S = S))
    expect_equal(colAvgsPerRowSet(sp_mat, S = S, FUN = colVarDiffs, na.rm = FALSE), matrixStats::colAvgsPerRowSet(mat, S = S, FUN = colVarDiffs, na.rm = FALSE))
    expect_equal(colAvgsPerRowSet(sp_mat, S = S, na.rm = FALSE, cols = col_subset),
                 matrixStats::colAvgsPerRowSet(mat, S = S, na.rm = FALSE, cols = col_subset))
  })

}
