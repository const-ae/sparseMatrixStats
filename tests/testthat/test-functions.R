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

rownames(diverse_mat) <- paste0("row_", seq_len(nrow(diverse_mat)))
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
    expect_equal(colSums2(sp_mat), MatrixGenerics::colSums2(mat))
    expect_equal(colSums2(sp_mat, na.rm=TRUE), MatrixGenerics::colSums2(mat, na.rm=TRUE))
    expect_equal(colSums2(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt), MatrixGenerics::colSums2(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })

  test_that("rowSums works", {
    sp_mat2 <- t(sp_mat)
    expect_equal(rowSums2(sp_mat2), MatrixGenerics::colSums2(mat))
    expect_equal(rowSums2(sp_mat2, na.rm=TRUE), MatrixGenerics::colSums2(mat, na.rm=TRUE))
    expect_equal(rowSums2(sp_mat2, cols = row_subset, rows = col_subset, useNames = use_names_opt), MatrixGenerics::colSums2(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })



  test_that("colMeans works", {
    expect_equal(colMeans2(sp_mat), MatrixGenerics::colMeans2(mat))
    expect_equal(colMeans2(sp_mat, na.rm=TRUE), MatrixGenerics::colMeans2(mat, na.rm=TRUE))
    expect_equal(colMeans2(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colMeans2(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })

  test_that("rowMeans works", {
    sp_mat2 <- t(sp_mat)
    expect_equal(rowMeans2(sp_mat2), MatrixGenerics::colMeans2(mat))
    expect_equal(rowMeans2(sp_mat2, na.rm=TRUE), MatrixGenerics::colMeans2(mat, na.rm=TRUE))
    expect_equal(rowMeans2(sp_mat2, cols = row_subset, rows = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colMeans2(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })

  test_that("colMedians works", {
    expect_equal(colMedians(sp_mat), MatrixGenerics::colMedians(mat))
    expect_equal(colMedians(sp_mat, na.rm=TRUE), MatrixGenerics::colMedians(mat, na.rm=TRUE))
    expect_equal(colMedians(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colMedians(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })



  test_that("colVars works", {
    expect_equal(colVars(sp_mat), MatrixGenerics::colVars(mat))
    expect_equal(colVars(sp_mat, na.rm=TRUE), MatrixGenerics::colVars(mat, na.rm=TRUE))
    expect_equal(colVars(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colVars(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))

    center <- colMeans2(sp_mat)
    expect_equal(colVars(sp_mat, center = center), MatrixGenerics::colVars(mat, center = center))
  })

  test_that("rowVars works", {
    sp_mat2 <- t(sp_mat)
    expect_equal(rowVars(sp_mat2), MatrixGenerics::colVars(mat))
    expect_equal(rowVars(sp_mat2, na.rm=TRUE), MatrixGenerics::colVars(mat, na.rm=TRUE))
    expect_equal(rowVars(sp_mat2, cols = row_subset, rows = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colVars(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })

  test_that("colSds works", {
    expect_equal(colSds(sp_mat), MatrixGenerics::colSds(mat))
    expect_equal(colSds(sp_mat, na.rm=TRUE), MatrixGenerics::colSds(mat, na.rm=TRUE))
    expect_equal(colSds(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colSds(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))

    center <- colMeans2(sp_mat)
    expect_equal(colSds(sp_mat, center = center), MatrixGenerics::colSds(mat, center = center))
  })

  test_that("colMads works", {
    expect_equal(colMads(sp_mat), MatrixGenerics::colMads(mat))
    expect_equal(colMads(sp_mat, na.rm=TRUE), MatrixGenerics::colMads(mat, na.rm=TRUE))
    expect_equal(colMads(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colMads(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))

    center <- colMeans2(sp_mat)
    expect_equal(colMads(sp_mat, center = center), MatrixGenerics::colMads(mat, center = center))
  })

  test_that("colMins works", {
    expect_equal(colMins(sp_mat), MatrixGenerics::colMins(mat))
    expect_equal(colMins(sp_mat, na.rm=TRUE), MatrixGenerics::colMins(mat, na.rm=TRUE))
    expect_equal(colMins(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colMins(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })


  test_that("colMaxs works", {
    expect_equal(colMaxs(sp_mat), MatrixGenerics::colMaxs(mat))
    expect_equal(colMaxs(sp_mat, na.rm=TRUE), MatrixGenerics::colMaxs(mat, na.rm=TRUE))
    expect_equal(colMaxs(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colMaxs(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })



  test_that("colCounts works", {
    expect_equal(colCounts(sp_mat, value=0), MatrixGenerics::colCounts(mat, value=0))
    expect_equal(colCounts(sp_mat, na.rm=TRUE, value=0), MatrixGenerics::colCounts(mat, na.rm=TRUE, value = 0))
    expect_equal(colCounts(sp_mat, value = 42), MatrixGenerics::colCounts(mat, value = 42))
    expect_equal(colCounts(sp_mat, na.rm=TRUE, value = 42), MatrixGenerics::colCounts(mat, na.rm=TRUE, value = 42))
    expect_equal(colCounts(sp_mat, value=0, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colCounts(mat, value=0, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })


  test_that("colAnyNAs works", {
    expect_equal(colAnyNAs(sp_mat), MatrixGenerics::colAnyNAs(mat))
    expect_equal(colAnyNAs(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colAnyNAs(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })


  test_that("colAnys works", {
    expect_equal(colAnys(sp_mat), MatrixGenerics::colAnys(mat))
    expect_equal(colAnys(sp_mat, na.rm=TRUE), MatrixGenerics::colAnys(mat, na.rm=TRUE))
    # as.logical(mat) is necessary, because matrixStats has a bug for this function
    # expect_equal(colAnys(sp_mat, value = FALSE), MatrixGenerics::colAnys(array(as.logical(mat), dim = dim(mat)), value = FALSE))
    expect_equal(colAnys(sp_mat, na.rm=TRUE, value = FALSE), MatrixGenerics::colAnys(mat != 0, na.rm=TRUE, value = FALSE))
    expect_equal(colAnys(sp_mat, value=0), MatrixGenerics::colAnys(mat, value=0))
    expect_equal(colAnys(sp_mat, na.rm=TRUE, value=0), MatrixGenerics::colAnys(mat, na.rm=TRUE, value = 0))
    # expect_equal(colAnys(sp_mat, value=NA), MatrixGenerics::colAnys(mat, value=NA))
    # expect_equal(colAnys(sp_mat, na.rm=TRUE, value=NA), MatrixGenerics::colAnys(mat, na.rm=TRUE, value = NA))
    expect_equal(colAnys(sp_mat, value = 42), MatrixGenerics::colAnys(mat, value = 42))
    expect_equal(colAnys(sp_mat, na.rm=TRUE, value = 42), MatrixGenerics::colAnys(mat, na.rm=TRUE, value = 42))
    expect_equal(colAnys(sp_mat, value=0, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colAnys(mat, value=0, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })


  test_that("colAlls works", {
    expect_equal(colAlls(sp_mat), MatrixGenerics::colAlls(mat))
    expect_equal(colAlls(sp_mat, na.rm=TRUE), MatrixGenerics::colAlls(mat, na.rm=TRUE))
    # expect_equal(colAlls(sp_mat, value = FALSE), MatrixGenerics::colAlls(array(as.logical(mat), dim = dim(mat)), value = FALSE))
    expect_equal(colAlls(sp_mat, na.rm=TRUE, value = FALSE), MatrixGenerics::colAlls(mat != 0, na.rm=TRUE, value = FALSE))
    expect_equal(colAlls(sp_mat, value=0), MatrixGenerics::colAlls(mat, value=0))
    expect_equal(colAlls(sp_mat, na.rm=TRUE, value=0), MatrixGenerics::colAlls(mat, na.rm=TRUE, value = 0))
    # expect_equal(colAnys(sp_mat, value=NA), MatrixGenerics::colAnys(mat, value=NA))
    # expect_equal(colAnys(sp_mat, na.rm=TRUE, value=NA), MatrixGenerics::colAnys(mat, na.rm=TRUE, value = NA))
    expect_equal(colAlls(sp_mat, value = 42), MatrixGenerics::colAlls(mat, value = 42))
    expect_equal(colAlls(sp_mat, na.rm=TRUE, value = 42), MatrixGenerics::colAlls(mat, na.rm=TRUE, value = 42))
    expect_equal(colAlls(sp_mat, value=0, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colAlls(mat, value=0, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })


  test_that("colLogSumExps works", {
    expect_equal(colLogSumExps(sp_mat), MatrixGenerics::colLogSumExps(mat))
    expect_equal(colLogSumExps(sp_mat, na.rm=TRUE), MatrixGenerics::colLogSumExps(mat, na.rm=TRUE))
    expect_equal(colLogSumExps(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colLogSumExps(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })


  test_that("colProds works", {
    expect_equal(colProds(sp_mat), MatrixGenerics::colProds(mat))
    expect_equal(colProds(sp_mat, na.rm=TRUE), MatrixGenerics::colProds(mat, na.rm=TRUE))
    expect_equal(colProds(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colProds(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })

  test_that("colQuantiles works", {
    expect_equal(colQuantiles(sp_mat), MatrixGenerics::colQuantiles(mat))
    expect_equal(colQuantiles(sp_mat, na.rm=TRUE), MatrixGenerics::colQuantiles(mat, na.rm=TRUE))
    expect_equal(colQuantiles(sp_mat, prob = 0.3, na.rm=TRUE), MatrixGenerics::colQuantiles(mat, prob = 0.3, na.rm=TRUE))
    expect_equal(colQuantiles(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colQuantiles(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))

    expect_equal(colQuantiles(sp_mat, type = 1L), MatrixGenerics::colQuantiles(mat, type = 1L))
    expect_equal(colQuantiles(sp_mat, type = 2L), MatrixGenerics::colQuantiles(mat, type = 2L))
    expect_equal(colQuantiles(sp_mat, type = 3L), MatrixGenerics::colQuantiles(mat, type = 3L))

    expect_equal(colQuantiles(sp_mat, type = 4L), MatrixGenerics::colQuantiles(mat, type = 4L))
    expect_equal(colQuantiles(sp_mat, type = 5L), MatrixGenerics::colQuantiles(mat, type = 5L))
    expect_equal(colQuantiles(sp_mat, type = 6L), MatrixGenerics::colQuantiles(mat, type = 6L))
    expect_equal(colQuantiles(sp_mat, type = 7L), MatrixGenerics::colQuantiles(mat, type = 7L))
    expect_equal(colQuantiles(sp_mat, type = 8L), MatrixGenerics::colQuantiles(mat, type = 8L))
    expect_equal(colQuantiles(sp_mat, type = 9L), MatrixGenerics::colQuantiles(mat, type = 9L))
  })


  test_that("colTabulates works", {
    suppressWarnings({ # Suppress warning of Inf -> NA
      int_mat <- matrix(as.integer(mat), nrow = nrow(mat), ncol = ncol(mat))
    })
    int_sp_mat <- as(int_mat, "dgCMatrix")
    expect_equal(colTabulates(int_sp_mat), MatrixGenerics::colTabulates(int_mat))
    expect_equal(colTabulates(int_sp_mat, values = integer(0L)), MatrixGenerics::colTabulates(int_mat, values = integer(0L)))
    values <- c(0, -2, NA, 3, 17)
    expect_equal(colTabulates(int_sp_mat, values = values), MatrixGenerics::colTabulates(int_mat, values = values))
    expect_equal(colTabulates(int_sp_mat, values = c(1, values)), MatrixGenerics::colTabulates(int_mat, values = c(1, values)))
    expect_equal(colTabulates(int_sp_mat, values = c(1, 1, values)), MatrixGenerics::colTabulates(int_mat, values = c(1, 1, values)))
    expect_equal(colTabulates(int_sp_mat, values = values[-1]), MatrixGenerics::colTabulates(int_mat, values = values[-1]))
    expect_equal(colTabulates(int_sp_mat, values = values, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colTabulates(int_mat, values = values, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })


  test_that("colOrderStats works", {
    no_na_mat <- mat
    no_na_mat[is.na(no_na_mat)] <- 99
    no_na_sp_mat <- as(no_na_mat, "dgCMatrix")

    if(nrow(no_na_mat) >= 6){
      expect_equal(colOrderStats(no_na_sp_mat, which = 1), MatrixGenerics::colOrderStats(no_na_mat, which = 1))
      expect_equal(colOrderStats(no_na_sp_mat, which = 6), MatrixGenerics::colOrderStats(no_na_mat, which = 6))
      expect_equal(colOrderStats(no_na_sp_mat, which = 1, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                   MatrixGenerics::colOrderStats(no_na_mat, which = 1, rows = row_subset, cols = col_subset, useNames = use_names_opt))
    }
    expect_error(colOrderStats(no_na_sp_mat, which = 110)) # which should be larger than nrow(no_na_mat)
    expect_error(MatrixGenerics::colOrderStats(no_na_mat, which = 110))
    skip("MatrixGenerics::xxxOrderStats() does not support missing values")
    expect_equal(colOrderStats(sp_mat, which = 6), MatrixGenerics::colOrderStats(mat, which = 6))
    expect_equal(colOrderStats(sp_mat, which = 10, na.rm=TRUE), MatrixGenerics::colOrderStats(mat, which = 6, na.rm=TRUE))
  })



  test_that("cumulative functions work", {
    expect_equal(colCumsums(sp_mat), MatrixGenerics::colCumsums(mat))
    expect_equal(colCumprods(sp_mat), MatrixGenerics::colCumprods(mat))
    expect_equal(colCummins(sp_mat), MatrixGenerics::colCummins(mat))
    expect_equal(colCummaxs(sp_mat), MatrixGenerics::colCummaxs(mat))

    expect_equal(colCumsums(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colCumsums(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
    expect_equal(colCumprods(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colCumprods(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
    expect_equal(colCummins(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colCummins(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
    expect_equal(colCummaxs(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colCummaxs(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
    # There is no na.rm version
  })


  test_that("colIQRs works", {
    expect_equal(colIQRs(sp_mat), MatrixGenerics::colIQRs(mat))
    expect_equal(colIQRs(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colIQRs(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })

  test_that("colRanges works", {
    expect_equal(colRanges(sp_mat), MatrixGenerics::colRanges(mat))
    expect_equal(colRanges(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colRanges(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })

  test_that("colRanks works", {
    expect_equal(colRanks(sp_mat), MatrixGenerics::colRanks(mat))
    expect_equal(colRanks(sp_mat, ties.method = "average"), MatrixGenerics::colRanks(mat, ties.method = "average"))
    expect_equal(colRanks(sp_mat, ties.method = "min"), MatrixGenerics::colRanks(mat, ties.method = "min"))
    expect_equal(colRanks(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colRanks(mat, rows = row_subset, cols = col_subset, useNames = use_names_opt))
  })



  test_that("colWeightedMeans works", {
    weights <- rnorm(nrow(sp_mat), mean=4, sd=0.1)
    expect_equal(colWeightedMeans(sp_mat, w=weights), MatrixGenerics::colWeightedMeans(mat, w=weights))
    expect_equal(colWeightedMeans(sp_mat, na.rm=TRUE, w=weights), MatrixGenerics::colWeightedMeans(mat, na.rm=TRUE, w=weights))
    expect_equal(colWeightedMeans(sp_mat, w=weights, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colWeightedMeans(mat, w=weights, rows = row_subset, cols = col_subset, useNames = use_names_opt))

    # Test check for length of w
    expect_error(colWeightedMeans(sp_mat, w=1:42))
    expect_error(MatrixGenerics::colWeightedMeans(mat, w=1:42))
  })


  test_that("colWeightedMedians works", {
    weights <- rnorm(nrow(sp_mat), mean=4, sd=0.1)
    expect_equal(colWeightedMedians(sp_mat), MatrixGenerics::colWeightedMedians(mat, interpolate = FALSE))
    expect_equal(colWeightedMedians(sp_mat, w=weights), MatrixGenerics::colWeightedMedians(mat, w=weights, interpolate = FALSE))
    expect_equal(colWeightedMedians(sp_mat, na.rm=TRUE, w=weights), MatrixGenerics::colWeightedMedians(mat, w=weights, na.rm=TRUE, interpolate = FALSE))
    expect_equal(colWeightedMedians(sp_mat, w=weights, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colWeightedMedians(mat, w=weights, interpolate = FALSE, rows = row_subset, cols = col_subset, useNames = use_names_opt))
    # The presence of the weight argument affects the default naming of the result
    expect_equal(colWeightedMedians(sp_mat, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colWeightedMedians(mat, interpolate = FALSE, rows = row_subset, cols = col_subset, useNames = use_names_opt))

    # Test check for length of w
    expect_error(colWeightedMeans(sp_mat, w=1:42))
    expect_error(MatrixGenerics::colWeightedMeans(mat, w=1:42))
  })


  test_that("colWeightedMads works", {
    expect_equal(colWeightedMads(sp_mat), MatrixGenerics::colWeightedMads(mat))
    expect_equal(colWeightedMads(sp_mat, na.rm=TRUE), MatrixGenerics::colWeightedMads(mat, na.rm=TRUE))

    weights <- rep(1, nrow(sp_mat))
    expect_equal(colWeightedMads(sp_mat, w=weights), MatrixGenerics::colWeightedMads(mat, w=weights))
    expect_equal(colWeightedMads(sp_mat, na.rm=TRUE, w=weights), MatrixGenerics::colWeightedMads(mat, w=weights, na.rm=TRUE))

    # Test check for length of w
    expect_error(colWeightedMeans(sp_mat, w=1:42))
    expect_error(MatrixGenerics::colWeightedMeans(mat, w=1:42))

    skip("different result than matrixStats version, because sparseMatrixStats uses `interpolate=FALSE`.")
    weights <- rnorm(nrow(sp_mat), mean=4, sd=0.1)
    expect_equal(colWeightedMads(sp_mat, w=weights), MatrixGenerics::colWeightedMads(mat, w=weights))
    expect_equal(colWeightedMads(sp_mat, na.rm=TRUE, w=weights), MatrixGenerics::colWeightedMads(mat, w=weights, na.rm=TRUE))
  })


  test_that("colWeightedVars works", {
    weights <- rnorm(nrow(sp_mat), mean=4, sd=0.1)
    expect_equal(colWeightedVars(sp_mat, w=weights), MatrixGenerics::colWeightedVars(mat, w=weights))
    expect_equal(colWeightedVars(sp_mat, na.rm=TRUE), MatrixGenerics::colWeightedVars(mat, na.rm=TRUE))
    expect_equal(colWeightedVars(sp_mat, w=weights, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colWeightedVars(mat, w=weights, rows = row_subset, cols = col_subset, useNames = use_names_opt))

    # Test check for length of w
    expect_error(colWeightedMeans(sp_mat, w=1:42))
    expect_error(MatrixGenerics::colWeightedMeans(mat, w=1:42))
  })


  test_that("colWeightedSds works", {
    weights <- rnorm(nrow(sp_mat), mean=4, sd=0.1)
    expect_equal(colWeightedSds(sp_mat, w=weights), MatrixGenerics::colWeightedSds(mat, w=weights))
    expect_equal(colWeightedSds(sp_mat, na.rm=TRUE), MatrixGenerics::colWeightedSds(mat, na.rm=TRUE))
    expect_equal(colWeightedSds(sp_mat, w=weights, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colWeightedSds(mat, w=weights, rows = row_subset, cols = col_subset, useNames = use_names_opt))

    # Test check for length of w
    expect_error(colWeightedMeans(sp_mat, w=1:42))
    expect_error(MatrixGenerics::colWeightedMeans(mat, w=1:42))
  })





  test_that("colXXdiff methods works", {
    expect_equal(colDiffs(sp_mat, diff = 1), MatrixGenerics::colDiffs(mat, diff = 1))
    expect_equal(colDiffs(sp_mat, diff = 3), MatrixGenerics::colDiffs(mat, diff = 3))
    expect_equal(colDiffs(sp_mat, diff = 3, lag= 2), MatrixGenerics::colDiffs(mat, diff = 3, lag = 2))
    expect_equal(colDiffs(sp_mat, diff = 1, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colDiffs(mat, diff = 1, rows = row_subset, cols = col_subset, useNames = use_names_opt))

    expect_equal(colVarDiffs(sp_mat, diff = 0), MatrixGenerics::colVarDiffs(mat, diff = 0))
    expect_equal(colVarDiffs(sp_mat, diff = 1), MatrixGenerics::colVarDiffs(mat, diff = 1))
    expect_equal(colVarDiffs(sp_mat, diff = 3), MatrixGenerics::colVarDiffs(mat, diff = 3))
    expect_equal(colVarDiffs(sp_mat, diff = 0, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colVarDiffs(mat, diff = 0, rows = row_subset, cols = col_subset, useNames = use_names_opt))

    expect_equal(colSdDiffs(sp_mat, diff = 0), MatrixGenerics::colSdDiffs(mat, diff = 0))
    expect_equal(colSdDiffs(sp_mat, diff = 1), MatrixGenerics::colSdDiffs(mat, diff = 1))
    expect_equal(colSdDiffs(sp_mat, diff = 3), MatrixGenerics::colSdDiffs(mat, diff = 3))
    expect_equal(colSdDiffs(sp_mat, na.rm=TRUE), MatrixGenerics::colSdDiffs(mat, na.rm=TRUE))
    expect_equal(colSdDiffs(sp_mat, diff = 0, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colSdDiffs(mat, diff = 0, rows = row_subset, cols = col_subset, useNames = use_names_opt))

    expect_equal(colMadDiffs(sp_mat, diff = 0), MatrixGenerics::colMadDiffs(mat, diff = 0))
    expect_equal(colMadDiffs(sp_mat, diff = 1), MatrixGenerics::colMadDiffs(mat, diff = 1))
    expect_equal(colMadDiffs(sp_mat, diff = 3), MatrixGenerics::colMadDiffs(mat, diff = 3))
    expect_equal(colMadDiffs(sp_mat, na.rm=TRUE), MatrixGenerics::colMadDiffs(mat, na.rm=TRUE))
    expect_equal(colMadDiffs(sp_mat, diff = 0, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colMadDiffs(mat, diff = 0, rows = row_subset, cols = col_subset, useNames = use_names_opt))

    expect_equal(colIQRDiffs(sp_mat, diff = 0), MatrixGenerics::colIQRDiffs(mat, diff = 0))
    if(descriptions[[idx]] != "plus/minus Inf"){
      # This might be a bug in matrixStats. It should probably return NA's
      expect_equal(colIQRDiffs(sp_mat, diff = 1), MatrixGenerics::colIQRDiffs(mat, diff = 1))
      expect_equal(colIQRDiffs(sp_mat, na.rm=TRUE), MatrixGenerics::colIQRDiffs(mat, na.rm=TRUE))
    }
    expect_equal(colIQRDiffs(sp_mat, diff = 3), MatrixGenerics::colIQRDiffs(mat, diff = 3))
    expect_equal(colIQRDiffs(sp_mat, diff = 0, rows = row_subset, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colIQRDiffs(mat, diff = 0, rows = row_subset, cols = col_subset, useNames = use_names_opt))

  })


  test_that("colCollapse works", {

    expect_equal(colCollapse(sp_mat, idxs = 1), MatrixGenerics::colCollapse(mat, idxs = 1))
    expect_equal(colCollapse(sp_mat, idxs = c(1,3)), MatrixGenerics::colCollapse(mat, idxs = c(1,3)))
    expect_equal(colCollapse(sp_mat, idxs = 1:5, cols = min(ncol(mat), 3)), MatrixGenerics::colCollapse(mat, idxs = 1:5, cols = min(ncol(mat), 3)))
    if(nrow(sp_mat) > 0 && ! is.null(col_subset)){
      expect_equal(colCollapse(sp_mat, idxs = 1, cols = col_subset), sp_mat[1, col_subset])
    }
    expect_equal(colCollapse(sp_mat, idxs = 1, cols = col_subset, useNames = use_names_opt),
                 MatrixGenerics::colCollapse(mat, idxs = 1, cols = col_subset, useNames = use_names_opt))
  })


  test_that("colAvgsPerRowSet works", {
    S <-  suppressWarnings(matrix(seq_len(nrow(mat)), ncol = 2))
    expect_equal(colAvgsPerRowSet(sp_mat, S = S), MatrixGenerics::colAvgsPerRowSet(mat, S = S))
    expect_equal(colAvgsPerRowSet(sp_mat, S = S, FUN = colVarDiffs, na.rm = FALSE), MatrixGenerics::colAvgsPerRowSet(mat, S = S, FUN = colVarDiffs, na.rm = FALSE))
    expect_equal(colAvgsPerRowSet(sp_mat, S = S, na.rm = FALSE, cols = col_subset),
                 MatrixGenerics::colAvgsPerRowSet(mat, S = S, na.rm = FALSE, cols = col_subset))
  })

}
