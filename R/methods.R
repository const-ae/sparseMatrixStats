

# Sum

#' @inherit MatrixGenerics::colSums2
#' @export
setMethod("colSums2", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colSums2(x, na_rm = na.rm)
})


# Mean

#' @inherit MatrixGenerics::colMeans2
#' @export
setMethod("colMeans2", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colMeans2(x, na_rm = na.rm)
})


# Median

#' @inherit MatrixGenerics::colMedians
#' @export
setMethod("colMedians", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colMedians(x, na_rm = na.rm)
})


# Vars

#' @inherit MatrixGenerics::colVars
#' @export
setMethod("colVars", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colVars(x, na_rm = na.rm)
})


# Sds

#' @inherit MatrixGenerics::colSds
#' @export
setMethod("colSds", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  sqrt(dgCMatrix_colVars(x, na_rm = na.rm))
})


# Mads

#' @inherit MatrixGenerics::colMads
#' @export
setMethod("colMads", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, constant = 1.4826, na.rm=FALSE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colMads(x, na_rm = na.rm, scale_factor = constant)
})


# LogSumExp

#' @inherit MatrixGenerics::colLogSumExps
#' @export
setMethod("colLogSumExps", signature(lx = "dgCMatrix"),
          function(lx, rows = NULL, cols = NULL, na.rm=FALSE){
  if(! is.null(rows)){
    lx <- lx[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    lx <- lx[, cols, drop = FALSE]
  }
  dgCMatrix_colLogSumExps(lx, na_rm = na.rm)
})


# Prods

#' Calculates the product for each row (column) in a matrix
#'
#' Calculates the product for each row (column) in a matrix
#'
#'
#' Attention: This method ignores the order of the values, because it assumes that
#' the product is commutative. Unfortunately, for 'double' this is not true.
#' For example `NaN * NA = NaN`, but `NA * NaN = NA`. This is relevant for this
#' function if there are `+-Inf`, because `Inf * 0 = NaN`. This function returns
#' `NA` whenever there is `NA` in the input. This is different from `matrixStats::colProds()`.
#'
#' @inherit MatrixGenerics::colProds
#'
#' @export
setMethod("colProds", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colProds(x, na_rm = na.rm)
})



# Min

#' @inherit MatrixGenerics::colMins
#' @export
setMethod("colMins", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colMins(x, na_rm = na.rm)
})


# Max

#' @inherit MatrixGenerics::colMaxs
#' @export
setMethod("colMaxs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colMaxs(x, na_rm = na.rm)
})


# OrderStats

#' @inherit MatrixGenerics::colOrderStats
#' @param na.rm If TRUE, NAs are excluded first, otherwise not.
#'
#' @export
setMethod("colOrderStats", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, which = 1, na.rm=FALSE){
  if(which < 1 || which > nrow(x)){
    stop("Argument 'which' is out of range.")
  }
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colOrderStats(x, which = which, na_rm = na.rm)
})




# Weighted Means

#' @inherit MatrixGenerics::colWeightedMeans
#' @export
setMethod("colWeightedMeans", signature(x = "dgCMatrix"),
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
    w <- w[rows]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  if(is.null(w)){
    dgCMatrix_colMeans2(x, na_rm = na.rm)
  }else{
    dgCMatrix_colWeightedMeans(x, weights = w, na_rm = na.rm)
  }
})



# Weighted Medians

#' @inherit MatrixGenerics::colWeightedMedians
#' @export
setMethod("colWeightedMedians", signature(x = "dgCMatrix"),
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
    w <- w[rows]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  if(is.null(w)){
    dgCMatrix_colMedians(x, na_rm = na.rm)
  }else{
    reduce_sparse_matrix_to_num(x, function(values, row_indices, number_of_zeros){
      if(length(values) == 0 && number_of_zeros > 0){
        return(0.0)
      }else if(length(values) == 0 && number_of_zeros > 0){
        return(NA)
      }else{
        new_vec <- c(0, values)
        zero_weight <- sum(w[-(row_indices + 1)])
        new_weights <- c(zero_weight, w[row_indices + 1])
        matrixStats::weightedMedian(new_vec, new_weights, na.rm=na.rm, interpolate = FALSE)
      }
    })
  }
})


# Weighted Vars

#' @inherit MatrixGenerics::colWeightedVars
#' @export
setMethod("colWeightedVars", signature(x = "dgCMatrix"),
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
    w <- w[rows]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  if(is.null(w)){
    dgCMatrix_colVars(x, na_rm = na.rm)
  }else{
    dgCMatrix_colWeightedVars(x, weights = w, na_rm = na.rm)
  }
})



# Weighted Sds

#' @inherit MatrixGenerics::colWeightedSds
#' @export
setMethod("colWeightedSds", signature(x = "dgCMatrix"),
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
    w <- w[rows]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  if(is.null(w)){
    sqrt(dgCMatrix_colVars(x, na_rm = na.rm))
  }else{
    sqrt(dgCMatrix_colWeightedVars(x, weights = w, na_rm = na.rm))
  }
})



# Weighted Mads

#' @inherit MatrixGenerics::colWeightedMads
#' @param center Not supported at the moment.
#'
#' @examples
#'   mat <- matrix(0, nrow=10, ncol=5)
#'   mat[sample(prod(dim(mat)), 25)] <- rpois(n=25, 5)
#'   sp_mat <- as(mat, "dgCMatrix")
#'   weights <- rnorm(10, mean=1, sd=0.1)
#'
#'   # sparse version
#'   sparseMatrixStats::colWeightedMads(sp_mat, weights)
#'
#'   # Attention the result differs from matrixStats
#'   # because it always uses 'interpolate=FALSE'.
#'   matrixStats::colWeightedMads(mat, weights)
#'
#' @export
setMethod("colWeightedMads", signature(x = "dgCMatrix"),
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE, constant = 1.4826, center = NULL){
  if(! is.null(center)) stop("colWeightedMads does not support the 'center' argument.")
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
    w <- w[rows]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  if(is.null(w)){
    dgCMatrix_colMads(x, na_rm = na.rm, scale_factor = constant)
  }else{
    reduce_sparse_matrix_to_num(x, function(values, row_indices, number_of_zeros){
      if(length(values) == 0){
        0.0
      }else{
        new_vec <- c(0, values)
        zero_weight <- sum(w[-(row_indices + 1)])
        new_weights <- c(zero_weight, w[row_indices + 1])
        center <- matrixStats::weightedMedian(new_vec, new_weights, na.rm=na.rm, interpolate = FALSE)
        x <- abs(new_vec - center)
        sigma <- matrixStats::weightedMedian(x, w = new_weights, na.rm = na.rm, interpolate = FALSE)
        # Rescale for normal distributions
        sigma <- constant * sigma
        sigma
      }
    })
  }
})



# Count

#' @inherit MatrixGenerics::colCounts
#' @export
setMethod("colCounts", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colCounts(x, value, na_rm = na.rm)
})


# AnyNA

#' @inherit MatrixGenerics::colAnyNAs
#'
#' @examples
#'   mat <- matrix(0, nrow=10, ncol=5)
#'   mat[sample(seq_len(5 *10), 5)] <- NA
#'   sp_mat <- as(mat, "dgCMatrix")
#'   colAnyNAs(sp_mat)
#'
#' @export
setMethod("colAnyNAs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colAnyNAs(x)
})


# Anys

#' @inherit MatrixGenerics::colAnys
#' @export
setMethod("colAnys", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colAnys(x, value, na_rm=na.rm)
})



# Alls

#' @inherit MatrixGenerics::colAlls
#' @export
setMethod("colAlls", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colAlls(x, value, na_rm=na.rm)
})



# Collapse

#' @inherit MatrixGenerics::colCollapse
#' @export
setMethod("colCollapse", signature(x = "dgCMatrix"),
          function(x, idxs, cols = NULL){
  idxs <- rep(idxs, length.out = ncol(x))
  if (!is.null(cols)) {
    x <- x[, cols, drop = FALSE]
    idxs <- idxs[cols]
  }
  rows <- seq_len(nrow(x))
  rows <- rows[idxs]
  idxs <- nrow(x) * (seq_len(ncol(x)) - 1L) + rows
  rows <- NULL
  x[idxs]
})




# colQuantiles

#' @inherit MatrixGenerics::colQuantiles
#' @export
setMethod("colQuantiles", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, probs = seq(from = 0, to = 1, by = 0.25), na.rm=FALSE, drop = TRUE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  mat <- dgCMatrix_colQuantiles(x, probs, na_rm = na.rm)
  # Add dim names
  digits <- max(2L, getOption("digits"))
  colnames(mat) <- sprintf("%.*g%%", digits, 100 * probs)
  rownames(mat) <- rownames(x)
  if(drop && nrow(mat) == 1){
    mat[1,]
  }else{
    mat
  }
})



# colTabulates

#' @inherit MatrixGenerics::colTabulates
#' @export
setMethod("colTabulates", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, values = NULL){
    default_value <- 0
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  if(is.null(values)){
    recheck_that_zeros_in_matrix <- TRUE
    repeat_duplicate_values <- FALSE
    values <- c(x@x, default_value)
    unique_values <- sort(unique(values), na.last = TRUE)
  }else{
    recheck_that_zeros_in_matrix <- FALSE
    repeat_duplicate_values <- TRUE
    unique_values <- unique(values)
  }
  mat <- dgCMatrix_colTabulate(x, unique_values)
  # Add dim names
  colnames(mat) <- ifelse(is.na(unique_values), "NA", unique_values)
  rownames(mat) <- colnames(x)
  if(recheck_that_zeros_in_matrix && all(mat[, as.character(default_value)] == 0)){
    # Remove zero column is there is not a single zero in x
    mat <- mat[, -which(colnames(mat) == as.character(default_value)), drop=FALSE]
  }
  if(repeat_duplicate_values){
    mat <- mat[,  ifelse(is.na(values), "NA", as.character(values)), drop=FALSE]
  }
  colnames(mat) <- ifelse(colnames(mat) == "NA", NA, colnames(mat))
  mat
})



# colIQRs

#' @inherit MatrixGenerics::colIQRs
#' @export
setMethod("colIQRs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE){
  col_q <- colQuantiles(x, rows, cols, probs=c(0.25, 0.75), na.rm = na.rm, drop = FALSE)
  col_q[,2] - col_q[,1]
})



# colRanges

#' @inherit MatrixGenerics::colRanges
#' @export
setMethod("colRanges", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE){
  col_max <- colMaxs(x, rows, cols, na.rm = na.rm)
  col_min <- colMins(x, rows, cols, na.rm = na.rm)
  unname(cbind(col_min, col_max))
})



# colCumsums

#' @inherit MatrixGenerics::colCumsums
#' @export
setMethod("colCumsums", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colCumsums(x)
})



# colCumprods

#' @inherit MatrixGenerics::colCumprods
#' @export
setMethod("colCumprods", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colCumprods(x)
})



# colCummins

#' @inherit MatrixGenerics::colCummins
#' @export
setMethod("colCummins", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colCummins(x)
})



# colCummaxs

#' @inherit MatrixGenerics::colCummaxs
#' @export
setMethod("colCummaxs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colCummaxs(x)
})




# colRanks

#' @inherit MatrixGenerics::colRanks
#' @param preserve.shape a boolean that specifies if the returned matrix has the same
#'   dimensions as the input matrix. By default this is true for `rowRanks()`, but false for
#'   `colRanks()`.
#' @param na.handling string specifying how `NA`s are handled. They can either be preserved with an `NA` rank
#'   ('keep') or sorted in at the end ('last'). Default is 'keep' derived from the behavior of the equivalent
#'
#' @details
#'    There are three different methods available for handling ties:
#'    \describe{
#'      \item{`max`}{for values with identical values the maximum rank is returned}
#'      \item{`average`}{for values with identical values the average of the ranks they cover
#'      is returned. Note, that in this case the return value is of type `numeric`.}
#'      \item{`min`}{for values with identical values the minimum rank is returned.}
#'    }
#' @export
setMethod("colRanks", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL,  ties.method = c("max", "average", "min"), preserve.shape = FALSE, na.handling = c("keep", "last")){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  ties.method <- match.arg(ties.method,  c("max", "average", "min"))
  na.handling <- match.arg(na.handling, c("keep", "last"))
  if(ties.method == "average"){
    dgCMatrix_colRanks_num(x, ties_method = ties.method, na_handling = na.handling, preserve_shape = preserve.shape)
  }else{
    dgCMatrix_colRanks_int(x, ties_method = ties.method, na_handling = na.handling, preserve_shape = preserve.shape)
  }
})



#' @inherit MatrixGenerics::colDiffs
#'
#' @export
setMethod("colDiffs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  if(differences == 0){
    x
  }else{
    reduce_sparse_matrix_to_matrix(x, n_result_rows = max(nrow(x) - differences * lag, 0), function(values, row_indices, number_of_zeros){
      tmp <- rep(0,  nrow(x))
      tmp[row_indices+1] <- values
      matrixStats::diff2(tmp, lag = lag, differences = differences)
    })
  }
})



#' @inherit MatrixGenerics::colVarDiffs
#'
#' @export
setMethod("colVarDiffs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  if(diff == 0){
    dgCMatrix_colVars(x, na_rm = na.rm)
  }else{
    n <- nrow(x)
    reduce_sparse_matrix_to_num(x, function(values, row_indices, number_of_zeros){
      tmp <- rep(0, n)
      tmp[row_indices+1] <- values
      matrixStats::varDiff(tmp, na.rm=na.rm, diff = diff, trim = trim)
    })
  }
})




#' @inherit MatrixGenerics::colSdDiffs
#'
#' @export
setMethod("colSdDiffs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  if(diff == 0){
    sqrt(dgCMatrix_colVars(x, na_rm = na.rm))
  }else{
    n <- nrow(x)
    reduce_sparse_matrix_to_num(x, function(values, row_indices, number_of_zeros){
      tmp <- rep(0, n)
      tmp[row_indices+1] <- values
      matrixStats::sdDiff(tmp, na.rm=na.rm, diff = diff, trim = trim)
    })
  }
})



#' @inherit MatrixGenerics::colMadDiffs
#' @param constant A scale factor. See \code{\link{mad}} for details.
#' @export
setMethod("colMadDiffs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, constant = 1.4826){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  if(diff == 0){
    dgCMatrix_colMads(x, na_rm = na.rm, scale_factor = constant)
  }else{
    n <- nrow(x)
    reduce_sparse_matrix_to_num(x, function(values, row_indices, number_of_zeros){
      tmp <- rep(0, n)
      tmp[row_indices+1] <- values
      matrixStats::madDiff(tmp, na.rm=na.rm, diff = diff, trim = trim, constant = constant)
    })
  }
})



#' @inherit MatrixGenerics::colIQRDiffs
#'
#' @export
setMethod("colIQRDiffs", signature(x = "dgCMatrix"),
function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  if(diff == 0){
    unname(colIQRs(x, na.rm = na.rm))
  }else{
    n <- nrow(x)
    reduce_sparse_matrix_to_num(x, function(values, row_indices, number_of_zeros){
      tmp <- rep(0, n)
      tmp[row_indices+1] <- values
      matrixStats::iqrDiff(tmp, na.rm=na.rm, diff = diff, trim = trim)
    })
  }
})



#' Calculates for each row (column) a summary statistic for equally sized subsets of columns (rows)
#'
#' Calculates for each row (column) a summary statistic for equally sized subsets of columns (rows)
#'
#' @inherit MatrixGenerics::colAvgsPerRowSet
#'
#'
#' @details
#'   **Note**: the handling of missing parameters differs from
#'   [matrixStats::colAvgsPerRowSet()]. The `matrixStats` version
#'   always removes `NA`'s if there are any in the data. This method
#'   however does whatever is passed in the `...` parameter.
#'
#' @aliases colAvgsPerRowSet
#' @export
setMethod("colAvgsPerRowSet", signature(X = "dgCMatrix"),
function(X, W = NULL, cols = NULL, S, FUN = colMeans2, ..., tFUN = FALSE){
  if(! is.null(W)) stop("the W parameter is not supported.")
  nbrOfSets <- ncol(S)
  setNames <- colnames(S)
  if (!is.function(FUN)) {
    stop("Argument 'FUN' is not a function: ", mode(S))
  }
  if (!is.null(cols)) {
    X <- X[, cols, drop = FALSE]
  }
  dimX <- dim(X)
  tFUN <- as.logical(tFUN)
  colnamesX <- colnames(X)
  dimnames(X) <- list(NULL, NULL)

  Z <- apply(S, MARGIN = 2L, FUN = function(jj) {
    jj <- jj[is.finite(jj)]
    Zjj <- X[jj, , drop = FALSE]
    jj <- NULL
    if (tFUN) {
      Zjj <- t(Zjj)
    }
    Zjj <- FUN(Zjj, ...)
    if (length(Zjj) != dimX[2L])
      stop("Internal error: length(Zjj) != dimX[1L]")
    Zjj
  })
  if (!is.matrix(Z)) {
    if (dimX[2] > 1L)
      stop("Internal error: dimX[1] > 1L")
    dim(Z) <- c(dimX[2L], nbrOfSets)
  }
  if (any(dim(Z) != c(dimX[2L], nbrOfSets)))
    stop("Internal error: dim(Z) != c(dimX[1L], nbrOfSets)")
  rownames(Z) <- colnamesX
  colnames(Z) <- setNames
  t(Z)
})

