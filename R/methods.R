

# Sum

#' @inherit matrixStats::colSums2
#' @export
setGeneric("colSums2", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colSums2(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colSums2
#' @export
setMethod("colSums2", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colSums2(x, na_rm = na.rm))



# Mean

#' @inherit matrixStats::colMeans2
#' @export
setGeneric("colMeans2", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colMeans2(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})


#' @rdname colMeans2
#' @export
setMethod("colMeans2", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colMeans2(x, na_rm = na.rm))


# Median

#' @inherit matrixStats::colMedians
#' @export
setGeneric("colMedians", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colMedians(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colMedians
#' @export
setMethod("colMedians", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colMedians(x, na_rm = na.rm))

# Vars

#' @inherit matrixStats::colVars
#' @export
setGeneric("colVars", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colVars(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colVars
#' @export
setMethod("colVars", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colVars(x, na_rm = na.rm))


# Sds

#' @inherit matrixStats::colSds
#' @export
setGeneric("colSds", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colSds(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colSds
#' @export
setMethod("colSds", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            sqrt(dgCMatrix_colVars(x, na_rm = na.rm)))


# Mads

#' @inherit matrixStats::colMads
#' @export
setGeneric("colMads", function(x, rows = NULL, cols = NULL, constant = 1.4826, na.rm=FALSE, ...){
  matrixStats::colMads(as.matrix(x), rows = rows, cols = cols, constant = constant, na.rm = na.rm, ...)
})

#' @rdname colMads
#' @export
setMethod("colMads", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, constant = 1.4826, na.rm=FALSE, ...)
            dgCMatrix_colMads(x, na_rm = na.rm, scale_factor = constant))


# LogSumExp

#' @inherit matrixStats::colLogSumExps
#' @export
setGeneric("colLogSumExps", function(lx, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colLogSumExps(as.matrix(lx), rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colLogSumExps
#' @export
setMethod("colLogSumExps", signature(lx = "dgCMatrix"),
          function(lx, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colLogSumExps(lx, na_rm = na.rm))


# Prods

#' @inherit matrixStats::colProds
#' @export
setGeneric("colProds", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colProds(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, center = center, dim. = dim., ...)
})

#' @rdname colProds
#' @export
setMethod("colProds", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colProds(x, na_rm = na.rm))



# Min

#' @inherit matrixStats::colMins
#' @export
setGeneric("colMins", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colMins(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colMins
#' @export
setMethod("colMins", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colMins(x, na_rm = na.rm))


# Max

#' @inherit matrixStats::colMaxs
#' @export
setGeneric("colMaxs", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colMaxs(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colMaxs
#' @export
setMethod("colMaxs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colMaxs(x, na_rm = na.rm))



# Weighted Means

#' @inherit matrixStats::colWeightedMeans
#' @export
setGeneric("colWeightedMeans", function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colWeightedMeans(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colWeightedMeans
#' @export
setMethod("colWeightedMeans", signature(x = "dgCMatrix"),
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  if(is.null(w)){
    dgCMatrix_colMeans2(x, na_rm = na.rm)
  }else{
    dgCMatrix_colWeightedMeans(x, weights = w, na_rm = na.rm)
  }
})



# Weighted Medians

#' @inherit matrixStats::colWeightedMedians
#' @export
setGeneric("colWeightedMedians", function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE,
                                          ties = NULL, ...){
  matrixStats::colWeightedMedians(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm,
                                  interpolate = FALSE, ties = ties, ...)
})

#' @rdname colWeightedMedians
#' @export
setMethod("colWeightedMedians", signature(x = "dgCMatrix"),
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE,
                   ties = NULL, ...){
  if(is.null(w)){
    dgCMatrix_colMedians(x, na_rm = na.rm)
  }else{
    reduce_sparse_matrix_to_num(x, function(values, row_indices, number_of_zeros){
      if(length(values) == 0){
        return(0.0)
      }else{
        new_vec <- c(0, values)
        zero_weight <- sum(w[-(row_indices + 1)])
        new_weights <- c(zero_weight, w[row_indices + 1])
        matrixStats::weightedMedian(new_vec, new_weights, na.rm=na.rm, interpolate = FALSE, ties = ties)
      }
    })
  }
})


# Weighted Vars

#' @inherit matrixStats::colWeightedVars
#' @export
setGeneric("colWeightedVars", function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colWeightedVars(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colWeightedVars
#' @export
setMethod("colWeightedVars", signature(x = "dgCMatrix"),
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  if(is.null(w)){
    dgCMatrix_colVars(x, na_rm = na.rm)
  }else{
    dgCMatrix_colWeightedVars(x, weights = w, na_rm = na.rm)
  }
})



# Weighted Sds

#' @inherit matrixStats::colWeightedSds
#' @export
setGeneric("colWeightedSds", function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colWeightedSds(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colWeightedSds
#' @export
setMethod("colWeightedSds", signature(x = "dgCMatrix"),
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  if(is.null(w)){
    sqrt(dgCMatrix_colVars(x, na_rm = na.rm))
  }else{
    sqrt(dgCMatrix_colWeightedVars(x, weights = w, na_rm = na.rm))
  }
})



# Weighted Mads

#' @inherit matrixStats::colWeightedMads
#' @export
setGeneric("colWeightedMads", function(x, w = NULL, rows = NULL, cols = NULL, constant = 1.4826, na.rm=FALSE, ...){
  matrixStats::colWeightedMads(as.matrix(x), rows = rows, cols = cols, constant = constant, na.rm = na.rm, ...)
})

#' @rdname colWeightedMads
#' @export
setMethod("colWeightedMads", signature(x = "dgCMatrix"),
          function(x, w = NULL, rows = NULL, cols = NULL, constant = 1.4826, na.rm=FALSE,
                   ties = NULL, ...){
  if(is.null(w)){
    dgCMatrix_colMads(x, na_rm = na.rm)
  }else{
    reduce_sparse_matrix_to_num(x, function(values, row_indices, number_of_zeros){
      if(length(values) == 0){
        0.0
      }else{
        new_vec <- c(0, values)
        zero_weight <- sum(w[-(row_indices + 1)])
        new_weights <- c(zero_weight, w[row_indices + 1])
        center <- matrixStats::weightedMedian(new_vec, new_weights, na.rm=na.rm, interpolate = FALSE, ties = ties)
        x <- abs(new_vec - center)
        sigma <- matrixStats::weightedMedian(x, w = new_weights, na.rm = na.rm, interpolate = FALSE, ties = ties)
        # Rescale for normal distributions
        sigma <- constant * sigma
        sigma
      }
    })
  }
})



# Count

#' @inherit matrixStats::colCounts
#' @export
setGeneric("colCounts", function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...){
  matrixStats::colCounts(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colCounts
#' @export
setMethod("colCounts", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...)
            dgCMatrix_colCounts(x, value, na_rm = na.rm))


# AnyNA

#' @inherit matrixStats::colAnyNAs
#' @export
setGeneric("colAnyNAs", function(x, rows = NULL, cols = NULL, ...){
  matrixStats::colAnyNAs(as.matrix(x), rows = rows, cols = cols, ...)
})

#' @rdname colAnyNAs
#' @export
setMethod("colAnyNAs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, ...)
            dgCMatrix_colAnyNAs(x))


# Anys

#' @inherit matrixStats::colAnys
#' @export
setGeneric("colAnys", function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...){
  matrixStats::colAnys(as.matrix(x), rows = rows, cols = cols, value = value, na.rm = na.rm, ...)
})

#' @rdname colAnys
#' @export
setMethod("colAnys", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...)
            dgCMatrix_colAnys(x, value, na_rm=na.rm))



# Alls

#' @inherit matrixStats::colAlls
#' @export
setGeneric("colAlls", function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...){
  matrixStats::colAlls(as.matrix(x), rows = rows, cols = cols, value = value, na.rm = na.rm, ...)
})

#' @rdname colAlls
#' @export
setMethod("colAlls", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...)
            dgCMatrix_colAlls(x, value, na_rm=na.rm))



# Collapse

#' @inherit matrixStats::colCollapse
#' @export
setGeneric("colCollapse", function(x, idxs, rows = NULL, cols = NULL,  ...){
  matrixStats::colCollapse(as.matrix(x), idxs, rows = rows, cols = cols, value = value, na.rm = na.rm, ...)
})

#' @rdname colCollapse
#' @export
setMethod("colCollapse", signature(x = "dgCMatrix"),
          function(x, idxs, rows = NULL, cols = NULL, ...)
            x[, idxs])




# colQuantiles

#' @inherit matrixStats::colQuantiles
#' @export
setGeneric("colQuantiles", function(x, rows = NULL, cols = NULL, probs = seq(from = 0, to = 1, by = 0.25), na.rm=FALSE, ...){
  matrixStats::colQuantiles(as.matrix(x), rows = rows, cols = cols, probs = probs, na.rm = na.rm, ...)
})

#' @rdname colQuantiles
#' @export
setMethod("colQuantiles", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, probs = seq(from = 0, to = 1, by = 0.25), na.rm=FALSE, ...){
  mat <- dgCMatrix_colQuantiles(x, probs, na_rm = na.rm)
  # Add dim names
  digits <- max(2L, getOption("digits"))
  colnames(mat) <- sprintf("%.*g%%", digits, 100 * probs)
  rownames(mat) <- rownames(x)
  mat
})



# colTabulates

#' @inherit matrixStats::colTabulates
#' @export
setGeneric("colTabulates", function(x, rows = NULL, cols = NULL, values = NULL, ...){
  matrixStats::colTabulates(as.matrix(x), rows = rows, cols = cols, values = values, ...)
})

#' @rdname colTabulates
#' @export
setMethod("colTabulates", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, values = NULL, ...){
  if(is.null(values)){
    values <- c(x@x, 0)
    unique_values <- sort(unique(values), na.last = TRUE)
  }else{
    unique_values <- unique(values)
  }

  mat <- dgCMatrix_colTabulate(x, unique_values)
  # Add dim names
  colnames(mat) <- unique_values
  rownames(mat) <- rownames(x)
  if(all(mat[, "0"] == 0)){
    # Remove zero column is there is not a single zero in x
    mat <- mat[, -which(colnames(mat) == "0"), drop=FALSE]
  }
  mat
})



# colIQRs

#' @inherit matrixStats::colIQRs
#' @export
setGeneric("colIQRs", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colIQRs(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colIQRs
#' @export
setMethod("colIQRs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  col_q <- colQuantiles(x, rows, cols, probs=c(0.25, 0.75), na.rm = na.rm)
  col_q[,2] - col_q[,1]
})



# colRanges

#' @inherit matrixStats::colRanges
#' @export
setGeneric("colRanges", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colRanges(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colRanges
#' @export
setMethod("colRanges", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
            col_max <- colMaxs(x, rows, cols, na.rm = na.rm)
            col_min <- colMins(x, rows, cols, na.rm = na.rm)
            unname(cbind(col_min, col_max))
          })



# colCumsums

#' @inherit matrixStats::colCumsums
#' @export
setGeneric("colCumsums", function(x, rows = NULL, cols = NULL, ...){
  matrixStats::colCumsums(as.matrix(x), rows = rows, cols = cols, ...)
})

#' @rdname colCumsums
#' @export
setMethod("colCumsums", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, ...)
            dgCMatrix_colCumsums(x))



# colCumprods

#' @inherit matrixStats::colCumprods
#' @export
setGeneric("colCumprods", function(x, rows = NULL, cols = NULL, ...){
  matrixStats::colCumprods(as.matrix(x), rows = rows, cols = cols, ...)
})

#' @rdname colCumprods
#' @export
setMethod("colCumprods", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, ...)
            dgCMatrix_colCumprods(x))



# colCummins

#' @inherit matrixStats::colCummins
#' @export
setGeneric("colCummins", function(x, rows = NULL, cols = NULL, ...){
  matrixStats::colCummins(as.matrix(x), rows = rows, cols = cols, ...)
})

#' @rdname colCummins
#' @export
setMethod("colCummins", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, ...)
            dgCMatrix_colCummins(x))



# colCummaxs

#' @inherit matrixStats::colCummaxs
#' @export
setGeneric("colCummaxs", function(x, rows = NULL, cols = NULL, ...){
  matrixStats::colCummaxs(as.matrix(x), rows = rows, cols = cols, ...)
})

#' @rdname colCummaxs
#' @export
setMethod("colCummaxs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, ...)
            dgCMatrix_colCummaxs(x))




# colRanks

#' @inherit matrixStats::colRanks
#' @param preserve.shape a boolean that specifies if the returned matrix has the same
#'   dimensions as the input matrix. By default this is true for `rowRanks()`, but false for
#'   `colRanks()`.
#' @param na.handling string specifying how `NA`s are handled. They can either be preserved with an `NA` rank
#'   ('keep') or sorted in at the end ('last'). Default is 'keep' derived from the behavior of the equivalent
#'   `matrixStats` function.
#' @export
setGeneric("colRanks", function(x, rows = NULL, cols = NULL, ties.method = c("max", "average", "min"), preserve.shape = FALSE, ...){
  matrixStats::colRanks(as.matrix(x), rows = rows, cols = cols, ties.method = ties.method, preserveShape = preserve.shape, ...)
})

#' @rdname colRanks
#' @export
setMethod("colRanks", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL,  ties.method = c("max", "average", "min"), preserve.shape = FALSE, na.handling = c("keep", "last"), ...){
  ties.method <- match.arg(ties.method,  c("max", "average", "min"))
  na.handling <- match.arg(na.handling, c("keep", "last"))
  if(ties.method == "average"){
    dgCMatrix_colRanks_num(x, ties_method = ties.method, na_handling = na.handling, preserve_shape = preserve.shape)
  }else{
    dgCMatrix_colRanks_int(x, ties_method = ties.method, na_handling = na.handling, preserve_shape = preserve.shape)
  }
})

