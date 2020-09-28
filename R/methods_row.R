

# Sum

#' @rdname colSums2-xgCMatrix-method
#' @export
setMethod("rowSums2", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  # dgCMatrix_colSums2(t(x), na_rm = na.rm)
  dgCMatrix_rowSums2(x, na_rm = na.rm)
})



# Mean

#' @rdname colMeans2-xgCMatrix-method
#' @export
setMethod("rowMeans2", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  # dgCMatrix_colMeans2(t(x), na_rm = na.rm)
  dgCMatrix_rowMeans2(x, na_rm = na.rm)
})


# Median

#' @rdname colMedians-dgCMatrix-method
#' @export
setMethod("rowMedians", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colMedians(t(x), na_rm = na.rm)
})


# Vars

#' @rdname colVars-xgCMatrix-method
#' @export
setMethod("rowVars", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  # dgCMatrix_colVars(t(x), na_rm = na.rm)
  dgCMatrix_rowVars(x, na_rm = na.rm)
})


# Sds

#' @rdname colSds-xgCMatrix-method
#' @export
setMethod("rowSds", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  sqrt(dgCMatrix_colVars(t(x), na_rm = na.rm))
})



# Mads

#' @rdname colMads-dgCMatrix-method
#' @export
setMethod("rowMads", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, constant = 1.4826, na.rm=FALSE){
  colMads(t(x), rows = cols, cols = rows, constant = constant, na.rm = na.rm)
})


# LogSumExp

#' @rdname colLogSumExps-xgCMatrix-method
#' @export
setMethod("rowLogSumExps", signature(lx = "xgCMatrix"),
          function(lx, rows = NULL, cols = NULL, na.rm=FALSE){
  if(! is.null(rows)){
    lx <- lx[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    lx <- lx[, cols, drop = FALSE]
  }
  setNames(dgCMatrix_colLogSumExps(t(lx), na_rm = na.rm), rownames(lx))
})


# Prods

#' @rdname colProds-xgCMatrix-method
#' @export
setMethod("rowProds", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colProds(t(x), na_rm = na.rm)
})



# Min

#' @rdname colMins-dgCMatrix-method
#' @export
setMethod("rowMins", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colMins(t(x), na_rm = na.rm)
})


# Max

#' @rdname colMaxs-dgCMatrix-method
#' @export
setMethod("rowMaxs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colMaxs(t(x), na_rm = na.rm)
})


# OrderStats

#' @rdname colOrderStats-dgCMatrix-method
#' @export
setMethod("rowOrderStats", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, which = 1, na.rm=FALSE){
  if(which < 1 || which > ncol(x)){
    stop("Argument 'which' is out of range.")
  }
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colOrderStats(t(x), which = which, na_rm = na.rm)
})



# Weighted Means

#' @rdname colWeightedMeans-xgCMatrix-method
#' @export
setMethod("rowWeightedMeans", signature(x = "xgCMatrix"),
    function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE){
  colWeightedMeans(t(x), w = w, rows = cols, cols = rows, na.rm = na.rm)
})


# Weighted Medians

#' @rdname colWeightedMedians-dgCMatrix-method
#' @export
setMethod("rowWeightedMedians", signature(x = "dgCMatrix"),
    function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE){
  colWeightedMedians(t(x), w = w, rows = cols, cols = rows, na.rm = na.rm)
})


# Weighted Vars

#' @rdname colWeightedVars-xgCMatrix-method
#' @export
setMethod("rowWeightedVars", signature(x = "xgCMatrix"),
function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE){
  colWeightedVars(t(x), w = w, rows = cols, cols = rows, na.rm = na.rm)
})



# Weighted Sds

#' @rdname colWeightedSds-xgCMatrix-method
#' @export
setMethod("rowWeightedSds", signature(x = "xgCMatrix"),
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
    w <- w[cols]
  }
  if(is.null(w)){
    setNames(sqrt(dgCMatrix_colVars(t(x), na_rm = na.rm)), rownames(x))
  }else{
    setNames(sqrt(dgCMatrix_colWeightedVars(t(x), weights = w, na_rm = na.rm)), rownames(x))
  }
})


# Weighted Mads

#' @rdname colWeightedMads-dgCMatrix-method
#' @export
setMethod("rowWeightedMads", signature(x = "dgCMatrix"),
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE,  constant = 1.4826){
  colWeightedMads(t(x), w=w, rows = cols, cols = rows, na.rm=na.rm, constant = constant)
})



# Count

#' @rdname colCounts-xgCMatrix-method
#' @export
setMethod("rowCounts", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE){
  stopifnot(length(value) == 1)
  if(is(x, "lgCMatrix")){
    value <- as.logical(value)
  }
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colCounts(t(x), value, na_rm = na.rm)
})


# AnyNA

#' @rdname colAnyNAs-xgCMatrix-method
#' @export
setMethod("rowAnyNAs", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colAnyNAs(t(x))
})


# Anys

#' @rdname colAnys-xgCMatrix-method
#' @export
setMethod("rowAnys", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE){
  stopifnot(length(value) == 1)
  if(is(x, "lgCMatrix")){
    value <- as.logical(value)
  }
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  if(isTRUE(value)){
    ! dgCMatrix_colAlls(t(x), value = 0, na_rm=na.rm)
  }else{
    dgCMatrix_colAnys(t(x), value, na_rm=na.rm)
  }
})



# Alls

#' @rdname colAlls-xgCMatrix-method
#' @export
setMethod("rowAlls", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE){
  stopifnot(length(value) == 1)
  if(is(x, "lgCMatrix")){
    value <- as.logical(value)
  }
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  if(isTRUE(value)){
    ! dgCMatrix_colAnys(t(x), value = 0, na_rm = na.rm)
  }else{
    dgCMatrix_colAlls(t(x), value, na_rm=na.rm)
  }
})



# Collapse

#' @rdname colCollapse-xgCMatrix-method
#' @export
setMethod("rowCollapse", signature(x = "xgCMatrix"),
          function(x, idxs, rows = NULL){
  idxs <- rep(idxs, length.out = nrow(x))
  if (!is.null(rows)) {
    x <- x[rows, , drop = FALSE]
    idxs <- idxs[rows]
  }
  cols <- seq_len(ncol(x)) - 1L
  cols <- cols[idxs]
  idxs <- nrow(x) * cols + seq_len(nrow(x))
  cols <- NULL
  x[idxs]
})




# Quantiles

#' @rdname colQuantiles-xgCMatrix-method
#' @export
setMethod("rowQuantiles", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL, probs = seq(from = 0, to = 1, by = 0.25), na.rm=FALSE, drop = TRUE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  mat <- dgCMatrix_colQuantiles(t(x), probs, na_rm = na.rm)
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



# Tabulates

#' @rdname colTabulates-xgCMatrix-method
#' @export
setMethod("rowTabulates", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL, values = NULL){
  colTabulates(t(x), rows = cols, cols = rows, values = values)
})



# IQRs

#' @rdname colIQRs-xgCMatrix-method
#' @export
setMethod("rowIQRs", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  col_q <- colQuantiles(t(x), probs=c(0.25, 0.75), na.rm = na.rm, drop = FALSE)
  unname(col_q[,2] - col_q[,1])
})



# Ranges

#' @rdname colRanges-dgCMatrix-method
#' @export
setMethod("rowRanges", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE){
  tx <- t(x)
  row_max <- colMaxs(tx, rows = cols, cols = rows, na.rm = na.rm)
  row_min <- colMins(tx, rows = cols, cols = rows, na.rm = na.rm)
  unname(cbind(row_min, row_max))
})



# Cumsums

#' @rdname colCumsums-xgCMatrix-method
#' @export
setMethod("rowCumsums", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  t(dgCMatrix_colCumsums(t(x)))
})



# Cumprods

#' @rdname colCumprods-xgCMatrix-method
#' @export
setMethod("rowCumprods", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  t(dgCMatrix_colCumprods(t(x)))
})



# Cummins

#' @rdname colCummins-dgCMatrix-method
#' @export
setMethod("rowCummins", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  t(dgCMatrix_colCummins(t(x)))
})



# Cummaxs

#' @rdname colCummaxs-dgCMatrix-method
#' @export
setMethod("rowCummaxs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  t(dgCMatrix_colCummaxs(t(x)))
})



# Ranks

#' @rdname colRanks-dgCMatrix-method
#' @export
setMethod("rowRanks", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL,  ties.method = c("max", "average", "min"), preserveShape = TRUE, na.handling = c("keep", "last")){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  colRanks(t(x), ties.method = ties.method, preserveShape = ! preserveShape, na.handling = na.handling)
})



#' @rdname colDiffs-dgCMatrix-method

#' @export
setMethod("rowDiffs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L){
  t(colDiffs(t(x), rows = cols, cols = rows, lag = lag, differences = differences))
})


# VarDiffs

#' @rdname colVarDiffs-dgCMatrix-method
#' @export
setMethod("rowVarDiffs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0){
  colVarDiffs(t(x), rows = cols, cols = rows, na.rm=na.rm, diff=diff, trim = trim)
})



# SdDiffs

#' @rdname colSdDiffs-dgCMatrix-method
#' @export
setMethod("rowSdDiffs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0){
  colSdDiffs(t(x), rows = cols, cols = rows, na.rm=na.rm, diff=diff, trim = trim)
})



# MadDiffs

#' @rdname colMadDiffs-dgCMatrix-method
#' @export
setMethod("rowMadDiffs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, constant = 1.4826){
  colMadDiffs(t(x), rows = cols, cols = rows, na.rm=na.rm, diff=diff, trim = trim, constant = constant)
})



# IQRDiffs

#' @rdname colIQRDiffs-dgCMatrix-method
#' @export
setMethod("rowIQRDiffs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0){
  colIQRDiffs(t(x), rows = cols, cols = rows, na.rm=na.rm, diff=diff, trim = trim)
})


#' @rdname colAvgsPerRowSet-xgCMatrix-method
#' @export
setMethod("rowAvgsPerColSet", signature(X = "xgCMatrix"),
          function(X, W = NULL, rows = NULL, S, FUN = rowMeans2, ..., tFUN = FALSE){
  tZ <- colAvgsPerRowSet(t(X), W = W, cols = rows, S = S, FUN  = FUN, ..., tFUN = ! tFUN)
  t(tZ)
})

