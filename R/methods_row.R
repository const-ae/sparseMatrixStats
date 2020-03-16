

# Sum

#' @rdname colSums2-dgCMatrix-method
#' @export
setMethod("rowSums2", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colSums2(t(x), na_rm = na.rm)
})



# Mean

#' @rdname colMeans2-dgCMatrix-method
#' @export
setMethod("rowMeans2", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colMeans2(t(x), na_rm = na.rm)
})


# Median

#' @rdname colMedians-dgCMatrix-method
#' @export
setMethod("rowMedians", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colMedians(t(x), na_rm = na.rm)
})


# Vars

#' @rdname colVars-dgCMatrix-method
#' @export
setMethod("rowVars", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colVars(t(x), na_rm = na.rm)
})


# Sds

#' @rdname colSds-dgCMatrix-method
#' @export
setMethod("rowSds", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
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
          function(x, rows = NULL, cols = NULL, constant = 1.4826, na.rm=FALSE, ...){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  colMads(t(x), rows = rows, cols = cols, constant = constant, na.rm = na.rm, ...)
})


# LogSumExp

#' @rdname colLogSumExps-dgCMatrix-method
#' @export
setMethod("rowLogSumExps", signature(lx = "dgCMatrix"),
          function(lx, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colLogSumExps(t(lx), na_rm = na.rm)
})


# Prods

#' @rdname colProds-dgCMatrix-method
#' @export
setMethod("rowProds", signature(x = "dgCMatrix"),
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
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
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
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
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
          function(x, rows = NULL, cols = NULL, which = 1, na.rm=FALSE, ...){
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

#' @rdname colWeightedMeans-dgCMatrix-method
#' @export
setMethod("rowWeightedMeans", signature(x = "dgCMatrix"),
    function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  colWeightedMeans(t(x), w = w, rows = rows, cols = cols, na.rm = na.rm)
})


# Weighted Medians

#' @rdname colWeightedMedians-dgCMatrix-method
#' @export
setMethod("rowWeightedMedians", signature(x = "dgCMatrix"),
    function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  colWeightedMedians(t(x), w = w, rows = rows, cols = cols, na.rm = na.rm)
})


# Weighted Vars

#' @rdname colWeightedVars-dgCMatrix-method
#' @export
setMethod("rowWeightedVars", signature(x = "dgCMatrix"),
function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  colWeightedVars(t(x), w = w, rows = rows, cols = cols, na.rm = na.rm, ...)
})



# Weighted Sds

#' @rdname colWeightedSds-dgCMatrix-method
#' @export
setMethod("rowWeightedSds", signature(x = "dgCMatrix"),
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  if(is.null(w)){
    sqrt(dgCMatrix_colVars(t(x), na_rm = na.rm))
  }else{
    sqrt(dgCMatrix_colWeightedVars(t(x), weights = w, na_rm = na.rm))
  }
})


# Weighted Mads

#' @rdname colWeightedMads-dgCMatrix-method
#' @export
setMethod("rowWeightedMads", signature(x = "dgCMatrix"),
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE,  constant = 1.4826, ...){
  colWeightedMads(t(x), w=w, rows=rows, cols=cols, na.rm=na.rm, constant = constant, ...)
})



# Count

#' @rdname colCounts-dgCMatrix-method
#' @export
setMethod("rowCounts", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colCounts(t(x), value, na_rm = na.rm)
})


# AnyNA

#' @rdname colAnyNAs-dgCMatrix-method
#' @export
setMethod("rowAnyNAs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, ...){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colAnyNAs(t(x))
})


# Anys

#' @rdname colAnys-dgCMatrix-method
#' @export
setMethod("rowAnys", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colAnys(t(x), value, na_rm=na.rm)
})



# Alls

#' @rdname colAlls-dgCMatrix-method
#' @export
setMethod("rowAlls", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  dgCMatrix_colAlls(t(x), value, na_rm=na.rm)
})



# Collapse

#' @rdname colCollapse-dgCMatrix-method
#' @export
setMethod("rowCollapse", signature(x = "dgCMatrix"),
          function(x, idxs, rows = NULL, ...){
  if (!is.null(rows)) {
    x <- x[rows, , drop = FALSE]
    idxs <- idxs[rows]
  }
  idxs <- rep(idxs, length.out = nrow(x))
  cols <- seq_len(ncol(x)) - 1L
  cols <- cols[idxs]
  idxs <- nrow(x) * cols + seq_len(nrow(x))
  cols <- NULL
  x[idxs]
})




# Quantiles

#' @rdname colQuantiles-dgCMatrix-method
#' @export
setMethod("rowQuantiles", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, probs = seq(from = 0, to = 1, by = 0.25), na.rm=FALSE, ...){
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
  mat
})



# Tabulates

#' @rdname colTabulates-dgCMatrix-method
#' @export
setMethod("rowTabulates", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, values = NULL, ...){
  colTabulates(t(x), rows = rows, cols = cols, values = values)
})



# IQRs

#' @rdname colIQRs-dgCMatrix-method
#' @export
setMethod("rowIQRs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  col_q <- colQuantiles(t(x), probs=c(0.25, 0.75), na.rm = na.rm)
  col_q[,2] - col_q[,1]
})



# Ranges

#' @rdname colRanges-dgCMatrix-method
#' @export
setMethod("rowRanges", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  tx <- t(x)
  row_max <- colMaxs(tx, rows, cols, na.rm = na.rm)
  row_max <- colMins(tx, rows, cols, na.rm = na.rm)
  unname(cbind(row_max, row_max))
})



# Cumsums

#' @rdname colCumsums-dgCMatrix-method
#' @export
setMethod("rowCumsums", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, ...){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  t(dgCMatrix_colCumsums(t(x)))
})



# Cumprods

#' @rdname colCumprods-dgCMatrix-method
#' @export
setMethod("rowCumprods", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, ...){
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
          function(x, rows = NULL, cols = NULL, ...){
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
          function(x, rows = NULL, cols = NULL, ...){
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
          function(x, rows = NULL, cols = NULL,  ties.method = c("max", "average", "min"), preserve.shape = TRUE, na.handling = c("keep", "last"), ...){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  colRanks(t(x), ties.method = ties.method, preserve.shape = !preserve.shape, na.handling = na.handling, ...)
})



#' @rdname colDiffs-dgCMatrix-method

#' @export
setMethod("rowDiffs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L, ...){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  t(colDiffs(t(x), lag = lag, differences = differences, ...))
})


# VarDiffs

#' @rdname colVarDiffs-dgCMatrix-method
#' @export
setMethod("rowVarDiffs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ...){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  colVarDiffs(t(x), na.rm=na.rm, diff=diff, trim = trim, ...)
})



# SdDiffs

#' @rdname colSdDiffs-dgCMatrix-method
#' @export
setMethod("rowSdDiffs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ...){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  colSdDiffs(t(x), na.rm=na.rm, diff=diff, trim = trim, ...)
})



# MadDiffs

#' @rdname colMadDiffs-dgCMatrix-method
#' @export
setMethod("rowMadDiffs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, constant = 1.4826, ...){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  colMadDiffs(t(x), na.rm=na.rm, diff=diff, trim = trim, constant = constant, ...)
})



# IQRDiffs

#' @rdname colIQRDiffs-dgCMatrix-method
#' @export
setMethod("rowIQRDiffs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ...){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  colIQRDiffs(t(x), na.rm=na.rm, diff=diff, trim = trim, ...)
})


