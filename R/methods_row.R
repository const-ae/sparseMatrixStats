

# Sum

#' @rdname colSums2-xgCMatrix-method
#' @export
setMethod("rowSums2", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, useNames = TRUE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  # dgCMatrix_colSums2(t(x), na_rm = na.rm)
  set_result_names_t(dgCMatrix_rowSums2(x, na_rm = na.rm), useNames)
})



# Mean

#' @rdname colMeans2-xgCMatrix-method
#' @export
setMethod("rowMeans2", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, useNames = TRUE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  # dgCMatrix_colMeans2(t(x), na_rm = na.rm)
  set_result_names_t(dgCMatrix_rowMeans2(x, na_rm = na.rm), useNames)
})


# Median

#' @rdname colMedians-dgCMatrix-method
#' @export
setMethod("rowMedians", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, useNames = TRUE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  set_result_names_t(dgCMatrix_colMedians(t(x), na_rm = na.rm), useNames)
})


# Vars

#' @rdname colVars-xgCMatrix-method
#' @export
setMethod("rowVars", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, center = NULL, useNames = TRUE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  # dgCMatrix_colVars(t(x), na_rm = na.rm)
  set_result_names_t(dgCMatrix_rowVars(x, na_rm = na.rm, center = center), useNames, default = ncol(x) > 0 && !is.null(center))
})


# Sds

#' @rdname colSds-xgCMatrix-method
#' @export
setMethod("rowSds", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, center = NULL, useNames = TRUE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  set_result_names_t(sqrt(dgCMatrix_rowVars(x, na_rm = na.rm, center = center)), useNames)
})



# Mads

#' @rdname colMads-dgCMatrix-method
#' @export
setMethod("rowMads", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, center = NULL, constant = 1.4826, na.rm=FALSE, useNames = TRUE){
  colMads(t(x), rows = cols, cols = rows, center = center, constant = constant, na.rm = na.rm, useNames = useNames)
})


# LogSumExp

#' @rdname colLogSumExps-xgCMatrix-method
#' @export
setMethod("rowLogSumExps", signature(lx = "xgCMatrix"),
          function(lx, rows = NULL, cols = NULL, na.rm=FALSE, useNames = TRUE){
  if(! is.null(rows)){
    lx <- lx[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    lx <- lx[, cols, drop = FALSE]
  }
  set_result_names_t(dgCMatrix_colLogSumExps(t(lx), na_rm = na.rm), useNames, default = TRUE, names = rownames(lx))
})


# Prods

#' @rdname colProds-xgCMatrix-method
#' @export
setMethod("rowProds", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, useNames = TRUE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  set_result_names_t(dgCMatrix_colProds(t(x), na_rm = na.rm), useNames)
})



# Min

#' @rdname colMins-dgCMatrix-method
#' @export
setMethod("rowMins", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, useNames = TRUE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  set_result_names_t(dgCMatrix_colMins(t(x), na_rm = na.rm), useNames)
})


# Max

#' @rdname colMaxs-dgCMatrix-method
#' @export
setMethod("rowMaxs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, useNames = TRUE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  set_result_names_t(dgCMatrix_colMaxs(t(x), na_rm = na.rm), useNames)
})


# OrderStats

#' @rdname colOrderStats-dgCMatrix-method
#' @export
setMethod("rowOrderStats", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, which = 1, useNames = TRUE, na.rm=FALSE){
  if(which < 1 || which > ncol(x)){
    stop("Argument 'which' is out of range.")
  }
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  set_result_names_t(dgCMatrix_colOrderStats(t(x), which = which, na_rm = na.rm), useNames)
})



# Weighted Means

#' @rdname colWeightedMeans-xgCMatrix-method
#' @export
setMethod("rowWeightedMeans", signature(x = "xgCMatrix"),
    function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE, useNames = TRUE){
  colWeightedMeans(t(x), w = w, rows = cols, cols = rows, na.rm = na.rm, useNames = useNames)
})


# Weighted Medians

#' @rdname colWeightedMedians-dgCMatrix-method
#' @export
setMethod("rowWeightedMedians", signature(x = "dgCMatrix"),
    function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE, useNames = TRUE){
  colWeightedMedians(t(x), w = w, rows = cols, cols = rows, na.rm = na.rm, useNames = useNames)
})


# Weighted Vars

#' @rdname colWeightedVars-xgCMatrix-method
#' @export
setMethod("rowWeightedVars", signature(x = "xgCMatrix"),
function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE, useNames = TRUE){
  colWeightedVars(t(x), w = w, rows = cols, cols = rows, na.rm = na.rm, useNames = useNames)
})



# Weighted Sds

#' @rdname colWeightedSds-xgCMatrix-method
#' @export
setMethod("rowWeightedSds", signature(x = "xgCMatrix"),
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE, useNames = TRUE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
    w <- w[cols]
  }
  if(is.null(w)){
    set_result_names_t(sqrt(dgCMatrix_rowVars(x, na_rm = na.rm, center = NULL)), useNames, default = TRUE)
  }else{
    set_result_names_t(sqrt(dgCMatrix_colWeightedVars(t(x), weights = w, na_rm = na.rm)), useNames, default = TRUE)
  }
})


# Weighted Mads

#' @rdname colWeightedMads-dgCMatrix-method
#' @export
setMethod("rowWeightedMads", signature(x = "dgCMatrix"),
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE,  constant = 1.4826, center = NULL, useNames = TRUE){
  colWeightedMads(t(x), w=w, rows = cols, cols = rows, na.rm=na.rm, constant = constant, center = center, useNames = useNames)
})



# Count

#' @rdname colCounts-xgCMatrix-method
#' @export
setMethod("rowCounts", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, useNames = TRUE){
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
  set_result_names_t(dgCMatrix_colCounts(t(x), value, na_rm = na.rm), useNames)
})


# AnyNA

#' @rdname colAnyNAs-xgCMatrix-method
#' @export
setMethod("rowAnyNAs", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL, useNames = TRUE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  set_result_names_t(dgCMatrix_colAnyNAs(t(x)), useNames)
})


# Anys

#' @rdname colAnys-xgCMatrix-method
#' @export
setMethod("rowAnys", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, useNames = TRUE){
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
  res <- if(isTRUE(value)){
    ! dgCMatrix_colAlls(t(x), value = 0, na_rm=na.rm)
  }else{
    dgCMatrix_colAnys(t(x), value, na_rm=na.rm)
  }
  set_result_names_t(res, useNames)
})



# Alls

#' @rdname colAlls-xgCMatrix-method
#' @export
setMethod("rowAlls", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, useNames = TRUE){
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
  res <- if(isTRUE(value)){
    ! dgCMatrix_colAnys(t(x), value = 0, na_rm = na.rm)
  }else{
    dgCMatrix_colAlls(t(x), value, na_rm=na.rm)
  }
  set_result_names_t(res, useNames)
})



# Collapse

#' @rdname colCollapse-xgCMatrix-method
#' @export
setMethod("rowCollapse", signature(x = "xgCMatrix"),
          function(x, idxs, rows = NULL, useNames = TRUE){
  idxs <- rep(idxs, length.out = nrow(x))
  if (!is.null(rows)) {
    x <- x[rows, , drop = FALSE]
    idxs <- idxs[rows]
  }
  cols <- seq_len(ncol(x)) - 1L
  cols <- cols[idxs]
  idxs <- nrow(x) * cols + seq_len(nrow(x))
  cols <- NULL
  set_result_names_t(x[idxs], useNames)
})




# Quantiles

#' @rdname colQuantiles-xgCMatrix-method
#' @export
setMethod("rowQuantiles", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL, probs = seq(from = 0, to = 1, by = 0.25), na.rm=FALSE, type = 7L, useNames = TRUE, drop = TRUE){
  colQuantiles(t(x), rows = cols, cols = rows, probs = probs, na.rm = na.rm, type = type, useNames = useNames, drop = drop)
})



# Tabulates

#' @rdname colTabulates-xgCMatrix-method
#' @export
setMethod("rowTabulates", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL, values = NULL, useNames = TRUE){
  colTabulates(t(x), rows = cols, cols = rows, values = values, useNames = useNames)
})



# IQRs

#' @rdname colIQRs-xgCMatrix-method
#' @export
setMethod("rowIQRs", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, useNames = TRUE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  col_q <- colQuantiles(t(x), probs=c(0.25, 0.75), na.rm = na.rm, useNames = TRUE, drop = FALSE)
  if(is.na(useNames)){
    useNames <- FALSE
  }
  set_result_names_t(unname(col_q[,2] - col_q[,1]), useNames, names = rownames(col_q))
})



# Ranges

#' @rdname colRanges-dgCMatrix-method
#' @export
setMethod("rowRanges", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, useNames = TRUE){
  tx <- t(x)
  row_max <- colMaxs(tx, rows = cols, cols = rows, na.rm = na.rm, useNames = TRUE)
  row_min <- colMins(tx, rows = cols, cols = rows, na.rm = na.rm, useNames = TRUE)
  if(is.na(useNames)){
    useNames <- FALSE
  }
  set_result_rownames(unname(cbind(row_min, row_max)), useNames, names = names(row_max))
})



# Cumsums

#' @rdname colCumsums-xgCMatrix-method
#' @export
setMethod("rowCumsums", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL, useNames = TRUE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  set_result_rownames(t(dgCMatrix_colCumsums(t(x))), useNames, names = rownames(x))
})



# Cumprods

#' @rdname colCumprods-xgCMatrix-method
#' @export
setMethod("rowCumprods", signature(x = "xgCMatrix"),
          function(x, rows = NULL, cols = NULL, useNames = TRUE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
 set_result_rownames(t(dgCMatrix_colCumprods(t(x))), useNames, names = rownames(x))
})



# Cummins

#' @rdname colCummins-dgCMatrix-method
#' @export
setMethod("rowCummins", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, useNames = TRUE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  set_result_rownames(t(dgCMatrix_colCummins(t(x))), useNames, names = rownames(x))
})



# Cummaxs

#' @rdname colCummaxs-dgCMatrix-method
#' @export
setMethod("rowCummaxs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, useNames = TRUE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  set_result_rownames(t(dgCMatrix_colCummaxs(t(x))), useNames, names = rownames(x))
})



# Ranks

#' @rdname colRanks-dgCMatrix-method
#' @export
setMethod("rowRanks", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL,  ties.method = c("max", "average", "min"), preserveShape = TRUE, na.handling = c("keep", "last"), ..., useNames = TRUE){
  if(! is.null(rows)){
    x <- x[rows, , drop = FALSE]
  }
  if(! is.null(cols)){
    x <- x[, cols, drop = FALSE]
  }
  colRanks(t(x), ties.method = ties.method, preserveShape = ! preserveShape, na.handling = na.handling, useNames = useNames)
})



#' @rdname colDiffs-dgCMatrix-method

#' @export
setMethod("rowDiffs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L, useNames = TRUE){
  t(colDiffs(t(x), rows = cols, cols = rows, lag = lag, differences = differences, useNames = useNames))
})


# VarDiffs

#' @rdname colVarDiffs-dgCMatrix-method
#' @export
setMethod("rowVarDiffs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, useNames = TRUE){
  colVarDiffs(t(x), rows = cols, cols = rows, na.rm=na.rm, diff=diff, trim = trim, useNames = useNames)
})



# SdDiffs

#' @rdname colSdDiffs-dgCMatrix-method
#' @export
setMethod("rowSdDiffs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, useNames = TRUE){
  colSdDiffs(t(x), rows = cols, cols = rows, na.rm=na.rm, diff=diff, trim = trim, useNames = useNames)
})



# MadDiffs

#' @rdname colMadDiffs-dgCMatrix-method
#' @export
setMethod("rowMadDiffs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, constant = 1.4826, ..., useNames = TRUE){
  colMadDiffs(t(x), rows = cols, cols = rows, na.rm=na.rm, diff=diff, trim = trim, constant = constant, useNames = useNames)
})



# IQRDiffs

#' @rdname colIQRDiffs-dgCMatrix-method
#' @export
setMethod("rowIQRDiffs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, useNames = TRUE){
  colIQRDiffs(t(x), rows = cols, cols = rows, na.rm=na.rm, diff=diff, trim = trim, useNames = useNames)
})


#' @rdname colAvgsPerRowSet-xgCMatrix-method
#' @export
setMethod("rowAvgsPerColSet", signature(X = "xgCMatrix"),
          function(X, W = NULL, rows = NULL, S, FUN = rowMeans2, ..., na.rm = NA, tFUN = FALSE){
  tZ <- colAvgsPerRowSet(t(X), W = W, cols = rows, S = S, FUN  = FUN, ..., na.rm = na.rm, tFUN = ! tFUN)
  t(tZ)
})

