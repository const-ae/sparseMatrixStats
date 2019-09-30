

# Sum

setGeneric("colSums2", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colSums2(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, center = center, dim. = dim., ...)
})

setMethod("colSums2", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colSums2(x, na_rm = na.rm))



# Mean
setGeneric("colMeans2", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colMeans2(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

setMethod("colMeans2", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colMeans2(x, na_rm = na.rm))


# Median

setGeneric("colMedians", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colMedians(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

setMethod("colMedians", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colMedians(x, na_rm = na.rm))

# Vars


setGeneric("colVars", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colVars(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

setMethod("colVars", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colVars(x, na_rm = na.rm))


# Sds


setGeneric("colSds", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colSds(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

setMethod("colSds", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            sqrt(dgCMatrix_colVars(x, na_rm = na.rm)))


# LogSumExp

setGeneric("colLogSumExps", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colLogSumExps(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, center = center, dim. = dim., ...)
})

setMethod("colLogSumExps", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            exp(dgCMatrix_colSums2(log(x), na_rm = na.rm)))


# Prods

setGeneric("colProds", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colProds(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, center = center, dim. = dim., ...)
})

setMethod("colProds", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colProds(x, na_rm = na.rm))



# Min


setGeneric("colMins", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colMins(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

setMethod("colMins", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colMins(x, na_rm = na.rm))


# Max

setGeneric("colMaxs", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colMaxs(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

setMethod("colMaxs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colMaxs(x, na_rm = na.rm))


# Count

setGeneric("colCounts", function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...){
  matrixStats::colCounts(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

setMethod("colCounts", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...)
            dgCMatrix_colCounts(x, value, na_rm = na.rm))


# AnyNA

setGeneric("colAnyNAs", function(x, rows = NULL, cols = NULL, ...){
  matrixStats::colAnyNAs(as.matrix(x), rows = rows, cols = cols, ...)
})

setMethod("colAnyNAs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, ...)
            dgCMatrix_colAnyNAs(x))


# Anys

setGeneric("colAnys", function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...){
  matrixStats::colAnys(as.matrix(x), rows = rows, cols = cols, value = value, na.rm = na.rm, ...)
})

setMethod("colAnys", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...)
            dgCMatrix_colAnys(x, value, na_rm=na.rm))



# Alls

setGeneric("colAlls", function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...){
  matrixStats::colAlls(as.matrix(x), rows = rows, cols = cols, value = value, na.rm = na.rm, ...)
})

setMethod("colAlls", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...)
            dgCMatrix_colAlls(x, value, na_rm=na.rm))



# Collapse

setGeneric("colCollapse", function(x, idxs, rows = NULL, cols = NULL,  ...){
  matrixStats::colCollapse(as.matrix(x), idxs, rows = rows, cols = cols, value = value, na.rm = na.rm, ...)
})

setMethod("colCollapse", signature(x = "dgCMatrix"),
          function(x, idxs, rows = NULL, cols = NULL, ...)
            x[, idxs])




# colQuantiles


setGeneric("colQuantiles", function(x, rows = NULL, cols = NULL, probs = seq(from = 0, to = 1, by = 0.25), na.rm=FALSE, ...){
  matrixStats::colQuantiles(as.matrix(x), rows = rows, cols = cols, probs = probs, na.rm = na.rm, ...)
})

setMethod("colQuantiles", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, probs = seq(from = 0, to = 1, by = 0.25), na.rm=FALSE, ...){
  mat <- dgCMatrix_colQuantiles(x, probs, na_rm = na.rm)
  # Add dim names
  digits <- max(2L, getOption("digits"))
  colnames(mat) <- sprintf("%.*g%%", digits, 100 * probs)
  rownames(mat) <- rownames(x)
  mat
})



# colIQRs

setGeneric("colIQRs", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colIQRs(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

setMethod("colIQRs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  col_q <- colQuantiles(x, rows, cols, probs=c(0.25, 0.75), na.rm = na.rm)
  col_q[,2] - col_q[,1]
})



# colRanges


setGeneric("colRanges", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colRanges(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

setMethod("colRanges", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
            col_max <- colMaxs(x, rows, cols, na.rm = na.rm)
            col_min <- colMins(x, rows, cols, na.rm = na.rm)
            unname(cbind(col_min, col_max))
          })



# colCumsums


setGeneric("colCumsums", function(x, rows = NULL, cols = NULL, ...){
  matrixStats::colCumsums(as.matrix(x), rows = rows, cols = cols, ...)
})

setMethod("colCumsums", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, ...)
            dgCMatrix_colCumsums(x))



# colCumprods


setGeneric("colCumprods", function(x, rows = NULL, cols = NULL, ...){
  matrixStats::colCumprods(as.matrix(x), rows = rows, cols = cols, ...)
})

setMethod("colCumprods", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, ...)
            dgCMatrix_colCumprods(x))



# colCummins


setGeneric("colCummins", function(x, rows = NULL, cols = NULL, ...){
  matrixStats::colCummins(as.matrix(x), rows = rows, cols = cols, ...)
})

setMethod("colCummins", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, ...)
            dgCMatrix_colCummins(x))



# colCummaxs


setGeneric("colCummaxs", function(x, rows = NULL, cols = NULL, ...){
  matrixStats::colCummaxs(as.matrix(x), rows = rows, cols = cols, ...)
})

setMethod("colCummaxs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, ...)
            dgCMatrix_colCummaxs(x))

