

# Sum

setGeneric("rowSums2", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::rowSums2(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

setMethod("rowSums2", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colSums2(t(x), na_rm = na.rm))



# Mean
setGeneric("rowMeans2", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colSums2(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

setMethod("rowMeans2", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colMeans2(t(x), na_rm = na.rm))


# Median

setGeneric("rowMedians", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::rowMedians(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

setMethod("rowMedians", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colMedians(t(x), na_rm = na.rm))

# Vars


setGeneric("rowVars", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::rowVars(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

setMethod("rowVars", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colVars(t(x), na_rm = na.rm))


# Sds


setGeneric("rowSds", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::rowSds(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

setMethod("rowSds", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            sqrt(dgCMatrix_colVars(t(x), na_rm = na.rm)))


# LogSumExp

setGeneric("rowLogSumExps", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colLogSumExps(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, center = center, dim. = dim., ...)
})

setMethod("rowLogSumExps", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            exp(dgCMatrix_colSums2(log(t(x)), na_rm = na.rm)))


# Prods

setGeneric("rowProds", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::rowProds(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, center = center, dim. = dim., ...)
})

setMethod("rowProds", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colProds(t(x), na_rm = na.rm))



# Min


setGeneric("rowMins", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::rowMins(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

setMethod("rowMins", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colMins(t(x), na_rm = na.rm))


# Max

setGeneric("rowMaxs", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::rowMaxs(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

setMethod("rowMaxs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colMaxs(t(x), na_rm = na.rm))


# Count

setGeneric("rowCounts", function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...){
  matrixStats::rowCounts(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

setMethod("rowCounts", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...)
            dgCMatrix_colCounts(t(x), value, na_rm = na.rm))


# AnyNA

setGeneric("rowAnyNAs", function(x, rows = NULL, cols = NULL, ...){
  matrixStats::rowAnyNAs(as.matrix(x), rows = rows, cols = cols, ...)
})

setMethod("rowAnyNAs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, ...)
            dgCMatrix_colAnyNAs(t(x)))


# Anys

setGeneric("rowAnys", function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...){
  matrixStats::rowAnys(as.matrix(x), rows = rows, cols = cols, value = value, na.rm = na.rm, ...)
})

setMethod("rowAnys", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...)
            dgCMatrix_colAnys(t(x), value, na_rm=na.rm))



# Alls

setGeneric("rowAlls", function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...){
  matrixStats::rowAlls(as.matrix(x), rows = rows, cols = cols, value = value, na.rm = na.rm, ...)
})

setMethod("rowAlls", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...)
            dgCMatrix_colAlls(t(x), value, na_rm=na.rm))



# Collapse

setGeneric("rowCollapse", function(x, idxs, rows = NULL, cols = NULL,  ...){
  matrixStats::rowCollapse(as.matrix(x), idxs, rows = rows, cols = cols, value = value, na.rm = na.rm, ...)
})

setMethod("rowCollapse", signature(x = "dgCMatrix"),
          function(x, idxs, rows = NULL, cols = NULL, ...)
            x[idxs, ])




# Quantiles


setGeneric("rowQuantiles", function(x, rows = NULL, cols = NULL, probs = seq(from = 0, to = 1, by = 0.25), na.rm=FALSE, ...){
  matrixStats::rowQuantiles(as.matrix(x), rows = rows, cols = cols, probs = probs, na.rm = na.rm, ...)
})

setMethod("rowQuantiles", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, probs = seq(from = 0, to = 1, by = 0.25), na.rm=FALSE, ...){
            mat <- dgCMatrix_colQuantiles(t(x), probs, na_rm = na.rm)
            # Add dim names
            digits <- max(2L, getOption("digits"))
            colnames(mat) <- sprintf("%.*g%%", digits, 100 * probs)
            rownames(mat) <- rownames(x)
            mat
          })



# IQRs

setGeneric("rowIQRs", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::rowIQRs(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

setMethod("rowIQRs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
            col_q <- colQuantiles(t(x), rows, cols, probs=c(0.25, 0.75), na.rm = na.rm)
            col_q[,2] - col_q[,1]
          })



# Ranges


setGeneric("rowRanges", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::rowRanges(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

setMethod("rowRanges", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
            tx <- t(x)
            row_max <- colMaxs(tx, rows, cols, na.rm = na.rm)
            row_max <- colMins(tx, rows, cols, na.rm = na.rm)
            unname(cbind(row_max, row_max))
          })



# Cumsums


setGeneric("rowCumsums", function(x, rows = NULL, cols = NULL, ...){
  matrixStats::rowCumsums(as.matrix(x), rows = rows, cols = cols, ...)
})

setMethod("rowCumsums", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, ...)
            dgCMatrix_colCumsums(t(x)))



# Cumprods


setGeneric("rowCumprods", function(x, rows = NULL, cols = NULL, ...){
  matrixStats::rowCumprods(as.matrix(x), rows = rows, cols = cols, ...)
})

setMethod("rowCumprods", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, ...)
            dgCMatrix_colCumprods(t(x)))



# Cummins


setGeneric("rowCummins", function(x, rows = NULL, cols = NULL, ...){
  matrixStats::rowCummins(as.matrix(x), rows = rows, cols = cols, ...)
})

setMethod("rowCummins", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, ...)
            dgCMatrix_colCummins(t(x)))



# Cummaxs


setGeneric("rowCummaxs", function(x, rows = NULL, cols = NULL, ...){
  matrixStats::rowCummaxs(as.matrix(x), rows = rows, cols = cols, ...)
})

setMethod("rowCummaxs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, ...)
            dgCMatrix_colCummaxs(t(x)))

