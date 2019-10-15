

# Sum

#' @rdname colSums2
#' @export
setGeneric("rowSums2", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::rowSums2(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colSums2
#' @export
setMethod("rowSums2", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colSums2(t(x), na_rm = na.rm))



# Mean

#' @rdname colMeans2
#' @export
setGeneric("rowMeans2", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::rowMeans2(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colMeans2
#' @export
setMethod("rowMeans2", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colMeans2(t(x), na_rm = na.rm))


# Median

#' @rdname colMedians
#' @export
setGeneric("rowMedians", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::rowMedians(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colMedians
#' @export
setMethod("rowMedians", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colMedians(t(x), na_rm = na.rm))

# Vars

#' @rdname colVars
#' @export
setGeneric("rowVars", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::rowVars(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colVars
#' @export
setMethod("rowVars", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colVars(t(x), na_rm = na.rm))


# Sds

#' @rdname colSds
#' @export
setGeneric("rowSds", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::rowSds(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colSds
#' @export
setMethod("rowSds", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            sqrt(dgCMatrix_colVars(t(x), na_rm = na.rm)))



# Mads

#' @rdname colMads
#' @export
setGeneric("rowMads", function(x, rows = NULL, cols = NULL, constant = 1.4826, na.rm=FALSE, ...){
  matrixStats::rowMads(as.matrix(x), rows = rows, cols = cols, constant = constant, na.rm = na.rm, ...)
})

#' @rdname colMads
#' @export
setMethod("rowMads", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, constant = 1.4826, na.rm=FALSE, ...)
            colMads(t(x), rows = rows, cols = cols, constant = constant, na.rm = na.rm, ...))


# LogSumExp

#' @rdname colLogSumExps
#' @export
setGeneric("rowLogSumExps", function(lx, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::rowLogSumExps(as.matrix(lx), rows = rows, cols = cols, na.rm = na.rm, center = center, dim. = dim., ...)
})

#' @rdname colLogSumExps
#' @export
setMethod("rowLogSumExps", signature(lx = "dgCMatrix"),
          function(lx, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colLogSumExps(t(lx), na_rm = na.rm))


# Prods

#' @rdname colProds
#' @export
setGeneric("rowProds", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::rowProds(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, center = center, dim. = dim., ...)
})

#' @rdname colProds
#' @export
setMethod("rowProds", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colProds(t(x), na_rm = na.rm))



# Min

#' @rdname colMins
#' @export
setGeneric("rowMins", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::rowMins(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colMins
#' @export
setMethod("rowMins", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colMins(t(x), na_rm = na.rm))


# Max

#' @rdname colMaxs
#' @export
setGeneric("rowMaxs", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::rowMaxs(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colMaxs
#' @export
setMethod("rowMaxs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colMaxs(t(x), na_rm = na.rm))


# OrderStats

#' @rdname colOrderStats
#' @export
setGeneric("rowOrderStats", function(x, rows = NULL, cols = NULL, which, ...){
  matrixStats::rowOrderStats(as.matrix(x), rows = rows, cols = cols, which = which, ...)
})

#' @rdname colOrderStats
#' @export
setMethod("rowOrderStats", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, which = 1, na.rm=FALSE, ...){
  if(which < 1 || which > ncol(x)){
    stop("Argument 'which' is out of range.")
  }
  dgCMatrix_colOrderStats(t(x), which = which, na_rm = na.rm)
})



# Weighted Means

#' @rdname colWeightedMeans
#' @export
setGeneric("rowWeightedMeans", function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::rowWeightedMeans(as.matrix(x), w = w, rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colWeightedMeans
#' @export
setMethod("rowWeightedMeans", signature(x = "dgCMatrix"),
    function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  colWeightedMeans(t(x), w = w, rows = rows, cols = cols, na.rm = na.rm)
})


# Weighted Medians

#' @rdname colWeightedMedians
#' @export
setGeneric("rowWeightedMedians", function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::rowWeightedMedians(as.matrix(x), w = w, rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colWeightedMedians
#' @export
setMethod("rowWeightedMedians", signature(x = "dgCMatrix"),
    function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  colWeightedMedians(t(x), w = w, rows = rows, cols = cols, na.rm = na.rm)
})


# Weighted Vars

#' @rdname colWeightedVars
#' @export
setGeneric("rowWeightedVars", function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::rowWeightedVars(as.matrix(x), w = w, rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colWeightedVars
#' @export
setMethod("rowWeightedVars", signature(x = "dgCMatrix"),
function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  colWeightedVars(t(x), w = w, rows = rows, cols = cols, na.rm = na.rm, ...)
})



# Weighted Sds

#' @rdname colWeightedSds
#' @export
setGeneric("rowWeightedSds", function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::rowWeightedSds(as.matrix(x), w = w, rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colWeightedSds
#' @export
setMethod("rowWeightedSds", signature(x = "dgCMatrix"),
          function(x, w = NULL, rows = NULL, cols = NULL, na.rm=FALSE, ...){
            if(is.null(w)){
              sqrt(dgCMatrix_colVars(t(x), na_rm = na.rm))
            }else{
              sqrt(dgCMatrix_colWeightedVars(t(x), weights = w, na_rm = na.rm))
            }
          })


# Weighted Mads

#' @rdname colWeightedMads
#' @export
setGeneric("rowWeightedMads", function(x, w = NULL, rows = NULL, cols = NULL, constant = 1.4826, na.rm=FALSE, ...){
  matrixStats::rowWeightedMads(as.matrix(x), w = w, rows = rows, cols = cols, constant = constant, na.rm = na.rm, ...)
})

#' @rdname colWeightedMads
#' @export
setMethod("rowWeightedMads", signature(x = "dgCMatrix"),
          function(x, w = NULL, rows = NULL, cols = NULL, constant = 1.4826, na.rm=FALSE,
                   ties = NULL, ...){
  colWeightedMads(t(x), w=w, rows=rows, cols=cols, constant = constant, na.rm=na.rm, ties = ties, ...)
})



# Count

#' @rdname colCounts
#' @export
setGeneric("rowCounts", function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...){
  matrixStats::rowCounts(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colCounts
#' @export
setMethod("rowCounts", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...)
            dgCMatrix_colCounts(t(x), value, na_rm = na.rm))


# AnyNA

#' @rdname colAnyNAs
#' @export
setGeneric("rowAnyNAs", function(x, rows = NULL, cols = NULL, ...){
  matrixStats::rowAnyNAs(as.matrix(x), rows = rows, cols = cols, ...)
})

#' @rdname colAnyNAs
#' @export
setMethod("rowAnyNAs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, ...)
            dgCMatrix_colAnyNAs(t(x)))


# Anys

#' @rdname colAnys
#' @export
setGeneric("rowAnys", function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...){
  matrixStats::rowAnys(as.matrix(x), rows = rows, cols = cols, value = value, na.rm = na.rm, ...)
})

#' @rdname colAnys
#' @export
setMethod("rowAnys", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...)
            dgCMatrix_colAnys(t(x), value, na_rm=na.rm))



# Alls

#' @rdname colAlls
#' @export
setGeneric("rowAlls", function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...){
  matrixStats::rowAlls(as.matrix(x), rows = rows, cols = cols, value = value, na.rm = na.rm, ...)
})

#' @rdname colAlls
#' @export
setMethod("rowAlls", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, value = TRUE, na.rm=FALSE, ...)
            dgCMatrix_colAlls(t(x), value, na_rm=na.rm))



# Collapse

#' @rdname colCollapse
#' @export
setGeneric("rowCollapse", function(x, idxs, rows = NULL, cols = NULL,  ...){
  matrixStats::rowCollapse(as.matrix(x), idxs, rows = rows, cols = cols, value = value, na.rm = na.rm, ...)
})

#' @rdname colCollapse
#' @export
setMethod("rowCollapse", signature(x = "dgCMatrix"),
          function(x, idxs, rows = NULL, cols = NULL, ...)
            x[idxs, ])




# Quantiles

#' @rdname colQuantiles
#' @export
setGeneric("rowQuantiles", function(x, rows = NULL, cols = NULL, probs = seq(from = 0, to = 1, by = 0.25), na.rm=FALSE, ...){
  matrixStats::rowQuantiles(as.matrix(x), rows = rows, cols = cols, probs = probs, na.rm = na.rm, ...)
})

#' @rdname colQuantiles
#' @export
setMethod("rowQuantiles", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, probs = seq(from = 0, to = 1, by = 0.25), na.rm=FALSE, ...){
            mat <- dgCMatrix_colQuantiles(t(x), probs, na_rm = na.rm)
            # Add dim names
            digits <- max(2L, getOption("digits"))
            colnames(mat) <- sprintf("%.*g%%", digits, 100 * probs)
            rownames(mat) <- rownames(x)
            mat
          })



# Tabulates

#' @rdname colTabulates
#' @export
setGeneric("rowTabulates", function(x, rows = NULL, cols = NULL, values = NULL, ...){
  matrixStats::rowTabulates(as.matrix(x), rows = rows, cols = cols, values = values, ...)
})

#' @rdname colTabulates
#' @export
setMethod("rowTabulates", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, values = NULL, ...){
  colTabulates(t(x), rows = rows, cols = cols, values = values)
})



# IQRs

#' @rdname colIQRs
#' @export
setGeneric("rowIQRs", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::rowIQRs(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colIQRs
#' @export
setMethod("rowIQRs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
            col_q <- colQuantiles(t(x), rows, cols, probs=c(0.25, 0.75), na.rm = na.rm)
            col_q[,2] - col_q[,1]
          })



# Ranges

#' @rdname colRanges
#' @export
setGeneric("rowRanges", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::rowRanges(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

#' @rdname colRanges
#' @export
setMethod("rowRanges", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
            tx <- t(x)
            row_max <- colMaxs(tx, rows, cols, na.rm = na.rm)
            row_max <- colMins(tx, rows, cols, na.rm = na.rm)
            unname(cbind(row_max, row_max))
          })



# Cumsums

#' @rdname colCumsums
#' @export
setGeneric("rowCumsums", function(x, rows = NULL, cols = NULL, ...){
  matrixStats::rowCumsums(as.matrix(x), rows = rows, cols = cols, ...)
})

#' @rdname colCumsums
#' @export
setMethod("rowCumsums", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, ...)
            t(dgCMatrix_colCumsums(t(x))))



# Cumprods

#' @rdname colCumprods
#' @export
setGeneric("rowCumprods", function(x, rows = NULL, cols = NULL, ...){
  matrixStats::rowCumprods(as.matrix(x), rows = rows, cols = cols, ...)
})

#' @rdname colCumprods
#' @export
setMethod("rowCumprods", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, ...)
            t(dgCMatrix_colCumprods(t(x))))



# Cummins

#' @rdname colCummins
#' @export
setGeneric("rowCummins", function(x, rows = NULL, cols = NULL, ...){
  matrixStats::rowCummins(as.matrix(x), rows = rows, cols = cols, ...)
})

#' @rdname colCummins
#' @export
setMethod("rowCummins", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, ...)
            t(dgCMatrix_colCummins(t(x))))



# Cummaxs

#' @rdname colCummaxs
#' @export
setGeneric("rowCummaxs", function(x, rows = NULL, cols = NULL, ...){
  matrixStats::rowCummaxs(as.matrix(x), rows = rows, cols = cols, ...)
})

#' @rdname colCummaxs
#' @export
setMethod("rowCummaxs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, ...)
            t(dgCMatrix_colCummaxs(t(x))))



# Ranks

#' @rdname colRanks
#' @export
setGeneric("rowRanks", function(x, rows = NULL, cols = NULL, ties.method = c("max", "average", "min"), preserve.shape = TRUE, ...){
  matrixStats::rowRanks(as.matrix(x), rows = rows, cols = cols, ties.method = ties.method, preserveShape = preserve.shape, ...)
})

#' @rdname colRanks
#' @export
setMethod("rowRanks", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL,  ties.method = c("max", "average", "min"), preserve.shape = TRUE, na.handling = c("keep", "last"), ...){
  colRanks(t(x), ties.method = ties.method, preserve.shape = !preserve.shape, na.handling = na.handling, ...)
})



#' @rdname colDiffs
#'
#' @export
setGeneric("rowDiffs", function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L,...){
  matrixStats::rowDiffs(as.matrix(x), rows = rows, cols = cols, lag = lag, differences=differences, ...)
})

#' @rdname colDiffs
#' @export
setMethod("rowDiffs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, lag = 1L, differences = 1L, ...){
  t(colDiffs(t(x), lag = lag, differences = differences, ...))
})


# VarDiffs

#' @rdname colVarDiffs
#' @export
setGeneric("rowVarDiffs", function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ...){
  matrixStats::rowVarDiffs(as.matrix(x), rows = rows, cols = cols, na.rm=na.rm, diff=diff, trim = trim, ...)
})

#' @rdname colVarDiffs
#' @export
setMethod("rowVarDiffs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ...){
  colVarDiffs(t(x), na.rm=na.rm, diff=diff, trim = trim, ...)
})



# SdDiffs

#' @rdname colSdDiffs
#' @export
setGeneric("rowSdDiffs", function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ...){
  matrixStats::rowSdDiffs(as.matrix(x), rows = rows, cols = cols, na.rm=na.rm, diff=diff, trim = trim, ...)
})

#' @rdname colSdDiffs
#' @export
setMethod("rowSdDiffs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ...){
  colSdDiffs(t(x), na.rm=na.rm, diff=diff, trim = trim, ...)
})



# MadDiffs

#' @rdname colMadDiffs
#' @export
setGeneric("rowMadDiffs", function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, constant = 1.4826, ...){
  matrixStats::rowMadDiffs(as.matrix(x), rows = rows, cols = cols, na.rm=na.rm, diff=diff, trim = trim, constant = constant, ...)
})

#' @rdname colMadDiffs
#' @export
setMethod("rowMadDiffs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, constant = 1.4826, ...){
  colMadDiffs(t(x), na.rm=na.rm, diff=diff, trim = trim, constant = constant, ...)
})



# IQRDiffs

#' @rdname colIQRDiffs
#' @export
setGeneric("rowIQRDiffs", function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ...){
  matrixStats::rowIQRDiffs(as.matrix(x), rows = rows, cols = cols, na.rm=na.rm, diff=diff, trim = trim, ...)
})

#' @rdname colIQRDiffs
#' @export
setMethod("rowIQRDiffs", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm = FALSE, diff = 1L, trim = 0, ...){
  colIQRDiffs(t(x), na.rm=na.rm, diff=diff, trim = trim, ...)
})


