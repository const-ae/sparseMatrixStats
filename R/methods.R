

# Sum

#
setGeneric("colSums2", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colSums2(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, center = center, dim. = dim., ...)
})

setMethod("colSums2", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colSums2(x, na_rm = na.rm))



# Mean
setGeneric("colMeans2", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colSums2(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

setMethod("colMeans2", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colMeans2(x, na_rm = na.rm))

# Vars


setGeneric("colVars", function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...){
  matrixStats::colVars(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, ...)
})

setMethod("colVars", signature(x = "dgCMatrix"),
          function(x, rows = NULL, cols = NULL, na.rm=FALSE, ...)
            dgCMatrix_colVars(x, na_rm = na.rm))


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
