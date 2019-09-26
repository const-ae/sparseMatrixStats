

# Sum

#
setGeneric("colSums2", function(x, rows = NULL, cols = NULL, na.rm=TRUE, center = NULL, dim. = dim(x), ...){
  matrixStats::colSums2(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, center = center, dim. = dim., ...)
})

setMethod("colSums2", signature(x = "dgCMatrix"),
          function(x, na.rm = FALSE, dims = 1, sparseResult = FALSE)
            dgCMatrix_colSums2(x, na_rm = na.rm))



# Mean


# Vars


setGeneric("colVars", function(x, rows = NULL, cols = NULL, na.rm=TRUE, center = NULL, dim. = dim(x), ...){
  matrixStats::colVars(as.matrix(x), rows = rows, cols = cols, na.rm = na.rm, center = center, dim. = dim., ...)
})




setMethod("colVars", signature(x = "dgCMatrix"),
          function(x, na.rm = FALSE, dims = 1, sparseResult = FALSE)
            dgCMatrix_colVars(x, na_rm = na.rm))

