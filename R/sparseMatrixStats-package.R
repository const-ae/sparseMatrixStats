## usethis namespace: start
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL

# NOTE: Import a single function to quieten R CMD check NOTE:
#         Package in Depends field not imported from: ‘matrixStats’
#           These packages need to be imported from (in the NAMESPACE file)
#           for when this namespace is loaded but not attached.
#' @importFrom matrixStats allocArray
NULL


#' @useDynLib sparseMatrixStats
NULL

#' @import methods
NULL

#' @importFrom Matrix t
NULL

#' @import MatrixGenerics
NULL

#' @importFrom stats setNames
NULL
