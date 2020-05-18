

#' Union of double and logical column-sparse matrices
#'
#' Union of dgCMatrix and lgCMatrix
#'
#'
#' @importClassesFrom Matrix dgCMatrix lgCMatrix
setClassUnion("xgCMatrix", members = c("dgCMatrix", "lgCMatrix"))

