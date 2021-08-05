
#============== Name setting functions ==============#

# set the names attribute of res depending on useNames. If `useNames = NA`, use the value from `default`.
# This method is specifically designed for the colXXX methods were the main argument is
# usually called `x`. Instead of explicitly passing this information, I use a bit of reflection
# to get the colnames of x
set_result_names <- function(res, useNames, default = FALSE, names = colnames(parent.frame()$x)){
  if(is.na(useNames)){
    useNames <- default
  }
  if (useNames) {
    if (!is.null(names)) {
      # Zero-length attribute? Keep behavior same as base R function
      if (length(names) == 0L) names <- NULL
      names(res) <- names
    }
  } else {
    names(res) <- NULL
  }
  res
}

# same as `set_result_names()` but set the rownames of res
set_result_rownames <- function(res, useNames, default = FALSE, names = colnames(parent.frame()$x)){
  if(is.na(useNames)){
    useNames <- default
  }
  if (useNames) {
    if (!is.null(names)) {
      # Zero-length attribute? Keep behavior same as base R function
      if (length(names) == 0L) names <- NULL
      rownames(res) <- names
    }
  } else {
    rownames(res) <- NULL
  }
  res
}

# same as `set_result_names()` but set the colnames of res
set_result_colnames <- function(res, useNames, default = FALSE, names = colnames(parent.frame()$x)){
  if(is.na(useNames)){
    useNames <- default
  }
  if (useNames) {
    if (!is.null(names)) {
      # Zero-length attribute? Keep behavior same as base R function
      if (length(names) == 0L) names <- NULL
      colnames(res) <- names
    }
  } else {
    colnames(res) <- NULL
  }
  res
}




# same as `set_result_names()` but use names = rownames(x) as default
set_result_names_t <- function(res, useNames, default = FALSE, names = rownames(parent.frame()$x)){
  if(is.na(useNames)){
    useNames <- default
  }
  if (useNames) {
    if (!is.null(names)) {
      # Zero-length attribute? Keep behavior same as base R function
      if (length(names) == 0L) names <- NULL
      names(res) <- names
    }
  } else {
    names(res) <- NULL
  }
  res
}


