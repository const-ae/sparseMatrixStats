

make_matrix <- function(nrow, ncol, frac_zero = 0.8, frac_na = 0){
  n_total <- ncol * nrow
  data <- rnorm(n = n_total, mean=0, sd=10)
  data[sample(seq_len(n_total), size = round(n_total * frac_zero))] <- 0
  data[sample(seq_len(n_total), size = round(n_total * frac_na))] <- NA
  matrix(data, nrow=nrow, ncol=ncol)
}

make_matrix_with_all_features <- function(nrow, ncol){
  if(ncol < 8){
    stop("Too few columns")
  }
  mat <- make_matrix(nrow, ncol, frac_zero = 0.8)
  # All observed
  mat[,1] <- rnorm(nrow)
  # All zero
  mat[,2] <- 0
  # Some missing
  mat[7, 3] <- NA
  # All missing
  mat[,4] <- NA
  # Some infinite
  mat[1, 5] <- Inf
  mat[3, 6] <- -Inf
  # Some with known value
  mat[1, 7] <- 42

  mat
}
