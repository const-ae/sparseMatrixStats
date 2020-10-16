
reduce_sparse_matrix_to_num <- function(sp_mat, reduce_function = function(values, row_indices, number_of_zeros){ NA_real_}){
  if(length(sp_mat@p) == 0){
    numeric(0)
  }else{
    vapply(seq_len(length(sp_mat@p)-1), function(index){
      start_pos <- sp_mat@p[index]
      end_pos <- sp_mat@p[index + 1]
      number_of_zeros <- nrow(sp_mat) - (end_pos - start_pos)
      values <- sp_mat@x[start_pos + seq_len(end_pos - start_pos)]
      row_indices <- sp_mat@i[start_pos + seq_len(end_pos - start_pos)]
      reduce_function(values, row_indices, number_of_zeros)
    }, FUN.VALUE = 0.0)
  }
}


reduce_sparse_matrix_to_matrix <- function(sp_mat, n_result_rows, reduce_function = function(values, row_indices, number_of_zeros){ NA_real_}){
  if(length(sp_mat@p) == 0){
    numeric(0)
  }else{
    res <- vapply(seq_len(length(sp_mat@p)-1), function(index){
      start_pos <- sp_mat@p[index]
      end_pos <- sp_mat@p[index + 1]
      number_of_zeros <- nrow(sp_mat) - (end_pos - start_pos)
      values <- sp_mat@x[start_pos + seq_len(end_pos - start_pos)]
      row_indices <- sp_mat@i[start_pos + seq_len(end_pos - start_pos)]
      reduce_function(values, row_indices, number_of_zeros)
    }, FUN.VALUE = rep(0.0, n_result_rows))
    if(n_result_rows == 1){
      matrix(res, nrow=1, ncol=length(res))
    }else{
      res
    }
  }
}


expand_and_reduce_sparse_matrix_to_matrix <- function(sp_mat, n_result_rows, reduce_function = function(dense_values){ NA_real_}){
  if(length(sp_mat@p) == 0){
    numeric(0)
  }else{
    res <- vapply(seq_len(length(sp_mat@p)-1), function(index){
      start_pos <- sp_mat@p[index]
      end_pos <- sp_mat@p[index + 1]
      number_of_zeros <- nrow(sp_mat) - (end_pos - start_pos)
      values <- sp_mat@x[start_pos + seq_len(end_pos - start_pos)]
      row_indices <- sp_mat@i[start_pos + seq_len(end_pos - start_pos)]
      dense_values <- rep(0, nrow(sp_mat))
      storage.mode(dense_values) <- typeof(values)
      dense_values[row_indices + 1] <- values
      reduce_function(dense_values)
    }, FUN.VALUE = vector(mode = typeof(sp_mat@x), n_result_rows))
    if(n_result_rows == 1){
      matrix(res, nrow=1, ncol=length(res))
    }else{
      res
    }
  }
}
