
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
