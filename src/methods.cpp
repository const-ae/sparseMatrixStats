#include <Rcpp.h>
#include "SparseMatrixView.hpp"
#include "ColumnView.hpp"
#include "VectorSubsetView.hpp"
#include "SkipNAVectorSubsetView.hpp"

using namespace Rcpp;

template<typename Functor>
NumericVector reduce_matrix(S4 matrix, bool na_rm, Functor op){
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  std::vector<double> result;
  result.reserve(sp_mat.ncol);
  if(na_rm){
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
      [op](ColumnView::col_container col) -> double {
        SkipNAVectorSubsetView<REALSXP> values_wrapper(&col.values);
        SkipNAVectorSubsetView<INTSXP> row_indices_wrapper(&col.row_indices);
        return op(values_wrapper, row_indices_wrapper, col.number_of_zeros);
      });
  }else{
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
      [op](ColumnView::col_container col) -> double {
        return op(col.values, col.row_indices, col.number_of_zeros);
      });
  }
  return wrap(result);
}


// [[Rcpp::export]]
NumericVector dgCMatrix_colSums2(S4 matrix, bool na_rm){
  return reduce_matrix(matrix, na_rm, [](auto values, auto row_indices, int number_of_zeros) -> double{
    return std::accumulate(values.begin(), values.end(), 0.0);
  });
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
mat <- make_matrix_with_all_features(nrow=10, ncol=6)
sp_mat <- as(mat, "dgCMatrix")
colSums2(sp_mat)
*/
