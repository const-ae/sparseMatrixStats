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


// [[Rcpp::export]]
NumericVector dgCMatrix_colMeans2(S4 matrix, bool na_rm){
  return reduce_matrix(matrix, na_rm, [](auto values, auto row_indices, int number_of_zeros) -> double{
    double accum = 0.0;
    R_len_t size = number_of_zeros;
    std::for_each(values.begin(), values.end(),
      [&](auto d){
        ++size;
        accum += d;
      });
    if(NumericVector::is_na(accum)){
      return accum;
    }else if(size == 0){
      return R_NaN;
    }else{
      return accum / size;
    }
  });
}

// [[Rcpp::export]]
NumericVector dgCMatrix_colVars(S4 matrix, bool na_rm){
  return reduce_matrix(matrix, na_rm, [](auto values, auto row_indices, int number_of_zeros) -> double{
    double accum = 0.0;
    double accum2 = 0.0;
    R_len_t size = number_of_zeros;
    std::for_each(values.begin(), values.end(),
                  [&](auto d){
                    ++size;
                    accum += d;
                    accum2 += pow(d, 2);
                  });
    if(NumericVector::is_na(accum)){
      return accum;
    }else if(size <= 1){
      return NA_REAL;    // Yes, var(3) actually returns NA instead of NaN
    }else{
      return ((accum2) - pow(accum , 2)/ size)  / (size-1);
    }
  });
}


// [[Rcpp::export]]
NumericVector dgCMatrix_colMins(S4 matrix, bool na_rm){
  return reduce_matrix(matrix, na_rm, [](auto values, auto row_indices, int number_of_zeros) -> double{
    auto min = std::min_element(values.begin(), values.end(), [](double a, double b) -> bool {
      return a < b;
    });
    if(min == values.end()){
      return number_of_zeros > 0 ? 0.0 : R_PosInf;
    }else{
      return std::min(*min, 0.0);
    }
  });
}

// [[Rcpp::export]]
NumericVector dgCMatrix_colMaxs(S4 matrix, bool na_rm){
  return reduce_matrix(matrix, na_rm, [](auto values, auto row_indices, int number_of_zeros) -> double{
    auto max_iter = std::max_element(values.begin(), values.end(), [](double a, double b) -> bool {
      return a < b;
    });
    if(max_iter == values.end()){
      return number_of_zeros > 0 ? 0.0 : R_NegInf;
    }else{
      return std::max(*max_iter, 0.0);
    }
  });
}


// [[Rcpp::export]]
NumericVector dgCMatrix_colCounts(S4 matrix, double value, bool na_rm){
  return reduce_matrix(matrix, na_rm, [value, na_rm](auto values, auto row_indices, int number_of_zeros) -> double{
    if(na_rm && value == 0.0){
      return number_of_zeros;
    }else if(na_rm){
      return std::count(values.begin(), values.end(), value);
    }else{
      bool contains_na = std::any_of(values.begin(), values.end(), [](const double d) -> bool{
        return NumericVector::is_na(d);
      });
      if(! contains_na){  // No NA's in the vector
        if(value == 0.0){
          return number_of_zeros;
        }else{
          return std::count(values.begin(), values.end(), value);
        }
      }else{
        return NA_REAL;
      }
    }
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
