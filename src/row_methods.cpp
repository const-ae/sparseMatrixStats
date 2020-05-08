#include <Rcpp.h>
#include "SparseMatrixView.h"
#include "ColumnView.h"
#include "VectorSubsetView.h"
#include "SkipNAVectorSubsetView.h"
#include "types.h"

using namespace Rcpp;



// [[Rcpp::export]]
NumericVector dgCMatrix_rowSums2(S4 matrix, bool na_rm){
  IntegerVector dim = matrix.slot("Dim");
  NumericVector values = matrix.slot("x");
  IntegerVector row_indices = matrix.slot("i");
  std::vector<LDOUBLE> result (dim[0], 0.0);

  double* val_iter = values.begin();
  int* idx_iter = row_indices.begin();
  while(val_iter != values.end() && idx_iter != row_indices.end()){
    if(na_rm && ISNA(*val_iter)){
      // Do nothing
    }else{
      result[*idx_iter] += *val_iter;
    }
    ++val_iter;
    ++idx_iter;
  }
  return wrap(result);
}



// [[Rcpp::export]]
NumericVector dgCMatrix_rowMeans2(S4 matrix, bool na_rm){
  IntegerVector dim = matrix.slot("Dim");
  NumericVector values = matrix.slot("x");
  IntegerVector row_indices = matrix.slot("i");
  std::vector<LDOUBLE> result (dim[0], 0.0);
  std::vector<int> nas_per_row (dim[0], 0);

  double* val_iter = values.begin();
  int* idx_iter = row_indices.begin();
  while(val_iter != values.end() && idx_iter != row_indices.end()){
    if(na_rm && ISNA(*val_iter)){
      nas_per_row[*idx_iter] += 1;
    }else{
      result[*idx_iter] += *val_iter;
    }
    ++val_iter;
    ++idx_iter;
  }
  auto res_iter = result.begin();
  auto na_count_iter = nas_per_row.begin();
  while(res_iter != result.end() && na_count_iter != nas_per_row.end()){
    *res_iter = *res_iter / (dim[1] - *na_count_iter);
    ++res_iter;
    ++na_count_iter;
  }
  return wrap(result);
}





// [[Rcpp::export]]
NumericVector dgCMatrix_rowVars(S4 matrix, bool na_rm){
  NumericVector means = dgCMatrix_rowMeans2(matrix, na_rm);
  IntegerVector dim = matrix.slot("Dim");
  NumericVector values = matrix.slot("x");
  IntegerVector row_indices = matrix.slot("i");
  std::vector<LDOUBLE> result (dim[0], 0.0);
  std::vector<int> nas_per_row (dim[0], 0);
  std::vector<int> zeros_per_row (dim[0], dim[1]);

  double* val_iter = values.begin();
  int* idx_iter = row_indices.begin();
  while(val_iter != values.end() && idx_iter != row_indices.end()){
    if(na_rm && ISNA(*val_iter)){
      nas_per_row[*idx_iter] += 1;
    }else{
      LDOUBLE diff = (*val_iter - means[*idx_iter]);
      result[*idx_iter] += diff * diff;
    }
    zeros_per_row[*idx_iter] -= 1;
    ++val_iter;
    ++idx_iter;
  }
  auto res_iter = result.begin();
  auto na_count_iter = nas_per_row.begin();
  auto zero_count_iter = zeros_per_row.begin();
  auto mean_iter = means.begin();
  while(res_iter != result.end() &&
        na_count_iter != nas_per_row.end() &&
        zero_count_iter != zeros_per_row.end() &&
        mean_iter != means.end()){
    if(dim[1] - *na_count_iter - 1 < 0){
      *res_iter = R_NaN;
    }else{
      *res_iter = ((*res_iter) + (*zero_count_iter) * (*mean_iter) * (*mean_iter)) / (dim[1] - *na_count_iter - 1);
    }
    ++res_iter;
    ++na_count_iter;
    ++zero_count_iter;
    ++mean_iter;
  }
  return wrap(result);
}





