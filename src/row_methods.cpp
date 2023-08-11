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
NumericVector dgCMatrix_rowSums2_col_select(S4 matrix, bool na_rm, LogicalVector col_selector){
  IntegerVector dim = matrix.slot("Dim");
  NumericVector values = matrix.slot("x");
  IntegerVector row_indices = matrix.slot("i");
  IntegerVector pointers = matrix.slot("p");
  std::vector<LDOUBLE> result (dim[0], 0.0);

  double* val_iter = values.begin();
  int* idx_iter = row_indices.begin();
  int* pointer_iter = pointers.begin();

  int idx = 0;
  int col_idx = -1;
  while(val_iter != values.end() && idx_iter != row_indices.end()){
    while(*pointer_iter <= idx && pointer_iter != pointers.end()){
      ++pointer_iter;
      ++col_idx;
    }
    int consider = col_selector[col_idx];
    if(ISNA(consider)){
      stop("NA values in the 'cols' argument are not allowed");
    }else if(consider == 1){
      if(na_rm && ISNA(*val_iter)){
        // Do nothing
      }else{
        result[*idx_iter] += *val_iter;
      }
    }else{
      // Do nothing
    }
    ++val_iter;
    ++idx_iter;
    ++idx;
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
  auto val_end = values.end();
  auto row_end = row_indices.end();
  while(val_iter != val_end && idx_iter != row_end){
    if(na_rm && ISNA(*val_iter)){
      nas_per_row[*idx_iter] += 1;
    }else{
      result[*idx_iter] += *val_iter;
    }
    ++val_iter;
    ++idx_iter;
  }
  auto res_iter = result.begin();
  auto res_end = result.end();
  auto na_count_iter = nas_per_row.begin();
  auto na_count_end = nas_per_row.end();
  while(res_iter != res_end && na_count_iter != na_count_end){
    *res_iter = *res_iter / (dim[1] - *na_count_iter);
    ++res_iter;
    ++na_count_iter;
  }

  return wrap(result);
}





// [[Rcpp::export]]
NumericVector dgCMatrix_rowVars(S4 matrix, bool na_rm, Nullable<NumericVector> center){
  bool center_provided = center.isNotNull();
  NumericVector means(0);
  if(center_provided){
    means = Rcpp::as<NumericVector>(center.get());
  }else{
    means = dgCMatrix_rowMeans2(matrix, na_rm);
  }
  IntegerVector dim = matrix.slot("Dim");
  NumericVector values = matrix.slot("x");
  IntegerVector row_indices = matrix.slot("i");
  std::vector<LDOUBLE> result (dim[0], 0.0);
  std::vector<int> nas_per_row (dim[0], 0);
  std::vector<int> zeros_per_row (dim[0], dim[1]);

  double* val_iter = values.begin();
  auto val_end = values.end();
  int* idx_iter = row_indices.begin();
  auto idx_end = row_indices.end();
  while(val_iter != val_end && idx_iter != idx_end){
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
  auto res_end = result.end();
  auto na_count_iter = nas_per_row.begin();
  auto na_count_end = nas_per_row.end();
  auto zero_count_iter = zeros_per_row.begin();
  auto zero_count_end = zeros_per_row.end();
  auto mean_iter = means.begin();
  auto mean_end = means.end();
  while(res_iter != res_end &&
        na_count_iter != na_count_end &&
        zero_count_iter != zero_count_end &&
        mean_iter != mean_end){
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





