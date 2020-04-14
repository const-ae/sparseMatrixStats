#include <Rcpp.h>
#include "SparseMatrixView.h"
#include "ColumnView.h"
#include "VectorSubsetView.h"
#include "SkipNAVectorSubsetView.h"
#include "quantile.h"
#include "sample_rank.h"
#include "my_utils.h"

using namespace Rcpp;


template<typename Functor>
NumericMatrix reduce_matrix_num_matrix_with_na(S4 matrix, R_len_t n_res_columns, bool transpose, Functor op){
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  std::vector<std::vector<double> > result;
  result.reserve(sp_mat.ncol);
  std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                 [op](ColumnView::col_container col) -> std::vector<double> {
                   return op(col.values, col.row_indices, col.number_of_zeros);
                 });
  std::vector<double> result_flat = flatten(result);
  if(transpose){
    return Rcpp::transpose(NumericMatrix(n_res_columns, sp_mat.ncol, result_flat.begin()));
  }else{
    return NumericMatrix(n_res_columns, sp_mat.ncol, result_flat.begin());
  }
}


template<typename Functor>
IntegerMatrix reduce_matrix_int_matrix_with_na(S4 matrix, R_len_t n_res_columns, bool transpose, Functor op){
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  std::vector<std::vector<int> > result;
  result.reserve(sp_mat.ncol);
  std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                 [op](ColumnView::col_container col) -> std::vector<int> {
                   return op(col.values, col.row_indices, col.number_of_zeros);
                 });
  std::vector<int> result_flat = flatten(result);
  if(transpose){
    return Rcpp::transpose(IntegerMatrix(n_res_columns, sp_mat.ncol, result_flat.begin()));
  }else{
    return IntegerMatrix(n_res_columns, sp_mat.ncol, result_flat.begin());
  }
}


/*---------------Simple Aggregation Functions-----------------*/

template<class View>
double sparse_sum(View values){
  return std::accumulate(values.begin(), values.end(), 0.0);
}

// [[Rcpp::export]]
NumericVector dgCMatrix_colSums2(S4 matrix, bool na_rm){
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  std::vector<double> result;
  result.reserve(sp_mat.ncol);
  if(na_rm){
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [](ColumnView::col_container col) -> double {
                     SkipNAVectorSubsetView<REALSXP> values_wrapper(&col.values);
                     return sparse_sum(values_wrapper);
                   });
  }else{
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [](ColumnView::col_container col) -> double {
                     return sparse_sum(col.values);
                   });
  }
  return wrap(result);
}



template<class View>
double sparse_mean(View values, int number_of_zeros){
  double accum = 0.0;
  R_len_t size = number_of_zeros;
  std::for_each(values.begin(), values.end(),
                [&](double d){
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
}

// [[Rcpp::export]]
NumericVector dgCMatrix_colMeans2(S4 matrix, bool na_rm){
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  std::vector<double> result;
  result.reserve(sp_mat.ncol);
  if(na_rm){
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [](ColumnView::col_container col) -> double {
                     SkipNAVectorSubsetView<REALSXP> values_wrapper(&col.values);
                     return sparse_mean(values_wrapper, col.number_of_zeros);
                   });
  }else{
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [](ColumnView::col_container col) -> double {
                     return sparse_mean(col.values, col.number_of_zeros);
                   });
  }
  return wrap(result);
}


template<class View>
double sparse_median(View values, int number_of_zeros, bool na_rm){
  if(! na_rm){
    bool any_na = is_any_na(values);
    if(any_na){
      return NA_REAL;
    }
  }
  R_len_t size = values.size();
  if(number_of_zeros > size){
    // Easy escape hatch
    return 0.0;
  }
  if(size + number_of_zeros == 0){
    return NA_REAL;
  }
  return quantile_sparse(values, number_of_zeros, 0.5);
}

// [[Rcpp::export]]
NumericVector dgCMatrix_colMedians(S4 matrix, bool na_rm){
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  std::vector<double> result;
  result.reserve(sp_mat.ncol);
  if(na_rm){
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [](ColumnView::col_container col) -> double {
                     SkipNAVectorSubsetView<REALSXP> values_wrapper(&col.values);
                     return sparse_median(values_wrapper, col.number_of_zeros, true);
                   });
  }else{
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [](ColumnView::col_container col) -> double {
                     return sparse_median(col.values, col.number_of_zeros, false);
                   });
  }
  return wrap(result);
}



template<class View>
double sparse_var(View values, int number_of_zeros){
  double accum = 0.0;
  double accum2 = 0.0;
  R_len_t size = number_of_zeros;
  std::for_each(values.begin(), values.end(),
                [&](double d){
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
}

// [[Rcpp::export]]
NumericVector dgCMatrix_colVars(S4 matrix, bool na_rm){
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  std::vector<double> result;
  result.reserve(sp_mat.ncol);
  if(na_rm){
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [](ColumnView::col_container col) -> double {
                     SkipNAVectorSubsetView<REALSXP> values_wrapper(&col.values);
                     return sparse_var(values_wrapper, col.number_of_zeros);
                   });
  }else{
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [](ColumnView::col_container col) -> double {
                     return sparse_var(col.values, col.number_of_zeros);
                   });
  }
  return wrap(result);
}


template<class View, class IndexView>
double sparse_mad(View values, IndexView row_indices, int number_of_zeros, bool na_rm, double scale_factor){
  if(! na_rm){
    bool any_na = is_any_na(values);
    if(any_na){
      return NA_REAL;
    }
  }
  R_len_t size = values.size();
  if(number_of_zeros > size){
    // Easy escape hatch
    return 0.0;
  }
  if(size + number_of_zeros == 0){
    return NA_REAL;
  }
  double med = quantile_sparse(values, number_of_zeros, 0.5);
  NumericVector complete_vector(size + number_of_zeros, std::abs(med));
  auto val_it = values.begin();
  auto ind_it = row_indices.begin();
  while(val_it != values.end() && ind_it != row_indices.end()){
    complete_vector[*ind_it] = std::abs(*val_it - med);
    ++val_it;
    ++ind_it;
  }
  return median(complete_vector) * scale_factor;
}


// [[Rcpp::export]]
NumericVector dgCMatrix_colMads(S4 matrix, bool na_rm, double scale_factor){
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  std::vector<double> result;
  result.reserve(sp_mat.ncol);
  if(na_rm){
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [scale_factor](ColumnView::col_container col) -> double {
                     SkipNAVectorSubsetView<REALSXP> values_wrapper(&col.values);
                     SkipNAVectorSubsetView<INTSXP> row_indices_wrapper(&col.row_indices);
                     return sparse_mad(values_wrapper, row_indices_wrapper, col.number_of_zeros, true, scale_factor);
                   });
  }else{
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [scale_factor](ColumnView::col_container col) -> double {
                     return sparse_mad(col.values, col.row_indices, col.number_of_zeros, false, scale_factor);
                   });
  }
  return wrap(result);
}

template<class View>
double sparse_min(View values, int number_of_zeros){
  auto min = std::min_element(values.begin(), values.end(), [](double a, double b) -> bool {
    return a < b;
  });
  if(min == values.end()){
    return number_of_zeros > 0 ? 0.0 : R_PosInf;
  }else{
    return std::min(*min, 0.0);
  }
}

// [[Rcpp::export]]
NumericVector dgCMatrix_colMins(S4 matrix, bool na_rm){
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  std::vector<double> result;
  result.reserve(sp_mat.ncol);
  if(na_rm){
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [](ColumnView::col_container col) -> double {
                     SkipNAVectorSubsetView<REALSXP> values_wrapper(&col.values);
                     return sparse_min(values_wrapper, col.number_of_zeros);
                   });
  }else{
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [](ColumnView::col_container col) -> double {
                     return sparse_min(col.values, col.number_of_zeros);
                   });
  }
  return wrap(result);
}

template<class View>
double sparse_max(View values, int number_of_zeros){
  auto max_iter = std::max_element(values.begin(), values.end(), [](double a, double b) -> bool {
    return a < b;
  });
  if(max_iter == values.end()){
    return number_of_zeros > 0 ? 0.0 : R_NegInf;
  }else{
    return std::max(*max_iter, 0.0);
  }
}

// [[Rcpp::export]]
NumericVector dgCMatrix_colMaxs(S4 matrix, bool na_rm){
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  std::vector<double> result;
  result.reserve(sp_mat.ncol);
  if(na_rm){
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [](ColumnView::col_container col) -> double {
                     SkipNAVectorSubsetView<REALSXP> values_wrapper(&col.values);
                     return sparse_max(values_wrapper, col.number_of_zeros);
                   });
  }else{
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [](ColumnView::col_container col) -> double {
                     return sparse_max(col.values, col.number_of_zeros);
                   });
  }
  return wrap(result);
}


template<class View>
double sparse_orderStats(View values, int number_of_zeros, bool na_rm, int which){
  if(! na_rm){
    bool any_na = is_any_na(values);
    if(any_na){
      return NA_REAL;
    }
  }
  R_len_t size = values.size();
  double used_which = std::min(which, size + number_of_zeros);
  if(used_which == 0){
    return NA_REAL;
  }else if(size == 0){
    return 0.0;
  }
  std::vector<double> sorted_values;
  std::copy(values.begin(), values.end(), std::back_inserter(sorted_values));
  std::sort(sorted_values.begin(), sorted_values.end(),[](double i1, double i2){
    if(Rcpp::NumericVector::is_na(i1)) return false;
    if(Rcpp::NumericVector::is_na(i2)) return true;
    return i1 < i2;
  });
  bool left_of_zero = sorted_values[0] < 0;
  bool right_of_zero = !left_of_zero && number_of_zeros == 0;
  int zero_counter = ! left_of_zero && ! right_of_zero;
  int vec_counter = 0;
  for(int i = 0; i < sorted_values.size() + number_of_zeros; i++){
    // Rcout << i << " " << vec_counter << " " << zero_counter << " " << left_of_zero << " " << right_of_zero << " " << std::endl;
    if(i == used_which - 1){
      if(! left_of_zero && ! right_of_zero){
        return 0;
      }else{
        return sorted_values[vec_counter];
      }
    }

    if(left_of_zero){
      vec_counter++;
      if(vec_counter == size || sorted_values[vec_counter] > 0){
        left_of_zero = false;
      }
    }
    if(right_of_zero){
      vec_counter++;
    }
    if(! left_of_zero && ! right_of_zero){
      zero_counter++;
      if(zero_counter > number_of_zeros){
        right_of_zero = true;
      }
    }
  }
  return NA_REAL;
}

// [[Rcpp::export]]
NumericVector dgCMatrix_colOrderStats(S4 matrix, int which, bool na_rm){
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  std::vector<double> result;
  result.reserve(sp_mat.ncol);
  if(na_rm){
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [which](ColumnView::col_container col) -> double {
                     SkipNAVectorSubsetView<REALSXP> values_wrapper(&col.values);
                     return sparse_orderStats(values_wrapper, col.number_of_zeros, true, which);
                   });
  }else{
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [which](ColumnView::col_container col) -> double {
                     return sparse_orderStats(col.values, col.number_of_zeros, false, which);
                   });
  }
  return wrap(result);
}


template<class View>
double sparse_logSumExp(View values, int number_of_zeros){
  auto max_iter = std::max_element(values.begin(), values.end(), [](double a, double b) -> bool {
    return a < b;
  });
  if(max_iter == values.end()){
    return number_of_zeros > 0 ? log(number_of_zeros) : R_NegInf;
  }else{
    double max = *max_iter;
    if(NumericVector::is_na(max)){
      return max;
    }
    if(max == R_PosInf){
      return R_PosInf;
    }
    double sum = std::accumulate(values.begin(), values.end(), 0.0, [max](double a, double b) -> double {
      return a + exp(b-max);
    });
    sum += exp(-max) * number_of_zeros;
    return max + log(sum);
  }
}

// [[Rcpp::export]]
NumericVector dgCMatrix_colLogSumExps(S4 matrix, bool na_rm){
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  std::vector<double> result;
  result.reserve(sp_mat.ncol);
  if(na_rm){
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [](ColumnView::col_container col) -> double {
                     SkipNAVectorSubsetView<REALSXP> values_wrapper(&col.values);
                     return sparse_logSumExp(values_wrapper, col.number_of_zeros);
                   });
  }else{
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [](ColumnView::col_container col) -> double {
                     return sparse_logSumExp(col.values, col.number_of_zeros);
                   });
  }
  return wrap(result);
}

template<class View>
double sparse_prod(View values, int number_of_zeros, bool na_rm){
  bool any_inf = std::any_of(values.begin(), values.end(), [](const double d) -> bool {
    return d == R_PosInf || d == R_NegInf;
  });
  if(na_rm){
    if(number_of_zeros > 0 && !any_inf){
      return 0.0;
    }else if(number_of_zeros > 0 && any_inf){
      return R_NaN;
    }else{
      return std::accumulate(values.begin(), values.end(), 1.0, [](double a, double b) -> double { return a * b;});
    }
  }else{
    bool any_na = is_any_na(values);
    if(any_na){
      return NA_REAL;
    }else{
      if(number_of_zeros > 0 && !any_inf){
        return 0.0;
      }else if(number_of_zeros > 0 && any_inf){
        return R_NaN;
      }else{
        return std::accumulate(values.begin(), values.end(), 1.0, [](double a, double b) { return a * b; });
      }
    }
  }
}

// [[Rcpp::export]]
NumericVector dgCMatrix_colProds(S4 matrix, bool na_rm){
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  std::vector<double> result;
  result.reserve(sp_mat.ncol);
  if(na_rm){
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [](ColumnView::col_container col) -> double {
                     SkipNAVectorSubsetView<REALSXP> values_wrapper(&col.values);
                     return sparse_prod(values_wrapper, col.number_of_zeros, true);
                   });
  }else{
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [](ColumnView::col_container col) -> double {
                     return sparse_prod(col.values, col.number_of_zeros, false);
                   });
  }
  return wrap(result);

}



/*---------------Weighted Aggregation Functions---------------*/


template<class View, class IndexView>
double sparse_weightedMean(View values, IndexView row_indices, int number_of_zeros, bool na_rm, NumericVector weights, double total_weights){
  double accum = 0.0;
  double remaining_weights = total_weights;
  auto val_it = values.begin();
  auto ind_it = row_indices.begin();
  while(val_it != values.end() && ind_it != row_indices.end()){
    double v = *val_it;
    if(NumericVector::is_na(v)){
      if(! na_rm){
        return NA_REAL;
      }
      remaining_weights -=  weights[*ind_it];
    }else{
      accum += v * weights[*ind_it];
    }
    ++val_it;
    ++ind_it;
  }
  if(NumericVector::is_na(accum)){
    return accum;
  }else if(remaining_weights < pow(10, -9)){
    return R_NaN;
  }else{
    return accum / remaining_weights;
  }
}

// [[Rcpp::export]]
NumericVector dgCMatrix_colWeightedMeans(S4 matrix, NumericVector weights, bool na_rm){
  double total_weights = sum(weights);

  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  std::vector<double> result;
  result.reserve(sp_mat.ncol);
  if(na_rm){
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [weights, total_weights](ColumnView::col_container col) -> double {
                     return sparse_weightedMean(col.values, col.row_indices, col.number_of_zeros, true, weights, total_weights);
                   });
  }else{
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [weights, total_weights](ColumnView::col_container col) -> double {
                     return sparse_weightedMean(col.values, col.row_indices, col.number_of_zeros, false, weights, total_weights);
                   });
  }
  return wrap(result);
}


template<class View, class IndexView>
double sparse_weightedVar(View values, IndexView row_indices, int number_of_zeros, bool na_rm, NumericVector weights, double total_weights){
  double accum = 0.0;
  double accum2 = 0.0;
  double remaining_weights = total_weights;
  auto val_it = values.begin();
  auto ind_it = row_indices.begin();
  while(val_it != values.end() && ind_it != row_indices.end()){
    double v = *val_it;
    double w = weights[*ind_it];
    if(NumericVector::is_na(v)){
      if(! na_rm){
        return NA_REAL;
      }
      remaining_weights -= w;
    }else{
      if(w > 0){
        accum += v * w;
        accum2 += pow(v, 2) * w;
      }
    }
    ++val_it;
    ++ind_it;
  }
  if(NumericVector::is_na(accum)){
    return accum;
  }else if(total_weights <= 1){
    return NA_REAL;    // Yes, var(3) actually returns NA instead of NaN
  }else{
    return ((accum2) - pow(accum , 2)/ total_weights)  / (total_weights-1);
  }
}


// [[Rcpp::export]]
NumericVector dgCMatrix_colWeightedVars(S4 matrix, NumericVector weights, bool na_rm){
  double total_weights = sum(weights);

  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  std::vector<double> result;
  result.reserve(sp_mat.ncol);
  if(na_rm){
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [weights, total_weights](ColumnView::col_container col) -> double {
                     return sparse_weightedVar(col.values, col.row_indices, col.number_of_zeros, true, weights, total_weights);
                   });
  }else{
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [weights, total_weights](ColumnView::col_container col) -> double {
                     return sparse_weightedVar(col.values, col.row_indices, col.number_of_zeros, false, weights, total_weights);
                   });
  }
  return wrap(result);
}


/*---------------Simple Detect Functions-----------------*/

template<class View>
double sparse_count(View values, int number_of_zeros, bool na_rm, double value){
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
}

// [[Rcpp::export]]
IntegerVector dgCMatrix_colCounts(S4 matrix, double value, bool na_rm){
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  std::vector<double> result;
  result.reserve(sp_mat.ncol);
  if(na_rm){
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [value](ColumnView::col_container col) -> double {
                     SkipNAVectorSubsetView<REALSXP> values_wrapper(&col.values);
                     return sparse_count(values_wrapper, col.number_of_zeros, true, value);
                   });
  }else{
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [value](ColumnView::col_container col) -> double {
                     return sparse_count(col.values, col.number_of_zeros, false, value);
                   });
  }
  return wrap(result);

}

// [[Rcpp::export]]
LogicalVector dgCMatrix_colAnyNAs(S4 matrix){
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  std::vector<int> result;
  result.reserve(sp_mat.ncol);
  std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                 [](ColumnView::col_container col) -> int {
                   return is_any_na(col.values);
                 });
  return wrap(result);
}


template<class View>
double sparse_any(View values, int number_of_zeros, bool na_rm, double value){
  if(na_rm && value == 0.0){
    return number_of_zeros > 0;
  }else if(! na_rm && value == 0.0){
    if(number_of_zeros > 0){
      return true;
    }else{
      bool all_na = std::all_of(values.begin(), values.end(), [](const double d) -> bool {
        return NumericVector::is_na(d);
      });
      if(all_na && !values.is_empty()){
        return NA_LOGICAL;
      }else{
        return false;
      }
    }
  }else if(na_rm){
    return std::any_of(values.begin(), values.end(),  [value](const double d) -> bool{
      return d == value;
    });
  }else{
    bool any_na = is_any_na(values);
    bool found_value = std::any_of(values.begin(), values.end(),  [value](const double d) -> bool{
      return d == value;
    });
    if(any_na){
      if(found_value){
        return true;
      }else{
        return NA_LOGICAL;
      }
    }else{
      return found_value;
    }
  }
}


// [[Rcpp::export]]
LogicalVector dgCMatrix_colAnys(S4 matrix, double value, bool na_rm){
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  std::vector<int> result;
  result.reserve(sp_mat.ncol);
  if(na_rm){
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [value](ColumnView::col_container col) -> int {
                     SkipNAVectorSubsetView<REALSXP> values_wrapper(&col.values);
                     return sparse_any(values_wrapper, col.number_of_zeros, true, value);
                   });
  }else{
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [value](ColumnView::col_container col) -> int {
                     return sparse_any(col.values, col.number_of_zeros, false, value);
                   });
  }
  return wrap(result);
}



template<class View>
double sparse_all(View values, int number_of_zeros, bool na_rm, int length, double value){
  if(value == 0.0){
    if(na_rm){
      return values.is_empty();
    }else{
      if(number_of_zeros == length){
        return true;
      }else{
        bool all_na = are_all_na(values);
        if(all_na){
          return NA_LOGICAL;
        }else{
          return false;
        }
      }
    }
  }else{
    if(number_of_zeros > 0){
      return false;
    }
    if(na_rm){
      return std::all_of(values.begin(), values.end(), [value](const double d) -> bool {
        return d == value;
      });
    }else{
      bool all_equal_or_na = std::all_of(values.begin(), values.end(), [value](const double d) -> bool {
        return d == value || NumericVector::is_na(d);
      });
      bool any_na = is_any_na(values);
      if(! all_equal_or_na){
        return false;
      }else if(all_equal_or_na && any_na){
        return NA_LOGICAL;
      }else if(all_equal_or_na && !any_na){
        return true;
      }
    }
  }
  return false;
}

// [[Rcpp::export]]
LogicalVector dgCMatrix_colAlls(S4 matrix, double value, bool na_rm){
  Rcpp::IntegerVector dim = matrix.slot("Dim");
  R_len_t nrows = dim[0];

  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  std::vector<int> result;
  result.reserve(sp_mat.ncol);
  if(na_rm){
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [value, nrows](ColumnView::col_container col) -> int {
                     SkipNAVectorSubsetView<REALSXP> values_wrapper(&col.values);
                     return sparse_all(values_wrapper, col.number_of_zeros, true, nrows, value);
                   });
  }else{
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [value, nrows](ColumnView::col_container col) -> int {
                     return sparse_all(col.values, col.number_of_zeros, false, nrows, value);
                   });
  }
  return wrap(result);
}


/*---------------Return matrix functions-----------------*/


template<class View>
std::vector<double> sparse_quantiles(View values, int number_of_zeros, bool na_rm, NumericVector probs){
  if(! na_rm){
    bool any_na = is_any_na(values);
    if(any_na){
      std::vector<double> result(probs.size(), NA_REAL);
      return result;
    }
  }
  if(values.size() + number_of_zeros == 0){
    std::vector<double> result(probs.size(), NA_REAL);
    return result;
  }
  std::vector<double> result;
  result.reserve(probs.size());
  std::transform(probs.begin(), probs.end(), back_inserter(result), [values, number_of_zeros](double prob) -> double{
    return quantile_sparse(values, number_of_zeros, prob);
  });
  return result;
}


// [[Rcpp::export]]
NumericMatrix dgCMatrix_colQuantiles(S4 matrix, NumericVector probs, bool na_rm){
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  int n_res_columns = probs.size();
  std::vector<std::vector<double> > result;
  result.reserve(sp_mat.ncol);
  if(na_rm){
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [probs](ColumnView::col_container col) -> std::vector<double> {
                     SkipNAVectorSubsetView<REALSXP> values_wrapper(&col.values);
                     SkipNAVectorSubsetView<INTSXP> row_indices_wrapper(&col.row_indices);
                     return sparse_quantiles(values_wrapper, col.number_of_zeros, true, probs);
                   });
  }else{
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [probs](ColumnView::col_container col) -> std::vector<double> {
                     return sparse_quantiles(col.values, col.number_of_zeros, false, probs);
                   });
  }
  std::vector<double> result_flat = flatten(result);
  return Rcpp::transpose(NumericMatrix(n_res_columns, sp_mat.ncol, result_flat.begin()));
}

template<class View>
std::vector<int> sparse_tabulate(View values, int number_of_zeros, std::map<double,int>& lookup_map, bool count_nas, int na_indx){
  std::vector<int> result(lookup_map.size() + count_nas, 0);
  int na_count = 0;
  for(double v: values){
    if(Rcpp::NumericVector::is_na(v)){
      ++na_count;
    }else{
      auto search = lookup_map.find(v);
      if(search != lookup_map.end()){
        result[search->second] += 1;
      }
    }
  }
  result[lookup_map.at(0)] = number_of_zeros;
  if(count_nas){
    result[na_indx] = na_count;
  }
  return result;
}

// [[Rcpp::export]]
IntegerMatrix dgCMatrix_colTabulate(S4 matrix, NumericVector sorted_unique_values){
  // std::set<int> unique_elements(values.begin(), values.end());
  // unique_elements.insert(0);
  // std::vector<int> sorted_elements;
  // std::copy(unique_elements.begin(), unique_elements.end(), std::back_inserter(sorted_elements));
  // sort(sorted_elements.begin(),sorted_elements.end(),[](int i1, int i2){
  //   if(Rcpp::IntegerVector::is_na(i1)) return false;
  //   if(Rcpp::IntegerVector::is_na(i2)) return true;
  //   return i1 < i2;
  // });
  std::map<double,int> lookup_map;
  bool count_nas = false;
  int na_indx = -1;
  for(int i = 0; i < sorted_unique_values.size(); ++i){
    double value = sorted_unique_values[i];
    if(Rcpp::NumericVector::is_na(value)){
      count_nas = true;
      na_indx = i;
    }else{
      lookup_map[value] = i;
    }
  }
  int n_res_columns = lookup_map.size() + count_nas;
  // return reduce_matrix_int_matrix_with_na(matrix, lookup_map.size() + count_nas, true,
  //     [&lookup_map, count_nas, na_indx](auto values, auto row_indices, int number_of_zeros) -> std::vector<int> {
  //
  // });
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  std::vector<std::vector<int> > result;
  result.reserve(sp_mat.ncol);
  std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                 [&lookup_map, count_nas, na_indx](ColumnView::col_container col) -> std::vector<int> {
                   return sparse_tabulate(col.values, col.number_of_zeros, lookup_map, count_nas, na_indx);
                 });
  std::vector<int> result_flat = flatten(result);
  return Rcpp::transpose(IntegerMatrix(n_res_columns, sp_mat.ncol, result_flat.begin()));
}



/*---------------Cumulative functions-----------------*/

template<class View, class IndexView>
std::vector<double> sparse_cumsum(View values, IndexView row_indices, int number_of_zeros, int length){
  std::vector<double> result(length);
  double acc = 0;
  auto row_it = row_indices.begin();
  auto val_it = values.begin();
  auto res_it = result.begin();
  for(int i = 0; i < length; ++i, ++res_it){
    if(row_it != row_indices.end() && i == *row_it){
      acc += *val_it;
      ++row_it;
      ++val_it;
    }
    *res_it = acc;
  }
  return result;
}

// [[Rcpp::export]]
NumericMatrix dgCMatrix_colCumsums(S4 matrix){
  Rcpp::IntegerVector dim = matrix.slot("Dim");
  R_len_t nrows = dim[0];
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  int n_res_columns = nrows;
  std::vector<std::vector<double> > result;
  result.reserve(sp_mat.ncol);
  std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                 [nrows](ColumnView::col_container col) -> std::vector<double> {
                   return sparse_cumsum(col.values, col.row_indices, col.number_of_zeros, nrows);
                 });
  std::vector<double> result_flat = flatten(result);
  return NumericMatrix(n_res_columns, sp_mat.ncol, result_flat.begin());
}


template<class View, class IndexView>
std::vector<double> sparse_cumprod(View values, IndexView row_indices, int number_of_zeros, int length){
  std::vector<double> result(length);
  double acc = 1;
  auto row_it = row_indices.begin();
  auto val_it = values.begin();
  auto res_it = result.begin();
  for(int i = 0; i < length; ++i, ++res_it){
    if(row_it != row_indices.end() && i == *row_it){
      acc *= *val_it;
      ++row_it;
      ++val_it;
    }else{
      acc = 0 * acc;
    }
    *res_it = acc;
  }
  return result;
}


// [[Rcpp::export]]
NumericMatrix dgCMatrix_colCumprods(S4 matrix){
  Rcpp::IntegerVector dim = matrix.slot("Dim");
  R_len_t nrows = dim[0];
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  int n_res_columns = nrows;
  std::vector<std::vector<double> > result;
  result.reserve(sp_mat.ncol);
  std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                 [nrows](ColumnView::col_container col) -> std::vector<double> {
                   return sparse_cumprod(col.values, col.row_indices, col.number_of_zeros, nrows);
                 });
  std::vector<double> result_flat = flatten(result);
  return NumericMatrix(n_res_columns, sp_mat.ncol, result_flat.begin());
}



template<class View, class IndexView>
std::vector<double> sparse_cummin(View values, IndexView row_indices, int number_of_zeros, int length){
  std::vector<double> result(length);
  if(length == 0){
    // Without this escape hatch, the following code would segfault
    return result;
  }
  auto row_it = row_indices.begin();
  auto val_it = values.begin();
  auto res_it = result.begin();
  int i = 0;
  double acc = 0.0;
  if(row_it != row_indices.end() && i == *row_it){
    acc = *val_it;
    ++row_it;
    ++val_it;
  }else{
    acc = 0.0;
  }
  *res_it = acc;
  ++res_it;
  for(i = 1; i < length; ++i, ++res_it){
    if(NumericVector::is_na(acc)){
      // Do nothing it will always stay NA
    }else if(row_it != row_indices.end() && i == *row_it){
      acc = std::min(*val_it, acc);
      ++row_it;
      ++val_it;
    }else{
      acc = std::min(0.0, acc);
    }
    *res_it = acc;
  }
  return result;
}


// [[Rcpp::export]]
NumericMatrix dgCMatrix_colCummins(S4 matrix){
  Rcpp::IntegerVector dim = matrix.slot("Dim");
  R_len_t nrows = dim[0];
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  int n_res_columns = nrows;
  std::vector<std::vector<double> > result;
  result.reserve(sp_mat.ncol);
  std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                 [nrows](ColumnView::col_container col) -> std::vector<double> {
                   return sparse_cummin(col.values, col.row_indices, col.number_of_zeros, nrows);
                 });
  std::vector<double> result_flat = flatten(result);
  return NumericMatrix(n_res_columns, sp_mat.ncol, result_flat.begin());
}


template<class View, class IndexView>
std::vector<double> sparse_cummax(View values, IndexView row_indices, int number_of_zeros, int length){
  std::vector<double> result(length);
  if(length == 0){
    // Without this escape hatch, the following code would segfault
    return result;
  }
  auto row_it = row_indices.begin();
  auto val_it = values.begin();
  auto res_it = result.begin();
  int i = 0;
  double acc = 0.0;
  if(row_it != row_indices.end() && i == *row_it){
    acc = *val_it;
    ++row_it;
    ++val_it;
  }else{
    acc = 0.0;
  }
  *res_it = acc;
  ++res_it;
  for(i = 1; i < length; ++i, ++res_it){
    if(NumericVector::is_na(acc)){
      // Do nothing it will always stay NA
    }else if(row_it != row_indices.end() && i == *row_it){
      acc = std::max(*val_it, acc);
      ++row_it;
      ++val_it;
    }else{
      acc = std::max(0.0, acc);
    }
    *res_it = acc;
  }
  return result;
}


// [[Rcpp::export]]
NumericMatrix dgCMatrix_colCummaxs(S4 matrix){
  Rcpp::IntegerVector dim = matrix.slot("Dim");
  R_len_t nrows = dim[0];
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  int n_res_columns = nrows;
  std::vector<std::vector<double> > result;
  result.reserve(sp_mat.ncol);
  std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                 [nrows](ColumnView::col_container col) -> std::vector<double> {
                   return sparse_cummax(col.values, col.row_indices, col.number_of_zeros, nrows);
                 });
  std::vector<double> result_flat = flatten(result);
  return NumericMatrix(n_res_columns, sp_mat.ncol, result_flat.begin());
}


/*------------------Ranking function------------------*/

// [[Rcpp::export]]
NumericMatrix dgCMatrix_colRanks_num(S4 matrix, std::string ties_method, std::string na_handling, bool preserve_shape){
  Rcpp::IntegerVector dim = matrix.slot("Dim");
  R_len_t nrows = dim[0];
  return reduce_matrix_num_matrix_with_na(matrix, nrows, !preserve_shape,
      [na_handling, ties_method](VectorSubsetView<REALSXP> values, VectorSubsetView<INTSXP> row_indices, int number_of_zeros) -> std::vector<double>{
    return calculate_sparse_rank<double>(values, row_indices, number_of_zeros, ties_method, na_handling);
  });
}


// [[Rcpp::export]]
IntegerMatrix dgCMatrix_colRanks_int(S4 matrix, std::string ties_method, std::string na_handling, bool preserve_shape){
  Rcpp::IntegerVector dim = matrix.slot("Dim");
  R_len_t nrows = dim[0];
  return reduce_matrix_int_matrix_with_na(matrix, nrows, !preserve_shape,
    [na_handling, ties_method](VectorSubsetView<REALSXP> values, VectorSubsetView<INTSXP> row_indices, int number_of_zeros) -> std::vector<int>{
      return calculate_sparse_rank<int>(values, row_indices, number_of_zeros, ties_method, na_handling);
  });
}





