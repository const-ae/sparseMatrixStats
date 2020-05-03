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
NumericVector reduce_matrix_double(S4 matrix, bool na_rm, Functor op){
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

template<typename Functor>
IntegerVector reduce_matrix_int(S4 matrix, bool na_rm, Functor op){
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  std::vector<int> result;
  result.reserve(sp_mat.ncol);
  if(na_rm){
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [op](ColumnView::col_container col) -> int {
                     SkipNAVectorSubsetView<REALSXP> values_wrapper(&col.values);
                     SkipNAVectorSubsetView<INTSXP> row_indices_wrapper(&col.row_indices);
                     return op(values_wrapper, row_indices_wrapper, col.number_of_zeros);
                   });
  }else{
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [op](ColumnView::col_container col) -> int {
                     return op(col.values, col.row_indices, col.number_of_zeros);
                   });
  }
  return wrap(result);
}

template<typename Functor>
LogicalVector reduce_matrix_lgl(S4 matrix, bool na_rm, Functor op){
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  std::vector<int> result;
  result.reserve(sp_mat.ncol);
  if(na_rm){
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [op](ColumnView::col_container col) -> int {
                     SkipNAVectorSubsetView<REALSXP> values_wrapper(&col.values);
                     SkipNAVectorSubsetView<INTSXP> row_indices_wrapper(&col.row_indices);
                     return op(values_wrapper, row_indices_wrapper, col.number_of_zeros);
                   });
  }else{
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [op](ColumnView::col_container col) -> int {
                     return op(col.values, col.row_indices, col.number_of_zeros);
                   });
  }
  return wrap(result);
}


template<typename Functor>
NumericMatrix reduce_matrix_num_matrix(S4 matrix, bool na_rm, R_len_t n_res_columns, bool transpose, Functor op){
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  std::vector<std::vector<double> > result;
  result.reserve(sp_mat.ncol);
  if(na_rm){
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [op](ColumnView::col_container col) -> std::vector<double> {
                     SkipNAVectorSubsetView<REALSXP> values_wrapper(&col.values);
                     SkipNAVectorSubsetView<INTSXP> row_indices_wrapper(&col.row_indices);
                     return op(values_wrapper, row_indices_wrapper, col.number_of_zeros);
                   });
  }else{
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [op](ColumnView::col_container col) -> std::vector<double> {
                     return op(col.values, col.row_indices, col.number_of_zeros);
                   });
  }
  std::vector<double> result_flat = flatten(result);
  if(transpose){
    return Rcpp::transpose(NumericMatrix(n_res_columns, sp_mat.ncol, result_flat.begin()));
  }else{
    return NumericMatrix(n_res_columns, sp_mat.ncol, result_flat.begin());
  }
}

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
IntegerMatrix reduce_matrix_int_matrix(S4 matrix, bool na_rm, R_len_t n_res_columns, bool transpose, Functor op){
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  std::vector<std::vector<int> > result;
  result.reserve(sp_mat.ncol);
  if(na_rm){
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [op](ColumnView::col_container col) -> std::vector<int> {
                     SkipNAVectorSubsetView<REALSXP> values_wrapper(&col.values);
                     SkipNAVectorSubsetView<INTSXP> row_indices_wrapper(&col.row_indices);
                     return op(values_wrapper, row_indices_wrapper, col.number_of_zeros);
                   });
  }else{
    std::transform(cv.begin(), cv.end(), std::back_inserter(result),
                   [op](ColumnView::col_container col) -> std::vector<int> {
                     return op(col.values, col.row_indices, col.number_of_zeros);
                   });
  }
  std::vector<int> result_flat = flatten(result);
  if(transpose){
    return Rcpp::transpose(IntegerMatrix(n_res_columns, sp_mat.ncol, result_flat.begin()));
  }else{
    return IntegerMatrix(n_res_columns, sp_mat.ncol, result_flat.begin());
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

// [[Rcpp::export]]
NumericVector dgCMatrix_colSums2(S4 matrix, bool na_rm){
  return reduce_matrix_double(matrix, na_rm, [](auto values, auto row_indices, int number_of_zeros) -> double{
    return sum_stable(values);
  });
}


template<typename Iterator>
inline double sp_mean(Iterator values, int number_of_zeros){
  LDOUBLE sum = 0.0;
  int size = number_of_zeros;
  for(double d : values){
    R_CHECK_USER_INTERRUPT(++size);
    sum += d;
  }
  if(NumericVector::is_na(sum)){
    return sum;
  }else if(size == 0){
    return R_NaN;
  }else{
    return sum / size;
  }
}

// [[Rcpp::export]]
NumericVector dgCMatrix_colMeans2(S4 matrix, bool na_rm){
  return reduce_matrix_double(matrix, na_rm, [](auto values, auto row_indices, int number_of_zeros) -> double{
    return sp_mean(values, number_of_zeros);
  });
}


// [[Rcpp::export]]
NumericVector dgCMatrix_colMedians(S4 matrix, bool na_rm){
  return reduce_matrix_double(matrix, na_rm, [na_rm](auto values, auto row_indices, int number_of_zeros) -> double{
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
  });
}


// [[Rcpp::export]]
NumericVector dgCMatrix_colVars(S4 matrix, bool na_rm){
  return reduce_matrix_double(matrix, na_rm, [](auto values, auto row_indices, int number_of_zeros) -> double{
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
NumericVector dgCMatrix_colMads(S4 matrix, bool na_rm, double scale_factor){
  return reduce_matrix_double(matrix, na_rm, [na_rm, scale_factor](auto values, auto row_indices, int number_of_zeros) -> double{
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
  });
}



// [[Rcpp::export]]
NumericVector dgCMatrix_colMins(S4 matrix, bool na_rm){
  return reduce_matrix_double(matrix, na_rm, [](auto values, auto row_indices, int number_of_zeros) -> double{
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
  return reduce_matrix_double(matrix, na_rm, [](auto values, auto row_indices, int number_of_zeros) -> double{
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
NumericVector dgCMatrix_colOrderStats(S4 matrix, int which, bool na_rm){
  return reduce_matrix_double(matrix, na_rm, [na_rm, which](auto values, auto row_indices, int number_of_zeros) -> double{
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
  });
}


// [[Rcpp::export]]
NumericVector dgCMatrix_colLogSumExps(S4 matrix, bool na_rm){
  return reduce_matrix_double(matrix, na_rm, [](auto values, auto row_indices, int number_of_zeros) -> double{
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
  });
}


// [[Rcpp::export]]
NumericVector dgCMatrix_colProds(S4 matrix, bool na_rm){
  return reduce_matrix_double(matrix, na_rm, [na_rm](auto values, auto row_indices, int number_of_zeros) -> double {
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
  });
}



/*---------------Weighted Aggregation Functions---------------*/

// [[Rcpp::export]]
NumericVector dgCMatrix_colWeightedMeans(S4 matrix, NumericVector weights, bool na_rm){
  double total_weights = sum(weights);
  return reduce_matrix_double(matrix, false, [weights, total_weights, na_rm](auto values, auto row_indices, int number_of_zeros) -> double{
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
  });
}


// [[Rcpp::export]]
NumericVector dgCMatrix_colWeightedVars(S4 matrix, NumericVector weights, bool na_rm){
  double total_weights = sum(weights);
  return reduce_matrix_double(matrix, false, [weights, total_weights, na_rm](auto values, auto row_indices, int number_of_zeros) -> double{
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
  });
}


/*---------------Simple Detect Functions-----------------*/

// [[Rcpp::export]]
IntegerVector dgCMatrix_colCounts(S4 matrix, double value, bool na_rm){
  return reduce_matrix_int(matrix, na_rm, [value, na_rm](auto values, auto row_indices, int number_of_zeros) -> int{
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

// [[Rcpp::export]]
LogicalVector dgCMatrix_colAnyNAs(S4 matrix){
  return reduce_matrix_lgl(matrix, false, [](auto values, auto row_indices, int number_of_zeros) -> int{
    return is_any_na(values);
  });
}


// [[Rcpp::export]]
LogicalVector dgCMatrix_colAnys(S4 matrix, double value, bool na_rm){
  return reduce_matrix_lgl(matrix, na_rm, [value, na_rm](auto values, auto row_indices, int number_of_zeros) -> int{
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
  });
}



// [[Rcpp::export]]
LogicalVector dgCMatrix_colAlls(S4 matrix, double value, bool na_rm){
  Rcpp::IntegerVector dim = matrix.slot("Dim");
  R_len_t nrows = dim[0];
  return reduce_matrix_lgl(matrix, na_rm, [value, na_rm, nrows](auto values, auto row_indices, int number_of_zeros) -> int{
    if(value == 0.0){
      if(na_rm){
        return values.is_empty();
      }else{
        if(number_of_zeros == nrows){
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
  });
}


/*---------------Return matrix functions-----------------*/


// [[Rcpp::export]]
NumericMatrix dgCMatrix_colQuantiles(S4 matrix, NumericVector probs, bool na_rm){
  return reduce_matrix_num_matrix(matrix, na_rm, probs.size(), true, [na_rm, probs](auto values, auto row_indices, int number_of_zeros) -> std::vector<double> {
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
  });
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
  return reduce_matrix_int_matrix_with_na(matrix, lookup_map.size() + count_nas, true,
      [&lookup_map, count_nas, na_indx](auto values, auto row_indices, int number_of_zeros) -> std::vector<int> {
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
  });
}



/*---------------Cumulative functions-----------------*/


// [[Rcpp::export]]
NumericMatrix dgCMatrix_colCumsums(S4 matrix){
  Rcpp::IntegerVector dim = matrix.slot("Dim");
  R_len_t nrows = dim[0];
  return reduce_matrix_num_matrix_with_na(matrix, nrows, false, [nrows](auto values, auto row_indices, int number_of_zeros) -> std::vector<double>{
    std::vector<double> result(nrows);
    double acc = 0;
    auto row_it = row_indices.begin();
    auto val_it = values.begin();
    auto res_it = result.begin();
    for(int i = 0; i < nrows; ++i, ++res_it){
      if(row_it != row_indices.end() && i == *row_it){
        acc += *val_it;
        ++row_it;
        ++val_it;
      }
      *res_it = acc;
    }
    return result;
  });
}



// [[Rcpp::export]]
NumericMatrix dgCMatrix_colCumprods(S4 matrix){
  Rcpp::IntegerVector dim = matrix.slot("Dim");
  R_len_t nrows = dim[0];
  return reduce_matrix_num_matrix_with_na(matrix, nrows, false, [nrows](auto values, auto row_indices, int number_of_zeros) -> std::vector<double>{
    std::vector<double> result(nrows);
    double acc = 1;
    auto row_it = row_indices.begin();
    auto val_it = values.begin();
    auto res_it = result.begin();
    for(int i = 0; i < nrows; ++i, ++res_it){
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
  });
}


// [[Rcpp::export]]
NumericMatrix dgCMatrix_colCummins(S4 matrix){
  Rcpp::IntegerVector dim = matrix.slot("Dim");
  R_len_t nrows = dim[0];
  return reduce_matrix_num_matrix_with_na(matrix, nrows, false, [nrows](auto values, auto row_indices, int number_of_zeros) -> std::vector<double>{
    std::vector<double> result(nrows);
    if(nrows == 0){
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
    for(i = 1; i < nrows; ++i, ++res_it){
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
  });
}


// [[Rcpp::export]]
NumericMatrix dgCMatrix_colCummaxs(S4 matrix){
  Rcpp::IntegerVector dim = matrix.slot("Dim");
  R_len_t nrows = dim[0];
  return reduce_matrix_num_matrix_with_na(matrix, nrows, false, [nrows](auto values, auto row_indices, int number_of_zeros) -> std::vector<double>{
    std::vector<double> result(nrows);
    if(nrows == 0){
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
    for(i = 1; i < nrows; ++i, ++res_it){
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
  });
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





