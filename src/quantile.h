#ifndef quantile_h
#define quantile_h


#include <Rcpp.h>
#include "VectorSubsetView.h"
#include <cmath>

using namespace Rcpp;

// ATTENTION: This method assumes that NA's have already been handled!
template<typename T>
double quantile_sparse(T values, int number_of_zeros, double prob){
  if(prob < 0 || prob > 1){
    throw std::range_error("prob must be between 0 and 1");
  }
  R_len_t size = values.size();
  int total_size = size + number_of_zeros;
  if(total_size == 0){
    return NA_REAL;
  }else if(size == 0){
    return 0.0;
  }
  double pivot = (total_size-1) * prob;
  // Rcout << "total_size: " << total_size << " pivot: " << pivot << std::endl;
  std::vector<double> sorted_values;
  std::copy(values.begin(), values.end(), std::back_inserter(sorted_values));
  std::sort(sorted_values.begin(), sorted_values.end());
  double left_of_pivot = NA_REAL;
  double right_of_pivot = NA_REAL;
  bool left_of_zero = sorted_values[0] < 0;
  bool right_of_zero = !left_of_zero && number_of_zeros == 0;
  int zero_counter = ! left_of_zero && ! right_of_zero;
  int vec_counter = 0;
  for(int i = 0; i < sorted_values.size() + number_of_zeros; i++){
    // Rcout << i << " " << vec_counter << " " << zero_counter << " " << left_of_zero << " " << right_of_zero << " " << std::endl;
    if(i == std::floor(pivot)){
      if(! left_of_zero && ! right_of_zero){
        left_of_pivot = 0;
      }else{
        left_of_pivot = sorted_values[vec_counter];
      }
      // Rcout << "left of pivot: " << i << " " << left_of_pivot << std::endl;
    }
    if(i == std::ceil(pivot)){
      if(! left_of_zero && ! right_of_zero){
        right_of_pivot = 0;
      }else{
        right_of_pivot = sorted_values[vec_counter];
      }
      // Rcout << "right of pivot: " << i << " " << right_of_pivot << std::endl;
      break;
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
  if(left_of_pivot == R_NegInf){
    return R_NegInf;
  }else if(right_of_pivot == R_PosInf){
    return R_PosInf;
  }else{
    return left_of_pivot + (right_of_pivot - left_of_pivot) * std::fmod(pivot, 1.0);
  }
}

// [[Rcpp::export]]
double quantile_sparse(NumericVector values, int number_of_zeros, double prob){
  VectorSubsetView<REALSXP> vsv(values, 0, values.size());
  return quantile_sparse<VectorSubsetView<REALSXP> >(vsv, number_of_zeros, prob);
}

#endif /* quantile_h */

/*** R
N <- rpois(1, lambda=2)
vec <- rnorm(N)
nz <- rpois(1, lambda=3)
compl_vec <- sort(c(vec, rep(0, nz)))
prob <- runif(1)
q1 <- quantile(compl_vec, prob)
q2 <- quantile_sparse(vec, nz, prob)
q1
q2
testthat::expect_equal(unname(q1), q2)

*/
