#include <Rcpp.h>
#include "VectorSubsetView.hpp"
#include "SkipNAVectorSubsetView.hpp"
#include "ColumnView.hpp"
#include "SparseMatrixView.hpp"
#include <algorithm>
#include <vector>

using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


// [[Rcpp::export]]
double subset_sum(NumericVector v, R_len_t start, R_len_t end){
  auto vsv = VectorSubsetView<REALSXP>(v, start, end);
  int s = vsv.size;
  for(double e: vsv){
    Rcout << e << std::endl;
  }
  return std::accumulate(vsv.begin(), vsv.end(), 0.0);
}

// [[Rcpp::export]]
double subset_sum_without_na(NumericVector v, R_len_t start, R_len_t end){
  auto vsv = VectorSubsetView<REALSXP>(v, start, end);
  auto vsv_wrapper = SkipNAVectorSubsetView<REALSXP>(&vsv);
  for(double e: vsv_wrapper){
    Rcout << e << std::endl;
  }
  return std::accumulate(vsv_wrapper.begin(), vsv_wrapper.end(), 0.0);
}

// [[Rcpp::export]]
NumericVector matrix_subset_sum(Rcpp::S4 matrix, R_len_t start, R_len_t end){
  dgCMatrixView sp_mat = wrap_dgCMatrix(matrix);
  ColumnView cv(&sp_mat);
  std::vector<double> result;
  result.reserve(sp_mat.ncol);
  std::transform(cv.begin(), cv.end(), std::back_inserter(result),
    [](ColumnView::col_container col) -> double {
      return std::accumulate(col.values.begin(), col.values.end(), 0.0);
    });
  return wrap(result);
}



// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
# vec <- 1:10
# vec[4] <- NA
# subset_sum(vec, 2, 6)
# subset_sum_without_na(vec, 2, 6)
# subset_sum_without_na(vec, 3, 6)
# vec[5:6] <- NA
# vec
# subset_sum_without_na(vec, 3, 6)
library(Matrix)
source("tests/testthat/setup.R")
mat <- make_matrix_with_all_features(nrow=10, ncol=6)
sp_mat <- as(mat, "dgCMatrix")
matrix_subset_sum(sp_mat, 1, 6)
colSums2(sp_mat, na.rm = FALSE)
colSums2(sp_mat, na.rm = TRUE)

large_mat <- make_matrix(nrow=1e4, ncol=1e4, frac_zero = 0.99)
large_sp_mat <- as(large_mat, "dgCMatrix")

tmp <- matrixStats::colSums2(large_mat)
tmp2 <- matrix_subset_sum(large_sp_mat, 0, 0)
tmp3 <- Matrix::colSums(large_sp_mat)

bench::mark(
  matrixStats::colSums2(large_mat),
  colSums2(large_sp_mat),
  Matrix::colSums(large_sp_mat)
)

large_mat_with_na <- large_mat
large_mat_with_na[sample(seq_len(prod(dim(large_mat))), 1000)] <- NA
sp_large_mat_with_na <- as(large_mat_with_na, "dgCMatrix")

bench::mark(
  matrixStats::colSums2(large_mat_with_na, na.rm=TRUE),
  colSums2(sp_large_mat_with_na, na.rm=TRUE),
  Matrix::colSums(sp_large_mat_with_na, na.rm=TRUE)
)

*/
