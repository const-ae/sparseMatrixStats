#include <Rcpp.h>
#include "SparseMatrixView.hpp"
#include "VectorSubsetView.hpp"
using namespace Rcpp;

// [[Rcpp::plugins("cpp11")]]

dgCMatrixView wrap_dgCMatrix(Rcpp::S4 sp_mat){
  Rcpp::IntegerVector dim = sp_mat.slot("Dim");
  Rcpp::NumericVector values = sp_mat.slot("x");
  R_len_t nrows = dim[0];
  R_len_t ncols = dim[1];

  Rcpp::IntegerVector row_indices = sp_mat.slot("i");
  Rcpp::IntegerVector col_ptrs = sp_mat.slot("p");
  return dgCMatrixView(nrows, ncols, values, row_indices, col_ptrs);
}


// [[Rcpp::export]]
void print_matrix(Rcpp::S4 matrix){
  auto sp_mat = wrap_dgCMatrix(matrix);

  Rcout << "Hello Rcpp" << std::endl;
  Rcout << sp_mat.ncol << std::endl;
  Rcout << sp_mat.values.at(0) << std::endl;
  Rcout << &sp_mat.values.get_ref().at(0) << std::endl;
}



// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
library(Matrix)
source("../tests/testthat/setup.R")
mat <- make_matrix_with_all_features(10, 5)
mat
sp_mat <- as(mat, "dgCMatrix")
sp_mat

print_matrix(sp_mat)
*/
