#ifndef SparseMatrixView_h
#define SparseMatrixView_h

#include <Rcpp.h>
using namespace Rcpp;

class dgCMatrixView {

public:
  const R_len_t nrow;
  const R_len_t ncol;
  const NumericVector values;
  const IntegerVector row_indices;
  const IntegerVector col_ptrs;

  dgCMatrixView(R_len_t nrow_, R_len_t ncol_, const NumericVector values_, const IntegerVector row_indices_, const IntegerVector col_ptrs_):
    nrow(nrow_), ncol(ncol_), values(values_), row_indices(row_indices_), col_ptrs(col_ptrs_) {}

};

dgCMatrixView wrap_dgCMatrix(Rcpp::S4 sp_mat);

#endif /* SparseMatrixView_h */
