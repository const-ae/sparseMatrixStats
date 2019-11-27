#ifndef ColumnView_hpp
#define ColumnView_hpp

#include <Rcpp.h>
#include "SparseMatrixView.hpp"
#include "VectorSubsetView.hpp"


class ColumnView {
  const dgCMatrixView* matrix;

public:
  class col_container {
  public:
    VectorSubsetView<REALSXP> values;
    VectorSubsetView<INTSXP> row_indices;
    const R_len_t number_of_zeros;

    col_container(VectorSubsetView<REALSXP> values_, VectorSubsetView<INTSXP> row_indices_, R_len_t number_of_zeros_):
      values(values_), row_indices(row_indices_), number_of_zeros(number_of_zeros_) {}
  };


  class iterator {
    ColumnView* cv;
    int index;
  public:
    using iterator_category= std::input_iterator_tag;
    using value_type = col_container;
    using reference = col_container&;
    using pointer = col_container*;
    using difference_type = void;

    iterator(ColumnView* cv_): cv(cv_), index(0) {
      if(cv != nullptr && cv->matrix->ncol == 0){
        cv = nullptr;
      }
    }

    col_container operator*() const {
      int start_pos = cv->matrix->col_ptrs[index];
      int end_pos = cv->matrix->col_ptrs[index + 1];
      int number_of_zeros = cv->matrix->nrow - (end_pos - start_pos);
      VectorSubsetView<REALSXP> values(cv->matrix->values, start_pos, end_pos);
      VectorSubsetView<INTSXP> row_indices(cv->matrix->row_indices, start_pos, end_pos);

      return col_container(values, row_indices, number_of_zeros);
    }

    iterator& operator++(){ // preincrement
      ++index;
      if(index == cv->matrix->ncol)
        cv=nullptr;
      return *this;
    }

    friend bool operator==(iterator const& lhs,iterator const& rhs){
      return lhs.cv == rhs.cv;
    }
    friend bool operator!=(iterator const& lhs,iterator const& rhs){
      return !(lhs==rhs);
    }

  };

  ColumnView(dgCMatrixView* matrix_): matrix(matrix_) {}
  iterator begin() { return iterator(this); }
  iterator end() { return iterator(nullptr); }

};











#endif /* ColumnView_hpp */
