#ifndef VectorSubsetView_hpp
#define VectorSubsetView_hpp


#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;
// [[Rcpp::plugins("cpp11")]]


template<int RTYPE>
class VectorSubsetView {
  typedef typename Rcpp::Vector<RTYPE> RcppVector;
  RcppVector vec;
public:
  const R_len_t start;
  const R_len_t size_m;
  typedef typename RcppVector::Proxy Proxy ;
  typedef typename RcppVector::Storage stored_type;

  class postinc_return {
    Proxy value;
  public:
    postinc_return(Proxy value_): value(value_) {}
    Proxy operator*(){return value; }
  };

  class iterator {
    VectorSubsetView* vsv;
    R_len_t current;
  public:
    using iterator_category = std::input_iterator_tag;
    using value_type = stored_type;
    using reference = stored_type;
    using pointer = stored_type const * ;
    using difference_type = ptrdiff_t;


    iterator(VectorSubsetView* vsv_): vsv(vsv_), current(0) {}

    Proxy operator*() const {
      return vsv->vec[vsv->start + current];
    }

    iterator& operator++(){ //preincrement
      ++current;
      if(current == vsv->size()){
        vsv = nullptr;
      }
      return *this;
    }

    postinc_return operator++(int){
      postinc_return temp(vsv->vec[vsv->start + current]);
      ++*this;
      return temp;
    }

    friend bool operator==(iterator const& lhs,iterator const& rhs){
      return lhs.vsv == rhs.vsv;
    }
    friend bool operator!=(iterator const& lhs,iterator const& rhs){
      return !(lhs==rhs);
    }

  };

  VectorSubsetView(const RcppVector vec_, const R_len_t start_, const R_len_t end_):
    vec(vec_), start(start_), size_m(end_ - start_) {
    if(end_ < start_){
      throw std::range_error("End must not be smaller than start");
    }
    if(start_ < 0){
      throw std::range_error("Start must not be smaller than 0");
    }
    if(end_ > vec.size()){
      throw std::range_error("End must not be larger than size of vec");
    }
  }

  iterator begin() {
    if(size_m == 0){
      return iterator(nullptr);
    }else{
      return iterator(this);
    }
  }
  iterator end() { return iterator(nullptr); }

  R_len_t size() { return size_m; }

  bool is_empty(){
    return size_m == 0;
  }

  Proxy operator[](R_len_t i) {
    return vec[start + i];
  }

};




// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
vec <- 1:10
subset_sum(vec)
*/



# endif /* VectorSubsetView_hpp */
