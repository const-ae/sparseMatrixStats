

#ifndef SkipNAVectorSubsetView_hpp
#define SkipNAVectorSubsetView_hpp

#include <Rcpp.h>
#include "VectorSubsetView.hpp"
using namespace Rcpp;


template<int RTYPE>
class SkipNAVectorSubsetView {
  typedef typename Rcpp::Vector<RTYPE> RcppVector;
  VectorSubsetView<RTYPE>* vsv;
public:
  typedef typename RcppVector::Proxy Proxy ;
  typedef typename RcppVector::Storage stored_type;

  class postinc_return {
    Proxy value;
  public:
    postinc_return(Proxy value_): value(value_) {}
    Proxy operator*(){return value; }
  };

  class iterator {
    typedef typename VectorSubsetView<RTYPE>::iterator vsv_iterator;
    vsv_iterator vsv_iter_start;
    vsv_iterator vsv_iter_end;
  public:
    using iterator_category = std::input_iterator_tag;
    using value_type = stored_type;
    using reference = stored_type;
    using pointer = stored_type const * ;
    using difference_type = ptrdiff_t;


    iterator(vsv_iterator vsv_iter_start_, vsv_iterator vsv_iter_end_): vsv_iter_start(vsv_iter_start_), vsv_iter_end(vsv_iter_end_) {
      while(vsv_iter_start != vsv_iter_end && RcppVector::is_na(*vsv_iter_start)){
        ++vsv_iter_start;
      }
    }


    Proxy operator*() const {
      return *(vsv_iter_start);
    }

    iterator& operator++(){ //preincrement
      ++vsv_iter_start;
      if(vsv_iter_start == vsv_iter_end){
        vsv_iter_start = nullptr;
        return *this;
      }else if(RcppVector::is_na(*vsv_iter_start)){
        return ++*this;
      }else{
        return *this;
      }
    }

    postinc_return operator++(int){
      postinc_return temp(*vsv_iter_start);
      ++*this;
      return temp;
    }

    friend bool operator==(iterator const& lhs,iterator const& rhs){
      return lhs.vsv_iter_start == rhs.vsv_iter_start;
    }
    friend bool operator!=(iterator const& lhs,iterator const& rhs){
      return !(lhs==rhs);
    }

  };

  SkipNAVectorSubsetView(VectorSubsetView<RTYPE>* vsv_): vsv(vsv_) {}

  iterator begin() {
    return iterator(vsv->begin(), vsv->end());
  }
  iterator end() {
    return iterator(nullptr, nullptr);
  }

};

// // [[Rcpp::export]]
// double subset_sum_without_na(NumericVector v, R_len_t start, R_len_t end){
//   auto vsv = VectorSubsetView<REALSXP>(v, start, end);
//   int s = vsv.size;
//   for(double e: vsv){
//     Rcout << e << std::endl;
//   }
//   return std::accumulate(vsv.begin(), vsv.end(), 0.0);
// }



#endif /* SkipNAVectorSubsetView_hpp */
