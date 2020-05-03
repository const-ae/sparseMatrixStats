#ifndef my_utils_h
#define my_utils_h

#include <Rcpp.h>
#include "types.h"

template<typename Iterator>
inline double sum_stable(Iterator iter){
    LDOUBLE sum = 0.0;
    int counter = 0;
    for(double e : iter){
        R_CHECK_USER_INTERRUPT(++counter);
        sum += e;
    }
    return sum;
}



// This function was copied from https://stackoverflow.com/a/17299623/604854
template <typename T>
std::vector<T> flatten(const std::vector<std::vector<T>>& v) {
    std::size_t total_size = 0;
    for (const auto& sub : v){
        total_size += sub.size(); // I wish there was a transform_accumulate
    }
    std::vector<T> result;
    result.reserve(total_size);
    for (const auto& sub : v)
        result.insert(result.end(), sub.begin(), sub.end());
    return result;
}


template<typename Iterator>
inline bool is_any_na(Iterator iter){
    return std::any_of(iter.begin(), iter.end(), [](const double d) -> bool {
        return Rcpp::NumericVector::is_na(d);
    });
}


template<typename Iterator>
inline bool are_all_na(Iterator iter){
    return  std::all_of(iter.begin(), iter.end(), [](const double d) -> bool {
        return Rcpp::NumericVector::is_na(d);
    });
}



#endif /* my_utils_h */
