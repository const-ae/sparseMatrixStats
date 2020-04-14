#ifndef my_utils_h
#define my_utils_h

#include <Rcpp.h>


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


template<typename View>
inline bool is_any_na(View values){
    return std::any_of(values.begin(), values.end(), [](const double d) -> bool {
        return Rcpp::NumericVector::is_na(d);
    });
}


template<typename View>
inline bool are_all_na(View values){
    return  std::all_of(values.begin(), values.end(), [](const double d) -> bool {
        return Rcpp::NumericVector::is_na(d);
    });
}



#endif /* my_utils_h */
