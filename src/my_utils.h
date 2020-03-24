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


template<typename Iterator>
inline bool is_any_na(Iterator iter){
    return std::any_of(iter.begin(), iter.end(), [](const double d) -> bool {
        return Rcpp::NumericVector::is_na(d);
    });
}


#endif /* my_utils_h */
