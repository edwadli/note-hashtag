#ifndef __support_hpp__
#define __support_hpp__

#include <algorithm>
#include <cmath>
#include <iostream>
#include <random>
#include <string>
#include <vector>

typedef int unit_t;
extern const unit_t LIT_UNIT;

namespace nh_support
{
  extern std::default_random_engine myrand;
  
  unit_t print_string(std::string output);
  
  int64_t int_of_float(double n);
  double float_of_int(int64_t n);
  
  template<typename T>
  T concat(T v1, T v2)
  {
    T result(v1);
    std::copy(v2.begin(), v2.end(), std::back_inserter(result));
    return result;
  }
  
  template<typename T>
  std::vector<T> shuffle(std::vector<T> v)
  {
    std::shuffle(v.begin(), v.end(), myrand);
    return v;
  }
  
  unit_t render_impl(
    std::vector<std::vector<std::vector<double>>> frequencies,
    std::vector<std::vector<double>> durations,
    std::vector<double> volumes,
    std::string filename
  );
}

#endif
