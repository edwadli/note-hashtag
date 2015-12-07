#ifndef __support_hpp__
#define __support_hpp__

#include <cmath>
#include <iostream>
#include <string>
#include <vector>

typedef int unit_t;
extern const unit_t LIT_UNIT;

namespace nh_support
{
  unit_t print_string(std::string output);
  
  int64_t int_of_float(double n);
  double float_of_int(int64_t n);
  
  template<typename T>
  std::vector<T> concat(std::vector<T> v1, std::vector<T> v2)
  {
    std::vector<T> result(v1);
    std::copy(v2.begin(), v2.end(), std::back_inserter(result));
    return result;
  }
}

#endif
