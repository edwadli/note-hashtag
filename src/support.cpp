#include "support.hpp"

void nh_support::print_string(std::string output) {
  std::cout << output;
}

int64_t nh_support::int_of_float(double n) {
  return (int64_t)n;
}

double nh_support::float_of_int(int64_t n) {
  return (double)n;
}
