#include "support.hpp"

const unit_t LIT_UNIT = 0;

std::default_random_engine nh_support::myrand(static_cast<unsigned int>(::time(0)));

unit_t nh_support::print_string(std::string output) {
  std::cout << output;
  return LIT_UNIT;
}

int64_t nh_support::int_of_float(double n) {
  return static_cast<int64_t>(n);
}

double nh_support::float_of_int(int64_t n) {
  return static_cast<double>(n);
}
