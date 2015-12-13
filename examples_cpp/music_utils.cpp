#include "music_utils.hpp"

float to_frequency(int note)
{
    note -= 10;
    return 440.0 * pow(2.0, (double)note / 12.0);
}

std::unordered_map<int, double> scale
{
    { 1, to_frequency(1) },
    { 2, to_frequency(3) },
    { 3, to_frequency(5) },
    { 4, to_frequency(6) },
    { 5, to_frequency(8) },
    { 6, to_frequency(10) },
    { 7, to_frequency(12) },
};
