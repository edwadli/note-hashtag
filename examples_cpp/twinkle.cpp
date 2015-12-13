#include <algorithm>
#include <cmath>
#include <iostream>
#include <unordered_map>
#include <vector>

#include <stk/BlowHole.h>
#include <stk/Bowed.h>
#include <stk/Rhodey.h>
#include <stk/Wurley.h>

#include "music_utils.hpp"

int main()
{
    std::vector<int> intro { 1, 1, 5, 5, 6, 6, 5 };
    std::vector<int> chorus { 4, 4, 3, 3, 2, 2, 1 };
    
    std::vector<int> bridge(chorus.size());
    std::transform(chorus.begin(), chorus.end(), bridge.begin(), [](int x) -> int { return x + 1; });
    
    std::vector<int> song;
    std::copy(intro.begin(), intro.end(), std::back_inserter(song));
    std::copy(chorus.begin(), chorus.end(), std::back_inserter(song));
    std::copy(bridge.begin(), bridge.end(), std::back_inserter(song));
    std::copy(bridge.begin(), bridge.end(), std::back_inserter(song));
    std::copy(intro.begin(), intro.end(), std::back_inserter(song));
    std::copy(chorus.begin(), chorus.end(), std::back_inserter(song));
    
    render("twinkle-blowhole.wav", song, stk::BlowHole(8.0));
    render("twinkle-bowed.wav", song, stk::Bowed());
    render("twinkle-rhodey.wav", song, stk::Rhodey());
    render("twinkle-wurley.wav", song, stk::Wurley());
    
    return 0;
}
