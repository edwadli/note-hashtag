#include <iostream>

#include <cmath>
#include <unordered_map>
#include <vector>

#include "stk/FileLoop.h"
#include "stk/FileWvOut.h"

using namespace stk;

// C4 when note == 1
float to_frequency(int note)
{
    note -= 10;
    return 440.0 * pow(2.0, (double)note / 12.0);
}

int main()
{
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
    
    // Set the global sample rate before creating class instances.
    Stk::setSampleRate(44100.0);
    FileLoop sine, silence;
    FileWvOut output;
    // Load the sine wave file.
    sine.openFile("/usr/local/share/stk/rawwaves/sinewave.raw", true);
    silence.openFile("/usr/local/share/stk/rawwaves/silence.raw", true);
    // Open a 16-bit, one-channel WAV formatted output file
    output.openFile("twinkle.wav", 1, FileWrite::FILE_WAV, Stk::STK_SINT16);
    
    const int note_length = 15000;
    
    for (int note : song)
    {
        sine.setFrequency(scale[note]);
        for (int i = 0; i < note_length; i++)
            output.tick(sine.tick());
        for (int i = 0; i < note_length / 10; i++)
            output.tick(silence.tick());
    }
    
    for (int i = 0; i < note_length; i++)
        output.tick(silence.tick());
    
    return 0;
}
