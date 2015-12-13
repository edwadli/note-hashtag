#ifndef __music_utils_hpp__
#define __music_utils_hpp__

#include <iostream>
#include <unordered_map>

#include <stk/Instrmnt.h>
#include <stk/FileLoop.h>
#include <stk/FileWvOut.h>

// C4 when note == 1
float to_frequency(int note);

template <typename TSong, typename TInstrument>
void render(std::string filename, const TSong &song, TInstrument instrument, const int note_length = 15000);

extern std::unordered_map<int, double> scale;

// Template implementations
template <typename TSong, typename TInstrument>
void render(std::string filename, const TSong &song, TInstrument instrument, const int note_length)
{
    // Set the global sample rate before creating class instances.
    stk::Stk::setSampleRate(44100.0);
    stk::Stk::setRawwavePath("../stk/rawwaves/");
    
    stk::FileLoop silence("../stk/rawwaves/silence.raw", true);
    
    // Open a 16-bit, one-channel WAV formatted output file
    stk::FileWvOut output(filename, 1, stk::FileWrite::FILE_WAV, stk::Stk::STK_SINT16);
    
    const int note_tail = note_length / 8;
    const int note_padding = 0;
    const int file_padding = 10000;
    for (int note : song)
    {
        for (int i = 0; i < note_length; i++) {
            instrument.noteOn(scale[note], 0.8);
            output.tick(instrument.tick());
        }
        for (int i = 0; i < note_tail; i++) {
            // [0.0, 1.0) -- bigger means it stops faster
            instrument.noteOff(0.5);
            output.tick(instrument.tick());
        }
        for (int i = 0; i < note_padding; i++){}
            output.tick(silence.tick());
    }

    for (int i = 0; i < file_padding; i++)
        output.tick(silence.tick());
    
    output.closeFile();
}

#endif
