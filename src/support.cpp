#include "support.hpp"

#include <stk/Wurley.h>
#include <stk/Instrmnt.h>
#include <stk/FileLoop.h>
#include <stk/FileWvOut.h>

const unit_t LIT_UNIT = 0;

std::default_random_engine nh_support::myrand(static_cast<unsigned int>(::time(0)));

unit_t nh_support::print_string(std::string output)
{
  std::cout << output;
  return LIT_UNIT;
}

int64_t nh_support::int_of_float(double n)
{
  return static_cast<int64_t>(n);
}

double nh_support::float_of_int(int64_t n)
{
  return static_cast<double>(n);
}

template <typename T>
void safe_insert(std::vector<T> &v, size_t idx, T obj)
{
  while (v.size() <= idx) v.push_back(T());
  v[idx] = obj;
}

template <typename T>
T safe_at(std::vector<T> &v, size_t idx)
{
  if (v.size() <= idx) return T();
  else return v[idx];
}

unit_t nh_support::render_impl(
  std::vector<std::vector<std::vector<double>>> frequencies,
  std::vector<std::vector<double>> durations,
  std::vector<double> volumes,
  std::string filename)
{
  auto instrument_allocator = []() { return stk::Wurley(); };
  const double sample_rate = 44100.0;
  // Set the global sample rate before creating class instances.
  stk::Stk::setSampleRate(sample_rate);
  stk::Stk::setRawwavePath("../stk/rawwaves/");
  
  auto instrument = instrument_allocator();
  
  std::vector<double> samples;
  // Loop through tracks
  for (size_t i_track = 0; i_track < frequencies.size(); i_track++) {
    std::vector<std::vector<double>> track = frequencies[i_track];
    
    const double volume_scale = volumes[i_track] / volumes.size();
    size_t i = 0; // Index for writing into samples array
    // Loop through chords
    for (size_t i_chord = 0; i_chord < track.size(); i_chord++) {
      std::vector<double> chord = track[i_chord];
      
      const double samples_total = sample_rate * durations[i_track][i_chord];
      const size_t samples_sound = static_cast<size_t>(0.9 * samples_total);
      // Loop through notes
      for (double note : chord) {
        // Rests have a frequency <= 0.0
        bool is_silence = (note <= 0.0);
        if (!is_silence) instrument.noteOn(note, 1.0);
        // Offset i + calculate number of samples to output
        const size_t end = i + samples_sound;
        for (size_t j = i; j < end; j++) {
          // Need to normalize so chords are all the same volume
          auto sample = safe_at(samples, j) + (is_silence ? 0.0 : volume_scale * instrument.tick() / chord.size());
          safe_insert(samples, j, sample);
        }
      }
      // Done outputting this note, so increment our index to the output buffer
      // Since we output audio samples until samples_sound, incrementing by samples_total gives us some silence at the
      // end of each note.
      i += static_cast<size_t>(samples_total);
    }
  }
  
  // Open a 16-bit, one-channel WAV formatted output file
  stk::FileWvOut output(filename, 1, stk::FileWrite::FILE_WAV, stk::Stk::STK_SINT16);
  for (double sample : samples) {
    output.tick(sample);
  }
  output.closeFile();
  
  return LIT_UNIT;
}
