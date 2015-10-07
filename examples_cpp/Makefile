CC  = clang
CXX = clang++

INCLUDES = -I/usr/local/include/

FLAGS    = -Wall -pedantic -O2 $(INCLUDES)
CFLAGS   = $(FLAGS)
CXXFLAGS = $(FLAGS) -std=c++1z

LDFLAGS = -L/usr/local/lib/
LDLIBS  = -lstdc++ -lstk

.PHONY: default
default: twinkle

twinkle: twinkle.o

.PHONY: clean
clean:
	rm -f *~ *.wav *.o core twinkle

.PHONY: all
all: clean default
