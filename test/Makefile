ERLANG_PATH = $(shell erl -eval 'io:format("~s", [lists:concat([code:root_dir(), "/erts-", erlang:system_info(version), "/include"])])' -s init stop -noshell)
NIF_INCLUDE:=-I../ -I$(ERLANG_PATH)

COMMON_FLAGS = -g -O3 -ansi -pedantic -Wall -Wextra -ffunction-sections $(NIF_INCLUDE)
CFLAGS = -std=c99
CPPFLAGS = -std=c++20

ifeq ($(disassemble),)
ifneq ($(OS),Windows_NT)
  COMMON_FLAGS += -fPIC

  ifeq ($(shell uname),Darwin)
    LDFLAGS += -dynamiclib -undefined dynamic_lookup
  else
    LDFLAGS += -shared
  endif
endif
endif

all: compile
	
compile: nifpptest.so tuple_twiddle_cpp.so$(SFX) tuple_twiddle_c.so$(SFX) \
  nifpptest.beam tuple_twiddle_cpp.beam tuple_twiddle_c.beam

%_c.o: %_c.c ../nifpp.h
	$(CC) $(COMMON_FLAGS) $(CFLAGS) -c $<

%.o: %.cpp ../nifpp.h
	$(CXX) $(COMMON_FLAGS) $(CPPFLAGS) -c $<

nifpptest.so: nifpptest.o nifpptest_2tu.o
	$(CXX) $(LDFLAGS) -o $@ $^

mmap_binary.so: mmap_binary.o
	$(CXX) $(LDFLAGS) -lboost_iostreams-mt -o $@ $<

tuple_twiddle_c.so: tuple_twiddle_c.o
	$(CC) $(LDFLAGS) -o $@ $<

tuple_twiddle_cpp.so: tuple_twiddle_cpp.o
	$(CXX) $(LDFLAGS) -o $@ $<

%.beam: %.erl
	erlc $<

test: compile
	erl -noshell -noinput -s nifpptest

clean:
	rm -f *.so* *.beam *.o *.dump *.asm

disassemble:
	objdump -w -d --section=.text.twiddle_nif tuple_twiddle_cpp.o > tuple_twiddle_cpp.asm
	objdump -w -d --section=.text.twiddle_nif tuple_twiddle_c.o   > tuple_twiddle_c.asm
