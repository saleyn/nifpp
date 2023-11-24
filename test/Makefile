ERLANG_PATH = $(shell erl -eval 'io:format("~s", [lists:concat([code:root_dir(), "/erts-", erlang:system_info(version), "/include"])])' -s init stop -noshell)
NIF_INCLUDE:=-I../ -I$(ERLANG_PATH)

ifneq ($(optimize),false)
  OPT = -O3 -ffunction-sections -funroll-loops -fomit-frame-pointer
endif

COMMON_FLAGS = -g -ansi -pedantic -Wall -Wextra $(OPT) $(NIF_INCLUDE)
CFLAGS = -std=c99
CPPFLAGS = -std=c++20
LD_COMMON_FLAGS = -flto

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

compile: nifpptest.so tuple_twiddle_cpp.so$(SFX) tuple_twiddle_c.so$(SFX) selecttest.so$(SFX) \
  nifpptest.beam tuple_twiddle_cpp.beam tuple_twiddle_c.beam selecttest.beam

ast: tuple_twiddle_cpp.cpp ../nifpp.h
	clang++ $(COMMON_FLAGS) $(CPPFLAGS) -Xclang -ast-print -fsyntax-only $< > $<.ast

%_c.o: %_c.c ../nifpp.h
	$(CC) $(COMMON_FLAGS) $(CFLAGS) -c $<

%.o: %.cpp ../nifpp.h
	$(CXX) $(COMMON_FLAGS) $(CPPFLAGS) -c $<

selecttest.o: selecttest.cpp ../nifpp.h
	$(CXX) $(COMMON_FLAGS) $(CPPFLAGS) -fpermissive -c $<

nifpptest.so: nifpptest.o nifpptest_2tu.o
	$(CXX) $(LDFLAGS) $(LD_COMMON_FLAGS) -o $@ $^

selecttest.so: selecttest.o
	$(CXX) $(LDFLAGS) $(LD_COMMON_FLAGS) -o $@ $^

mmap_binary.so: mmap_binary.o
	$(CXX) $(LDFLAGS) $(LD_COMMON_FLAGS) -lboost_iostreams-mt -o $@ $<

tuple_twiddle_c.so: tuple_twiddle_c.o
	$(CC) $(LDFLAGS) $(LD_COMMON_FLAGS) -o $@ $<

tuple_twiddle_cpp.so: tuple_twiddle_cpp.o
	$(CXX) $(LDFLAGS) $(LD_COMMON_FLAGS) -o $@ $<

%.beam: %.erl
	erlc -DTEST -DEUNIT $<

test: compile
	erl -noshell -noinput -s selecttest -eval 'halt(0).'
	erl -noshell -noinput -s nifpptest

clean:
	rm -f *.so* *.beam *.o *.dump *.asm *.ast

disassemble:
	objdump -w -d --section=.text.twiddle_nif tuple_twiddle_cpp.o > tuple_twiddle_cpp.asm
	objdump -w -d --section=.text.twiddle_nif tuple_twiddle_c.o   > tuple_twiddle_c.asm
