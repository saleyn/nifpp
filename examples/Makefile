ERLANG_PATH = $(shell erl -eval 'io:format("~s", [lists:concat([code:root_dir(), "/erts-", erlang:system_info(version), "/include"])])' -s init stop -noshell)
NIF_INCLUDE:=-I../ -I$(ERLANG_PATH)

COMMON_FLAGS = -g -O3 -ansi -pedantic -Wall -Wextra $(NIF_INCLUDE)
CFLAGS = -std=c99
CPPFLAGS = -std=c++17

ifneq ($(OS),Windows_NT)
	COMMON_FLAGS += -fPIC

	ifeq ($(shell uname),Darwin)
		LDFLAGS += -dynamiclib -undefined dynamic_lookup
	else
		LDFLAGS += -shared
	endif
endif

all: nifpptest.so tuple_twiddle_cpp.so tuple_twiddle_c.so nifpptest.beam tuple_twiddle_cpp.beam tuple_twiddle_c.beam

nifpptest.o nifpptest_2tu.o: %.o: %.cpp ../nifpp.h
	$(CXX) $(COMMON_FLAGS) $(CPPFLAGS) $(LDFLAGS) $(CPPFLAGS) -c $<

nifpptest.so: nifpptest.o nifpptest_2tu.o
	$(CXX) $(COMMON_FLAGS) $(CPPFLAGS) $(LDFLAGS) $(CPPFLAGS) -o $@ $^

mmap_binary.so: mmap_binary.cpp ../nifpp.h
	$(CXX) $(COMMON_FLAGS) $(CPPFLAGS) $(LDFLAGS) $(CPPFLAGS) -o $@ -lboost_iostreams-mt $<

tuple_twiddle_c.so: tuple_twiddle_c.c ../nifpp.h
	$(CC) $(COMMON_FLAGS) $(CFLAGS) $(LDFLAGS) -o $@ $<

tuple_twiddle_cpp.so: tuple_twiddle_cpp.cpp ../nifpp.h
	$(CXX) $(COMMON_FLAGS) $(CPPFLAGS) $(LDFLAGS) $(CPPFLAGS) -o $@ $<

%.beam: %.erl
	erlc $<

test: all
	erl -noshell -noinput -s nifpptest

clean:
	rm -f *.so *.beam *.o
	rm -rf *.so.dSYM
