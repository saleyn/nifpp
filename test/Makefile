# Try to get Erlang include path from environment first, then auto-detect
ERLANG_PATH = $(or $(ERLANG_INCLUDE),$(shell erl -eval 'io:format("~s", [lists:concat([code:root_dir(), "/erts-", erlang:system_info(version), "/include"])])' -s init stop -noshell 2>/dev/null || echo ""))
# If auto-detection failed, try alternative paths
ifeq ($(ERLANG_PATH),)
  ERLANG_PATH = $(shell erl -eval 'io:format("~s", [lists:concat([code:root_dir(), "/usr/include"])])' -s init stop -noshell 2>/dev/null || echo "")
endif
NIF_INCLUDE:=-I../ $(if $(ERLANG_PATH),-I$(ERLANG_PATH),)

# Debug output for CI troubleshooting
$(info DEBUG: ERLANG_INCLUDE=$(ERLANG_INCLUDE))
$(info DEBUG: ERLANG_PATH=$(ERLANG_PATH))
$(info DEBUG: NIF_INCLUDE=$(NIF_INCLUDE))

ifneq ($(optimize),false)
  OPT = -O3 -ffunction-sections -funroll-loops -fomit-frame-pointer
endif

COMMON_FLAGS = -g -pedantic -Wall -Wextra $(OPT) $(NIF_INCLUDE)
CFLAGS = -std=c99
CPPFLAGS = -std=c++20
LD_COMMON_FLAGS = -flto

ifeq ($(disassemble),)
  # Windows-specific settings
  ifeq ($(OS),Windows_NT)
    LDFLAGS += -shared
    LIB_EXT = .dll
  else
    COMMON_FLAGS += -fPIC
    LIB_EXT = .so

    ifeq ($(shell uname),Darwin)
      LDFLAGS += -dynamiclib -undefined dynamic_lookup
    else
      LDFLAGS += -shared
    endif
  endif
endif

all: compile

# Define Linux-specific targets
ifeq ($(shell uname -s 2>/dev/null || echo unknown),Linux)
  LINUX_TARGETS = selecttest.so$(SFX) select_wrapper_test.so$(SFX) selecttest.beam select_wrapper_test.beam
else
  LINUX_TARGETS =
endif

compile: nifpptest.so tuple_twiddle_cpp.so$(SFX) tuple_twiddle_c.so$(SFX) \
  nifpptest.beam tuple_twiddle_cpp.beam tuple_twiddle_c.beam $(LINUX_TARGETS)

ast: tuple_twiddle_cpp.cpp ../enif.hpp
	clang++ $(COMMON_FLAGS) $(CPPFLAGS) -Xclang -ast-print -fsyntax-only $< > $<.ast

%_c.o: %_c.c ../enif.hpp
	$(CC) $(COMMON_FLAGS) $(CFLAGS) -c $<

%.o: %.cpp ../enif.hpp
	$(CXX) $(COMMON_FLAGS) $(CPPFLAGS) -c $<

# Linux-specific build rules
ifeq ($(shell uname -s 2>/dev/null || echo unknown),Linux)
selecttest.o: selecttest.cpp ../enif.hpp
	$(CXX) $(COMMON_FLAGS) $(CPPFLAGS) -fpermissive -c $<
endif

nifpptest.so: nifpptest.o nifpptest_2tu.o
	$(CXX) $(LDFLAGS) $(LD_COMMON_FLAGS) -o $@ $^

ifeq ($(shell uname -s 2>/dev/null || echo unknown),Linux)
selecttest.so: selecttest.o
	$(CXX) $(LDFLAGS) $(LD_COMMON_FLAGS) -o $@ $^

select_wrapper_test.so: select_wrapper_test.o
	$(CXX) $(LDFLAGS) $(LD_COMMON_FLAGS) -o $@ $^
endif

mmap_binary.so: mmap_binary.o
	$(CXX) $(LDFLAGS) $(LD_COMMON_FLAGS) -lboost_iostreams-mt -o $@ $<

tuple_twiddle_c.so: tuple_twiddle_c.o
	$(CC) $(LDFLAGS) $(LD_COMMON_FLAGS) -o $@ $<

tuple_twiddle_cpp.so: tuple_twiddle_cpp.o
	$(CXX) $(LDFLAGS) $(LD_COMMON_FLAGS) -o $@ $<

%.beam: %.erl
	erlc -DTEST -DEUNIT $<

test: compile
ifeq ($(shell uname -s 2>/dev/null || echo unknown),Linux)
	erl -noshell -noinput -s selecttest -eval 'halt(0).'
	erl -noshell -noinput -s select_wrapper_test -eval 'halt(0).'
endif
	erl -noshell -noinput -s nifpptest

clean:
	rm -f *.so* *.beam *.o *.dump *.asm *.ast

disassemble:
	objdump -w -d --section=.text.twiddle_nif tuple_twiddle_cpp.o > tuple_twiddle_cpp.asm
	objdump -w -d --section=.text.twiddle_nif tuple_twiddle_c.o   > tuple_twiddle_c.asm
