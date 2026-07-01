all: compile test

compile test disassemble:
	@make --no-print-directory -C test $@

clean:
	@make --no-print-directory -C test $@
	@rm -f rebar3.crashdump

check:
	clang-tidy --config-file=.clang-tidy enif.hpp -- -std=c++20 \
		-I. -I$(shell erl -noshell -eval 'io:format("~s/usr/include~n", [code:root_dir()]), halt().')
	clang-format -i enif.hpp

CheckOptions:
.PHONY: test
