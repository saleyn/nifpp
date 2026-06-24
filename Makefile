all: compile test

compile test clean disassemble:
	make --no-print-directory -C test $@

clean:
	make --no-print-directory -C test $@
	rm -f rebar3.crashdump

.PHONY: test
