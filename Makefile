all: compile test

compile test clean disassemble:
	make -C test $@

.PHONY: test
