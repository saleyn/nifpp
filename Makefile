all: compile test

compile:
	make -C examples

test:
	make -C examples $@
