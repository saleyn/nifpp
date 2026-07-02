all: compile test

compile test disassemble:
	@make --no-print-directory -C test $@

clean:
	@make --no-print-directory -C test $@
	@rm -f rebar3.crashdump

check:
	@if command -v clang-tidy 2>/dev/null; then \
	  clang-tidy --config-file=.clang-tidy enif.hpp -- -std=c++20 \
		-I. -I$(shell erl -noshell -eval 'io:format("~s/usr/include~n", [code:root_dir()]), halt().'); \
	fi
	clang-format -i enif.hpp

bump-version:
	@MAJOR=$$(grep 'NIFPP_MAJOR_VSN' enif.hpp | grep -o '[0-9]*'); \
	MINOR=$$(grep 'NIFPP_MINOR_VSN' enif.hpp | grep -o '[0-9]*'); \
	NEW_MINOR=$$((MINOR + 1)); \
	TAG="$$MAJOR.$$NEW_MINOR"; \
	echo "Bumping version $$MAJOR.$$MINOR -> $$TAG"; \
	printf "Commit change and create tag $$TAG? [y/N] "; \
	read ACK; \
	if [ "$$ACK" = "y" ] || [ "$$ACK" = "Y" ]; then \
		sed -i "s/NIFPP_MINOR_VSN = $$MINOR/NIFPP_MINOR_VSN = $$NEW_MINOR/" enif.hpp; \
		git add enif.hpp; \
		git commit -m "Bump version to $$TAG"; \
		git tag "$$TAG"; \
		echo "Tagged $$TAG"; \
	else \
		echo "Aborted."; \
	fi

CheckOptions:
.PHONY: test bump-version
