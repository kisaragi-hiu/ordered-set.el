EMACS ?= emacs

all: test

test: clean-elc
	${MAKE} compile
	${MAKE} unit
	${MAKE} clean-elc

unit:
	cask emacs --batch -L . -L test -l "test/set-test.el" -f ert-run-tests-batch
	cask emacs --batch -L . -L test -l "test/set-test-perf.el" -f ert-run-tests-batch

compile:
	cask build

clean-elc:
	rm -f set.elc

.PHONY:	all test unit compile
