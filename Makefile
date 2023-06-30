EMACS ?= emacs

all: test

test: clean-elc
	${MAKE} unit
	${MAKE} compile
	${MAKE} unit
	${MAKE} clean-elc

unit:
	eask test ert 'test/*test.el'
	eask test ert 'test/*perf.el'

compile:
	eask compile

clean-elc:
	rm -f set.elc

.PHONY:	all test unit compile
