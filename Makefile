EMACS ?= emacs

all: test

test:
	eldev test

compile:
	eldev compile

clean:
	eldev clean

.PHONY:	all clean test unit compile
