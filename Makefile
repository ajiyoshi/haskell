
all: test

test: doctest

doctest:
	doctest Pipeline.hs
