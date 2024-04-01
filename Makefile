build:
	dune build

utop:
	dune utop input/src

.PHONY: test
test:
	dune exec input/test/test_input.exe

clean:
	dune clean

