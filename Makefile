.PHONY: test check

utop:
	dune utop bin

build:
	dune build

test:
	dune exec test/perlin.exe

clean:
	dune clean

doc:
	dune build @doc

