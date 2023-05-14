.PHONY: test check

play:
	dune build
	dune exec ./bin/perlin.exe

build:
	dune build

test:
	dune exec test/test.exe

clean:
	dune clean

doc:
	dune build @doc

