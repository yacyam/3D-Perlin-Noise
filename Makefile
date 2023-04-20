.PHONY: test check

play:
	dune build
	dune exec ./bin/perlin.exe

build:
	dune build

test:
	dune exec test/perlin.exe

clean:
	dune clean

doc:
	dune build @doc

