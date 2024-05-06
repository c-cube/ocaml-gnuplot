
default:
	@dune build @install

clean:
	@dune clean

doc:
	@dune build @doc

format:
	@dune build $(DUNE_OPTS) @fmt --auto-promote

format-check:
	@dune build $(DUNE_OPTS) @fmt --display=quiet

.PHONY: default clean
