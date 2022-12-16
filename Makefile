
export OCAMLRUNPARAM=b 

.PHONY: all
all:
	@dune test
	@dune exec example/example.exe