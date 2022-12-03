.PHONY: test check

cloc:
	cloc --by-file --include-lang=OCaml .

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

zip:
	rm -f fp.zip
	zip -r fp.zip . -x@exclude.lst

clean:
	dune clean
	rm -f fp.zip

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh