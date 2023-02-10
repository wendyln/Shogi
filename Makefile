.PHONY: test check

setup:
	opam update
	opam install -y utop odoc ounit2 qcheck bisect_ppx menhir ocaml-lsp-server ocamlformat ocamlformat-rpc ANSITerminal lwt
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f shogi.zip
	zip -r shogi.zip . -x@exclude.lst

clean:
	dune clean
	rm -f shogi.zip

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh
