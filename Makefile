all: opam-publish

opam-publish: _build/src/opam_publish.native
	@cp $< $@

ALWAYS:
	@
_build/src/opam_publish.%: ALWAYS
	ocamlbuild -use-ocamlfind -pkgs opam-lib,opam-lib.repositories,opam-lib.client,cmdliner src/opam_publish.$*

clean:
	ocamlbuild -clean

