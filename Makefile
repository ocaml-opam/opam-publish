all: opam-publish

ALWAYS:
	@

opam-publish: ALWAYS
	dune build _build/default/src/publishMain.exe
	@cp _build/default/src/publishMain.exe $@

build: ALWAYS
	dune build @install --dev

install: ALWAYS
	dune install

uninstall: ALWAYS
	dune uninstall

clean: ALWAYS
	rm -rf _build opam-publish .merlin
