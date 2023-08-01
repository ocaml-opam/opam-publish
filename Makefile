all: build

ALWAYS:
	@

build: ALWAYS
	dune build @install
	cp _build/default/src/publishMain.exe ./opam-publish

install: ALWAYS
	dune install

uninstall: ALWAYS
	dune uninstall

clean: ALWAYS
	rm -rf _build opam-publish .merlin
