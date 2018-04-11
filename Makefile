all: opam-publish

ALWAYS:
	@

opam-publish: ALWAYS
	jbuilder build _build/default/src/publishMain.exe
	@cp _build/default/src/publishMain.exe $@

build: ALWAYS
	jbuilder build @install --dev

install: ALWAYS
	jbuilder install

uninstall: ALWAYS
	jbuilder uninstall

clean: ALWAYS
	rm -rf _build opam-publish .merlin
