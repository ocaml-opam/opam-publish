all: opam-publish

ALWAYS:
	@

opam-publish: _build/default/src/publishMain.exe ALWAYS
	jbuilder build $<
	@cp $< $@

build: ALWAYS
	jbuilder build @install --dev

install: ALWAYS
	jbuilder install

uninstall: ALWAYS
	jbuilder uninstall

clean: ALWAYS
	rm -rf _build opam-publish .merlin
