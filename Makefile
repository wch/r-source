####---- Master Makefile for R --- this is NOT made by configure ---

install: config.status
	@echo Building/Installing R
	cd src; make install
	@echo; echo You should;  echo "	make docs"
	@echo "now, or at least";echo "	make help"; echo

config.status: configure config.site
	-./configure

docs: build-docs
help: build-help
html: build-html
latex: build-latex

build-docs build-help build-latex build-html test-Examples:
	-@cd etc; make $@

tests: test-Examples

patches:
	@echo "Patches? Patches?  We don't need no stinking patches!"

clean:
	@echo "Cleaning at top level"
	@cd demos/dynload; make $@
	@echo "Cleaning ./etc/"; cd etc; make $@
	@echo "Cleaning the source tree"; cd src; make $@

moreclean: clean acclean
	@echo "More Cleaning ./etc/"; cd etc; make $@

acclean:
	@echo cleaning configure files
	@rm -f config.cache config.log config.status

distclean: realclean

realclean: acclean
	@echo cleaning at top level
	@-rm -f bin/R*
	@-rm -rf `echo library/* | sed 's@library/CVS *@@'`
	@-rm -f doc/manual/pkg-*
	@cd demos/dynload; make $@
	@echo "Realcleaning ./etc"; cd etc; make $@
	@echo "Realcleaning the source tree"; cd src; make $@
