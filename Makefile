####---- Master Makefile for R --- this is NOT made by configure ---

R: config.status
	@echo "Building R"
	cd src; make R
	@echo; echo "You should"; echo "	make docs"
	@echo "now, or at least"; echo "	make help"; echo

config.status: configure config.site
	-./configure

docs: build-docs
help: build-help
html: build-html
latex: build-latex

install:
	@echo "Installing R"
	@echo "SORRY, this hasn't been implemented yet."

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
	@echo "Cleaning ./etc/ a bit more"; cd etc; make $@

acclean:
	@echo "Cleaning configure files"
	@rm -f config.cache config.log config.status

distclean: realclean

realclean: acclean
	@echo "Cleaning at top level"
	@-rm -f bin/R*
	@-rm -rf `echo library/* | sed 's@library/CVS *@@'`
	@-rm -f doc/manual/pkg-*
	@-rm -f doc/html/packages* doc/html/function* 
	@cd demos/dynload; make $@
	@echo "Really cleaning ./etc/"; cd etc; make $@
	@echo "Really cleaning the source tree"; cd src; make $@
