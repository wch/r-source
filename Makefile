####---- Master Makefile for R --- this is NOT made by configure ---

include Makeconf

all: R docs

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
dvi: build-dvi

build-docs build-help build-latex build-html build-dvi::
	-@cd etc; make $@

tests test-Examples::
	-@cd tests; make $@

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

install: R help html
	$(INSTALL) -d $(bindir) $(mandir)/man1
	$(INSTALL_DATA) R.1 $(mandir)/man1
	$(INSTALL) -d $(rhome)/bin \
		$(rhome)/cmd \
		$(rhome)/etc \
		$(rhome)/include \
		$(rhome)/library
	$(INSTALL_DATA) COPYING COPYRIGHTS MIRROR-SITES RESOURCES $(rhome)
	cd afm; make install
	$(INSTALL_PROGRAM) bin/R.binary $(rhome)/bin
	cat bin/R | sed "s@RHOME=.*@RHOME=$(rhome)@" > $(bindir)/R
	chmod 755 $(bindir)/R
	$(INSTALL_PROGRAM) cmd/[a-z]* $(rhome)/cmd
	cd demos; make install
	cd doc; make install
	cd etc; make install
	$(INSTALL_DATA) include/*.h $(rhome)/include
	@echo "Installing library ..."
	cd library; tar c [a-z]* | (cd $(rhome)/library; tar x) 
	$(INSTALL_DATA) library/LibIndex $(rhome)/library

