####---- Master Makefile for R --- this is NOT made by configure ---

include Makeconf

all: R docs

R: config.status
	@echo "Building R"
	cd src; $(MAKE) R
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
	-@cd etc; MANSRC=../src/library/*/man/*.Rd $(MAKE) $@

tests test-Examples::
	-@cd tests; $(MAKE) $@

patches:
	@echo "Patches? Patches?  We don't need no stinking patches!"

clean:
	@echo "Cleaning at top level"
	@cd demos/dynload; $(MAKE) $@
	@echo "Cleaning ./etc/"; cd etc; $(MAKE) $@
	@echo "Cleaning the source tree"; cd src; $(MAKE) $@

moreclean: clean acclean
	@echo "Cleaning ./etc/ a bit more"; cd etc; $(MAKE) $@

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
	@cd demos/dynload; $(MAKE) $@
	@echo "Really cleaning ./etc/"; cd etc; $(MAKE) $@
	@echo "Really cleaning the source tree"; cd src; $(MAKE) $@

install: all
	$(INSTALL) -d $(bindir) $(mandir)/man1
	$(INSTALL_DATA) R.1 $(mandir)/man1
	$(INSTALL) -d $(rhome)/bin \
		$(rhome)/cmd \
		$(rhome)/etc \
		$(rhome)/include \
		$(rhome)/library
	$(INSTALL_DATA) COPYING COPYRIGHTS MIRROR-SITES RESOURCES $(rhome)
	cd afm; $(MAKE) install
	$(INSTALL_PROGRAM) bin/R.binary $(rhome)/bin
	cat bin/R | sed "s@RHOME=.*@RHOME=$(rhome)@" > $(bindir)/R
	chmod 755 $(bindir)/R
	$(INSTALL_PROGRAM) cmd/[a-z]* $(rhome)/cmd
	cd demos; $(MAKE) install
	cd doc; $(MAKE) install
	cd etc; $(MAKE) install
	$(INSTALL_DATA) include/*.h $(rhome)/include
	@echo "Installing library ..."
	cd library; tar c [a-z]* | (cd $(rhome)/library; tar x) 
	$(INSTALL_DATA) library/LibIndex $(rhome)/library

