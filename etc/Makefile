## ${RHOME}/etc/Makefile

TESTS   = ../tests
Ex_Tests = $(TESTS)/Examples
CleanMe1 = SHLIB COMPILE INSTALL REMOVE
CleanMe2 = Rdconv Rdindex Rman2Rd Sd2Rd
CleanMe3 = build-help build-htmlpkglist help.pretty html2dos
CleanMe  = $(CleanMe1) $(CleanMe2) $(CleanMe3)

MANSRC = ../src/library/*/man/*.Rd
MANbaseDIR = ../src/library/base/man

##--------------------------------- TARGETS ------------------------------
DVI = ../doc/manual/Man.dvi

build-help: stamp-help
build-html: stamp-html
build-latex: stamp-latex $(DVI)
build-docs: stamp-docs $(DVI)
tests: test-Examples

test-Examples: $(Ex_Tests)
	@cp -p Makefile-test-Ex $(Ex_Tests)/Makefile
	-(cd $(Ex_Tests); make $@)

stamp-help: $(MANSRC)
	@echo "Building ALL help pages"
	./build-allhelp
	@touch $@

stamp-html: $(MANSRC)
	@echo "Building R Base Reference Manual (in HTML)"
	./build-allhtml
	@touch $@

#stamp-latex: $(MANbaseDIR)/*.Rd
stamp-latex: $(MANSRC)
	@echo "Building R Base Reference Manual (in LaTeX/dvi)"
	./build-alllatex
	@touch $@

stamp-docs: $(MANSRC)
	@echo "Building ALL documentation (help, HTML, LaTeX)"
	./build-alldocs
	@touch $@
	@touch stamp-help
	@touch stamp-html
	@touch stamp-latex

../doc/manual/Man.dvi: stamp-latex
	(cd ../doc/manual; make Man.dvi)

$(Ex_Tests):
	@if [ ! -d $@ ]; then mkdir -p $@ ; fi

clean:
	@rm -f $(CleanMe1)
	@-(cd $(Ex_Tests) && make $@)

distclean realclean:
	@rm -fr $(Ex_Tests) $(CleanMe)
	@rm -f stamp-*
