## ${RHOME}/etc/Makefile

TESTS   = ../tests
Ex_Tests = $(TESTS)/Examples
CleanMe = SHLIB COMPILE INSTALL Rdconv Rdindex Rman2Rd Sd2Rd

MANSRC = ../src/library/*/man/*.Rd
MANbaseDIR = ../src/library/base/man

##--------------------------------- TARGETS ------------------------------
build-index: stamp-index
build-help: stamp-help
build-html: stamp-html
build-latex: stamp-latex
tests: test-Examples

test-Examples: $(Ex_Tests)
	@cp Makefile-test-Ex $(Ex_Tests)/Makefile
	-(cd  $(Ex_Tests); make $@)

stamp-help: build-index $(MANSRC)
	@echo "Building ALL help pages"
	./build-allhelp
	@touch $@

stamp-html: build-index $(MANSRC)
	@echo "Building R Base Reference Manual (in HTML)"
	./build-allhtml
	@touch $@

stamp-index: $(MANSRC)
	@echo "Building R Base Reference Manual (Index)"
	./build-allindex
	@touch $@


##future:
#stamp-latex: $(MANSRC)
stamp-latex: $(MANbaseDIR)/*.Rd
	@echo "Building R Base Reference Manual (in LaTeX/dvi)"
	./build-latex
	@touch $@

$(Ex_Tests):
	@if [ ! -d $@ ]; then mkdir -p $@ ; fi

clean:
	@rm -f COMPILE INSTALL SHLIB
	@(cd $(Ex_Tests); make $@)

distclean realclean:
	@rm -f COMPILE INSTALL SHLIB
	@rm -fr $(Ex_Tests) $(CleanMe)
	@rm -f stamp-*
