## ${RHOME}/etc/Makefile

## These are used for `tests' dependencies:
Rbinary = ../bin/R.binary
Rbase   = ../library/base
TESTS   = ../tests
Ex_Tests = $(TESTS)/Examples
CleanMe = SHLIB COMPILE INSTALL Rdconv Rdindex Rman2Rd Sd2Rd


## The following must be an absolute path or RELATIVE to 'Ex_Tests':
#ok for GNU make:
#R	= $(shell cd ..;pwd)/bin/R
R	= ../../bin/R

MANSRC = ../src/library/*/man/*
MANbaseDIR = ../src/library/base/man

##--- Uncomment one of the 2 'Rd_files' defintion -- depending on 'make' version
##--- <<<<<< CONFIGURE should do this >>>>>>
##-- Sun make
#> Rd_files:sh = ./Rd.files
##-- GNU make
Rd_files := $(shell ./Rd.files)
#________in future_______  (for ALL 'make' versions):
#Rd_files = $(MANbaseDIR)/*.Rd

Ex_Tests_src = $(Rd_files:%=$(Ex_Tests)/%.R)
#________in future_______:
#Ex_Tests_src = $(Rd_files:%.Rd=$(Ex_Tests)/%.R)


##--------------------------------- TARGETS ------------------------------
build-help: stamp-help
build-html: stamp-html
build-latex: stamp-latex
tests: test-Examples
test-Examples: $(Ex_Tests)/All-Ex.Rout

##-- The next two are just for testing purposes:
show-R: ; @-(cd ../include; echo "R = '$(R)'")
show-Rd:
	@-echo 'R help() files = "Rd files" :'
	@-echo $(Rd_files)

$(Ex_Tests)/%.R: $(MANbaseDIR)/%
	./doc2EX < $< > $@


stamp-help: $(MANSRC)
	@echo "Building ALL help pages"
	./build-allhelp
	@touch $@
##future:
#stamp-html: $(MANSRC)
stamp-html: $(MANbaseDIR)/*
	@echo "Building R Base Reference Manual (in HTML)"
	./build-html
	@touch $@
##future:
#stamp-latex: $(MANSRC)
stamp-latex: $(MANbaseDIR)/*
	@echo "Building R Base Reference Manual (in LaTeX/dvi)"
	./build-latex
	@touch $@

$(Ex_Tests):
	@if [ ! -d $@ ]; then mkdir -p $@ ; fi

$(Ex_Tests)/All-Ex.Rout: $(Ex_Tests) $(Ex_Tests)/All-Ex.R $(Rbinary) $(Rbase)
	@if [ -f $@.bak ]; then mv $@.bak $@.bakk ; fi
	@if [ -f $@ ]; then mv $@ $@.bak ; fi
	@echo 'Running all help() EXAMPLES ..'
	(cd $(Ex_Tests);rm -f .RData; $(R) -v 4 < All-Ex.R)> $@

$(Ex_Tests)/All-Ex.R: $(Ex_Tests_src)
	@if [ -f $@.bak ]; then mv $@.bak $@.bakk ; fi
	@if [ -f $@ ]; then mv $@ $@.bak ; fi
	@echo "massaging single Examples into $@ ..."
	@./massage-Examples $(Ex_Tests_src) > $@

clean:
	@rm -f COMPILE INSTALL SHLIB
	@rm -f $(Ex_Tests)/All-Ex.R

realclean: clean
	@rm -fr $(Ex_Tests) $(CleanMe)
