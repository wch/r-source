## ${RHOME}/etc/Makefile

include ../Makeconf


CleanMe1 = SHLIB COMPILE INSTALL REMOVE
CleanMe2 = Rdconv Rdindex Rman2Rd Sd2Rd
CleanMe3 = build-help help.pretty html2dos
CleanMe  = $(CleanMe1) $(CleanMe2) $(CleanMe3)

MANSRC = ../src/library/*/man/*.Rd
MANbaseDIR = ../src/library/base/man
MANdest= ../doc/manual

##--------------------------------- TARGETS ------------------------------
DVI = $(MANdest)/Man.dvi

build-help: stamp-help
build-html: stamp-html
build-latex: stamp-latex $(DVI)
build-docs: stamp-docs $(DVI)

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

$(DVI): stamp-latex
	-(cd $(MANdest); make Man.dvi)

clean:
	-(cd $(MANdest); make $@)

moreclean: clean
	@rm -f $(CleanMe1)

distclean realclean:
	@rm -fr $(CleanMe)
	@rm -f stamp-*


install:
	$(INSTALL) -d $(rhome)/etc
	$(INSTALL) Makeconf COMPILE INSTALL REMOVE SHLIB $(rhome)/etc
	$(INSTALL) Rd2dvi  Rd2txt Rdconv Rdconvlib.pl Rdindex Rman2Rd \
		   Sd2Rd build-help buildlib.pl help.pretty \
		   html-layout.pl $(rhome)/etc
	$(INSTALL_PROGRAM) Rd2txt Rdconv Rdindex Sd2Rd $(bindir)
