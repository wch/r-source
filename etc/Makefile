## ${RHOME}/etc/Makefile

include ../Makeconf


CleanMe1 = SHLIB INSTALL REMOVE
CleanMe2 = Rdconv Rdindex Rman2Rd Sd2Rd
CleanMe3 = build-help help.pretty html2dos
CleanMe  = $(CleanMe1) $(CleanMe2) $(CleanMe3)

#MANSRC = ../src/library/*/man/*.Rd
MANbaseDIR = ../src/library/base/man
MANdest= ../doc/manual

##--------------------------------- TARGETS ------------------------------
DVI = $(MANdest)/Man.dvi

build-help: stamp-help
build-html: stamp-html
build-latex: stamp-latex
build-docs: stamp-docs
build-dvi:  $(DVI)

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
	-(cd $(MANdest); $(MAKE) Man.dvi)

clean:
	-(cd $(MANdest); $(MAKE) $@)

moreclean: clean
	@rm -f $(CleanMe1)

distclean realclean:
	@rm -fr $(CleanMe)
	@rm -f stamp-*


install:
	$(INSTALL) -d $(rhome)/etc
	for f in COMPILE INSTALL REMOVE SHLIB Rd2dvi Rd2txt \
	   Rdconv Rdindex Rman2Rd Sd2Rd build-help help.pretty; \
		do $(INSTALL) $$f $(rhome)/etc; \
	done
	for f in buildlib.pl html-layout.pl Makeconf Rdconvlib.pl; \
		do $(INSTALL_DATA) $$f $(rhome)/etc; \
	done
	for f in Rd2txt Rdconv Rdindex Sd2Rd; \
		do $(INSTALL_PROGRAM) $$f $(bindir); \
	done
