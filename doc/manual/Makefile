## The following must be an ABSOLUTE path
## or RELATIVE to 'Ex_Tests' (RHOME/test/Examples/ :

include ../../Makeconf

rRHOME= ../..
#
RETC = $(rRHOME)/etc
RLIB = $(rRHOME)/library
RsrcLIB = $(rRHOME)/src/library
BUILDlatex = $(RETC)/Rdconv --type latex

Packages = pkg-base.tex pkg-eda.tex pkg-mva.tex pkg-stepfun.tex
#- Man.tex  also must have an  \input{..} for each of these ^^^^
Manparts = Man-1.ps Man-2.ps Man-3.ps Man-4.ps\
	   Man-5.ps Man-6.ps Man-7.ps Man-8.ps
Manbooks = $(Manparts:.ps=.bps)


all: Man.dvi

$(RLIB)/base/latex/%.tex: $(RsrcLIB)/base/man/%.Rd
	$(BUILDlatex) $< > $@
%.bps: %.ps
	psbook $< | pstops \
	 -w21cm -h29.7cm  '2:0L@.80(1.09w,-.04h)+1L@.80(1.09w,.46h)' > $@

pkg-base.tex: $(RLIB)/base/latex/*.tex
	@echo 'Collecting components of  $@ ..'
	./lib2tex base
pkg-eda.tex: $(RLIB)/eda/latex/*.tex
	@echo 'Collecting components of  $@ ..'
	./lib2tex eda
pkg-mva.tex: $(RLIB)/mva/latex/*.tex
	@echo 'Collecting components of  $@ ..'
	./lib2tex mva
pkg-stepfun.tex: $(RLIB)/stepfun/latex/*.tex
	@echo 'Collecting components of  $@ ..'
	./lib2tex stepfun

Man.dvi: Man.tex Version.tex Rd.sty $(Packages)
	@echo 'making latex documentation...'
	-latex '\nonstopmode\input{Man.tex}'
	-makeindex Man
	-latex '\nonstopmode\input{Man.tex}'

ABOUT.dvi: ABOUT.tex writing-Rd.tex Rd.sty
	-latex ABOUT
	-latex ABOUT
ABOUT.ps: ABOUT.dvi
	-dvips -f $< -o $@
##-- need \nonstopmode, since we still have some 'alias' problems in Index
Version.tex: $(rRHOME)/bin/R
	$^ --version > $@

Manbooks:: $(Manbooks)

$(Manparts): Man.dvi
	dvips        -l36 -f $< -o Man-1.ps
	dvips  -p37  -l72 -f $< -o Man-2.ps
	dvips  -p73 -l108 -f $< -o Man-3.ps
	dvips -p109 -l144 -f $< -o Man-4.ps
	dvips -p145 -l180 -f $< -o Man-5.ps
	dvips -p181 -l216 -f $< -o Man-6.ps
	dvips -p217 -l252 -f $< -o Man-7.ps
	dvips -p253 -l288 -f $< -o Man-8.ps
	dvips -p289       -f $< -o Man-9.ps

Man.ps:	Man.dvi
	dvips -f $< -o $@

Man-plotmath.ps: Man.dvi
	dvips -f $< -pp5-6 -S 2   -i -o $@
	b=`basename $@ .ps`; rm -f $$b.001; mv $$b.002 $@

Man_2on1.dvi: Man.dvi
	-dvidvi -m "2:0,1(21.1cm,0)" $^ $@
Man.ps-2: Man_2on1.dvi
	-dvips -f $< -x 707 -t landscape -O -0.69cm,-0.70cm -o $@
Man-index.ps-2: Man.ps-2
	P=`perl -ne 'next unless /\bbop\b.*\bIndex\b/; @_=split; printf "%s\n", int(.5+$$_[1]/2)' Man.ps`; psselect -p"$$P"- $^ > $@

##-- double sided printing.. [NOT tested for a while...]
print2:	Man.ps
	mpage -P -2 $< > Man.ps-2
	lpr -s Man.ps-2

clean:
	@echo cleaning in doc/manual
	@-rm -f *.aux *.toc Man.i?? *.dvi *.log *.ps *.ps-2

realclean: clean
	@echo "'realclean'ing in manual/latex"
	@-rm -f -r auto pkg-*.tex

install:
	$(INSTALL) -d $(rhome)/doc/manual
	for f in Makefile *.tex *.sty; \
		do $(INSTALL_DATA) $$f $(rhome)/doc/manual; \
	done

