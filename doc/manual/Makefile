## The following must be an ABSOLUTE path
## or RELATIVE to 'Ex_Tests' (RHOME/test/Examples/ :
rRHOME= ../..
#
RETC = $(rRHOME)/etc
RLIB = $(rRHOME)/library
RsrcLIB = $(rRHOME)/src/library
BUILDlatex = $(RETC)/Rdconv --type latex

Packages = pkg-base.tex pkg-eda.tex pkg-mva.tex pkg-stepfun.tex
#- Man.tex  also must have an  \input{..} for each of these ^^^^
Manparts = Man-1.ps Man-2.ps Man-3.ps Man-4.ps
Manbooks = $(Manparts:.ps=.bps)


all: Man.dvi

$(RLIB)/base/latex/%.tex: $(RsrcLIB)/base/man/%.Rd
	$(BUILDlatex) $< > $@
%.bps: %.ps
	psbook $< | pstops \
	 -w21cm -h29.7cm  '2:0L@.80(1.09w,-.04h)+1L@.80(1.09w,.46h)' > $@

pkg-base.tex: $(RLIB)/base/latex/*.tex
	@echo 'Collecting components of  $@ ..'
	@sed 's/__PKG__/base/g' Pkg-start.tex | cat - $^ Pkg-end.tex > $@
pkg-eda.tex: $(RLIB)/eda/latex/*.tex
	@echo 'Collecting components of  $@ ..'
	@sed 's/__PKG__/eda/g' Pkg-start.tex | cat - $^ Pkg-end.tex > $@
pkg-mva.tex: $(RLIB)/mva/latex/*.tex
	@echo 'Collecting components of  $@ ..'
	@sed 's/__PKG__/mva/g' Pkg-start.tex | cat - $^ Pkg-end.tex > $@
pkg-stepfun.tex: $(RLIB)/stepfun/latex/*.tex
	@echo 'Collecting components of  $@ ..'
	@sed 's/__PKG__/stepfun/g' Pkg-start.tex | cat - $^ Pkg-end.tex > $@

Man.dvi: Man.tex Version.tex Rd.sty $(Packages)
	@echo 'making latex documentation...'
	-latex '\nonstopmode\input{Man.tex}'
	-makeindex Man
	-latex '\nonstopmode\input{Man.tex}'
##-- need \nonstopmode, since we still have some 'alias' problems in Index
Version.tex:
	../../bin/R --version > $@

Manbooks:: $(Manbooks)

$(Manparts): Man.dvi
	dvips        -l64 -f $< -o Man-1.ps
	dvips  -p65 -l128 -f $< -o Man-2.ps
	dvips -p129 -l192 -f $< -o Man-3.ps
	dvips -p193       -f $< -o Man-4.ps

Man.ps:	Man.dvi
	dvips -f $< -o $@

Man-plotmath.ps: Man.dvi
	dvips -f $< -pp5-6 -S 2   -i -o $@
	b=`basename $@ .ps`; rm -f $$b.001; mv $$b.002 $@

print2:	Man.ps
	mpage -P -2 $< > Man.ps-2
	lpr -s Man.ps-2

clean:
	@echo cleaning in doc/manual
	@-rm -f *.aux *.toc Man.i?? *.dvi *.log *.ps *.ps-2

realclean: clean
	@echo "'realclean'ing in manual/latex"
	@-rm -f -r auto pkg-*.tex

