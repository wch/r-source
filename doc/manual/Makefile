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

all: Man.dvi

$(RLIB)/base/latex/%.tex: $(RsrcLIB)/base/man/%.Rd
	$(BUILDlatex) $< > $@

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

Man.dvi: Man.tex Rd.sty $(Packages)
	@echo 'making latex documentation...'
	-latex '\nonstopmode\input{Man.tex}'
	-makeindex Man
	-latex '\nonstopmode\input{Man.tex}'
##-- need \nonstopmode, since we still have some 'alias' problems in Index

Man.ps:	Man.dvi
	dvips -f $< -o $@

print2:	Man.ps
	mpage -P -2 $< > Man.ps-2
	lpr -s Man.ps-2

clean:
	@echo cleaning in doc/manual
	@-rm -f *.aux *.toc Man.i?? *.dvi *.log *.ps *.ps-2

realclean: clean
	@echo "'realclean'ing in manual/latex"
	@-rm -f -r auto pkg-*.tex

