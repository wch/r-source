## This Makefile is to be run on a Unix / Linux machine to make
## files necessary for the Macintosh build.

RHOME = ../..
all: profiles library headers html

## ------------- headers -----------
headers: ../include/config.h Rconfig.h Rversion.h FFDecl.h FFTab.h

../include/config.h: config.mac.h
	cp config.mac.h ../include/config.h

Rconfig.h: ../include/config.h
	(cd ../include; $(RHOME)/tools/GETCONFIG > ../macintosh/Rconfig.h)

Rversion.h: $(RHOME)/date-stamp $(RHOME)/VERSION
	$(RHOME)/tools/GETVERSION > Rversion.h

SED0='/^\#/d'
SED01='/Rsockfork/d'
SED1='s/F77_SUBROUTINE(\(.*\))/void * \1_();/'
SED2='s/C_FUNCTION(\(.*\))/void * \1();/'
SED3='s/F77_SUBROUTINE(\(.*\))/{ "\1_",	\1_ },/'
SED4='s/C_FUNCTION(\(.*\))/{ "\1",	\1 },/'

FFDecl.h: ../appl/ROUTINES
	sed -e $(SED0) -e $(SED01) -e $(SED1) -e $(SED2) $< > $@
FFTab.h: ../appl/ROUTINES
	sed -e $(SED0) -e $(SED01) -e $(SED3) -e $(SED4) $< > $@

## ------------- profiles -----------
profiles: $(RHOME)/library/base/R/Rprofile

$(RHOME)/library/base/R/Rprofile:  ../library/profile/Common.R \
../library/profile/Rprofile.mac
	@echo -------- Building $@ from $^--------
	mkdir -p $(RHOME)/library/base/R
	cat ../library/profile/Common.R ../library/profile/Rprofile.mac > $(RHOME)/library/base/R/Rprofile

## ------------- library -----------
PACKAGES = base ctest eda lqs modreg mva nls splines stepfun ts
DATA = base modreg mva nls ts

fixdesc:
	echo "creating DESCRIPTION files"
	@sh ../gnuwin32/fixed/GETDESC $(PACKAGES)

library: mainfiles datafiles help zipfiles

mainfiles: fixdesc
	@mkdir -p $(RHOME)/library
	@for pkg in $(PACKAGES); do \
	  mkdir -p $(RHOME)/library/$${pkg}; \
	  cp ../library/$${pkg}/DESCRIPTION $(RHOME)/library/$${pkg}; \
	  cp ../library/$${pkg}/INDEX ../library/$${pkg}/TITLE $(RHOME)/library/$${pkg}; \
	  echo "installing R files in $${pkg}"; \
	  mkdir -p $(RHOME)/library/$${pkg}/R; \
	  cat ../library/$${pkg}/R/*.R \
	    > $(RHOME)/library/$${pkg}/R/$${pkg};\
	  if test -d ../library/$${pkg}/R/mac; then \
	    cat ../library/$${pkg}/R/mac/*.R >> $(RHOME)/library/$${pkg}/R/$${pkg};\
	  fi \
	done

datafiles:
	@for pkg in $(DATA); do \
	  echo "installing data files in $${pkg}"; \
	  mkdir -p $(RHOME)/library/$${pkg}/data; \
	  cp ../library/$${pkg}/data/00Index \
	    ../library/$${pkg}/data/*.* $(RHOME)/library/$${pkg}/data; \
	done


help:
	@for pkg in $(PACKAGES); do \
	  PERL5LIB=$(RHOME)/share/perl perl build-help -txt -html -example -latex ../library/$${pkg} $(RHOME)/library; \
	  perl Rd2contents ../library/$${pkg} \
           $(RHOME)/library/$${pkg}/CONTENTS; \
	done
	cat $(RHOME)/library/*/CONTENTS > $(RHOME)/doc/html/search/index.txt
	PERL5LIB=$(RHOME)/share/perl perl build-help  -htmllists

zipfiles:
	(cd $(RHOME)/library/base/help; \
	  zip -jqm Rhelp * -x 00Titles AnIndex)
	(cd $(RHOME)/library/base/R-ex; zip -jqm Rex *.R)
	(cd $(RHOME)/library/base/latex; zip -jqm Rhelp *.tex)

## ------------- html -----------

html:
	cp html/*.html $(RHOME)/doc/html

distribution:
	rm -f dist.zip
	(cd $(RHOME); zip -rq9 src/macintosh/dist library doc/html \
	  src/macintosh/FF*.h src/macintosh/Rconfig.h src/macintosh/Rversion.h \
	  -x doc/html/CVS/* doc/html/search/CVS/*)
