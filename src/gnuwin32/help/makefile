# default target builds indices
DOS= # -dosnames   # for file-name mapping
PERL=perl
CAT=cat
CP=cp
RM=rm

indices:
	$(PERL) build.help ${DOS} -htmllists

help-%: 
	$(PERL) build.help ${DOS} -nroff -html -example -latex $*

contents-%:
	$(PERL) Rd2contents ../../library/$* ../../../library/$*/CONTENTS
	$(CAT) ../../../library/*/CONTENTS > ../../../doc/html/search/index.txt

pkg-%:
	$(PERL) build.help ${DOS} -nroff -html -example -latex ../../library/$*

ziphelp-%:
	$(MAKE) -C ../../../library/$* -f ../../src/gnuwin32/help/makefile ZIPFLAGS=jqX  PKG=$* zipit

ziponlyhelp-%:
	$(MAKE) -C ../../../library/$* -f ../../src/gnuwin32/help/makefile ZIPFLAGS=jqmX PKG=$* zipit


winhelp-%:
	$(MAKE) -C ../../library/$* -f ../../gnuwin32/help/makefile PKG=$* win

clean:
	$(RM) -fr *~

# to be run from src/library/pkg

win: $(PKG).rtf
	-mkdir ../../../library/$(PKG)/winhlp
	$(CP) -p $(PKG).hlp $(PKG).cnt ../../../library/$(PKG)/winhlp

%.rtf: man/*.Rd
	-$(RM) -f $*.rtf
	$(PERL) ../../gnuwin32/help/Rd2Sd.pl -p $* man/*.Rd
	$(PERL) -S nr2hlp $* $*.Sd
	hcp505 $*
	$(RM) -f $*.ph $*.hpj $*.rtf


# following are designed to be run from the library/pkg directory:

zipit: help/Rhelp.zip

help/Rhelp.zip: ../../src/library/$(PKG)/man/*.Rd
	-$(MAKE) -C help  -f ../../../src/gnuwin32/help/makefile uz-Rhelp
	-$(MAKE) -C R-ex  -f ../../../src/gnuwin32/help/makefile uz-Rex
	-$(MAKE) -C latex -f ../../../src/gnuwin32/help/makefile uz-Rhelp
	-$(MAKE) -C ../../src/gnuwin32/help help-$(PKG)
	zip -${ZIPFLAGS} help/Rhelp help/* -x help/00Titles help/AnIndex
	zip -${ZIPFLAGS} R-ex/Rex R-ex/*.R
	zip -${ZIPFLAGS} latex/Rhelp latex/*.tex

uz-%:
	unzip -qou $*
