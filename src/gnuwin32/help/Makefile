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

chm-%: 
	$(PERL) build.help ${DOS} -chm $*
# hhc always gives an error code
	$(MAKE) -C ../../library/$*/chm -f ../../../gnuwin32/help/Makefile $*.chm
	mkdir -p ../../../library/$*/chtml
	cp ../../library/$*/chm/$*.chm ../../../library/$*/chtml

winhlp-%: chm-%
	$(MAKE) -C ../../library/$*/chm -f ../../../gnuwin32/help/Makefile $*.hlp
	mkdir -p ../../../library/$*/winhlp
	cp ../../library/$*/chm/$*.cnt ../../library/$*/chm/$*.hlp ../../../library/$*/winhlp

contents-%:
	$(PERL) Rd2contents ../../library/$* ../../../library/$*/CONTENTS
	$(CAT) ../../../library/*/CONTENTS > ../../../doc/html/search/index.txt

pkg-%:
	$(PERL) build.help ${DOS} -nroff -html -example -latex ../../library/$*

ziphelp-%:
	$(MAKE) -C ../../../library/$* -f ../../src/gnuwin32/help/Makefile ZIPFLAGS=jqX  PKG=$* zipit

ziponlyhelp-%:
	$(MAKE) -C ../../../library/$* -f ../../src/gnuwin32/help/Makefile ZIPFLAGS=jqmX PKG=$* zipit


winhelp-%:
	$(MAKE) -C ../../library/$* -f ../../gnuwin32/help/Makefile PKG=$* win

latex-%:
	$(MAKE) -C ../../../library/$* -f ../../src/gnuwin32/help/Makefile PKG=$* unzipit
	$(PERL) build.help ${DOS} -latex ../../library/$*

clean:
	$(RM) -fr *~ *.o *.obj *.dll *.def *.a

# to be run from src/library/pkg/chm

HTM = $(filter-out 00Index.html, $(wildcard *.html))
%.chm: $(HTM) logo.jpg
	-hhc $*.hhp

%.hlp: %.rtf logo.bmp
	 hcrtf /x $*.hpj

%.rtf: $(HTM)
	perl ../../../gnuwin32/help/html-to-rtf.pl $*.hhp

logo.jpg: ../../../../doc/html/logo.jpg
	$(CP) $^ $@

logo.bmp: ../../../gnuwin32/help/logo.bmp
	$(CP) $^ $@

# following are designed to be run from the library/pkg directory:

zipit: help/Rhelp.zip
unzipit:
	-$(MAKE) -C latex -f ../../../src/gnuwin32/help/Makefile uz-Rhelp

help/Rhelp.zip: $(wildcard ../../src/library/$(PKG)/man/*.Rd \
../../src/library/$(PKG)/man/windows/*.Rd)
	-$(MAKE) -C help  -f ../../../src/gnuwin32/help/Makefile uz-Rhelp
	-$(MAKE) -C R-ex  -f ../../../src/gnuwin32/help/Makefile uz-Rex
	-$(MAKE) -C latex -f ../../../src/gnuwin32/help/Makefile uz-Rhelp
	-$(MAKE) -C ../../src/gnuwin32/help help-$(PKG)
	zip -${ZIPFLAGS} help/Rhelp help/* -x help/00Titles help/AnIndex
	zip -${ZIPFLAGS} R-ex/Rex R-ex/*.R
	zip -${ZIPFLAGS} latex/Rhelp latex/*.tex

uz-%:
	unzip -qou $*
