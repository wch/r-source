# default target builds indices
DOS= # -dosnames   # for file-name mapping
PERL=perl
CAT=cat
CP=cp
RM=rm

RHOME=$(shell cd ../../..; perl src/gnuwin32/pwd.pl)# must be absolute path
PKGDIR=../../library
RLIB=$(RHOME)/library

indices:
	$(PERL) build.help ${DOS} -htmllists

pkg-% help-%: 
	$(PERL) build.help ${DOS} -txt -html -example -latex $(PKGDIR)/$* $(RLIB)

chm-%: 
	$(PERL) build.help ${DOS} -chm $(PKGDIR)/$* $(RLIB)
# hhc always gives an error code
	$(MAKE) -C $(PKGDIR)/$*/chm -f $(RHOME)/src/gnuwin32/help/Makefile $*.chm
	mkdir -p $(RLIB)/$*/chtml
	$(CP) $(PKGDIR)/$*/chm/$*.chm $(RLIB)/$*/chtml

winhlp-%: chm-%
	$(MAKE) -C $(PKGDIR)/$*/chm -f $(RHOME)/src/gnuwin32/help/Makefile $*.hlp
	mkdir -p $(RLIB)/$*/winhlp
	$(CP) $(PKGDIR)/$*/chm/$*.cnt $(PKGDIR)/$*/chm/$*.hlp $(RLIB)/$*/winhlp

# need not have HTML installed when building packages
contents-%:
	$(PERL) Rd2contents $(PKGDIR)/$* $(RLIB)/$*/CONTENTS
	@mkdir -p $(RHOME)/doc/html/search
	$(CAT) $(RHOME)/library/*/CONTENTS > $(RHOME)/doc/html/search/index.txt
	$(PERL) build.help ${DOS} -htmllists

ziphelp-%:
	$(MAKE) -C $(RLIB)/$* -f $(RHOME)/src/gnuwin32/help/Makefile ZIPFLAGS=jqX  PKG=$* zipit

ziponlyhelp-%:
	$(MAKE) -C $(RLIB)/$* -f $(RHOME)/src/gnuwin32/help/Makefile ZIPFLAGS=jqmX PKG=$* zipit


winhelp-%:
	$(MAKE) -C $(PKGDIR)/$* -f $(RHOME)/src/gnuwin32/help/Makefile PKG=$* win

latex-%:
	$(MAKE) -C $(RLIB)/$* -f $(RHOME)/src/gnuwin32/help/Makefile PKG=$* unzipit
	$(PERL) build.help ${DOS} -latex $(PKGDIR)/$*

clean:
	$(RM) -fr *~ *.o *.obj *.dll *.def *.a

# to be run from src/library/pkg/chm

HTM = $(filter-out 00Index.html, $(wildcard *.html))
%.chm: $(HTM) logo.jpg
	-hhc $*.hhp

%.hlp: %.rtf logo.bmp
	hcrtf /x $*.hpj

%.rtf: $(HTM)
	perl $(RHOME)/src/gnuwin32/help/html-to-rtf.pl $*.hhp

logo.jpg: $(RHOME)/doc/html/logo.jpg
	$(CP) $^ $@

logo.bmp: $(RHOME)/src/gnuwin32/help/logo.bmp
	$(CP) $^ $@

# following are designed to be run from the library/pkg directory:

zipit: help/Rhelp.zip zipup
unzipit:
	-@$(MAKE) -C latex -f $(RHOME)/src/gnuwin32/help/Makefile uz-Rhelp

help/Rhelp.zip: $(wildcard ../../src/library/$(PKG)/man/*.Rd \
../../src/library/$(PKG)/man/windows/*.Rd)
	-@$(MAKE) -C help  -f $(RHOME)/src/gnuwin32/help/Makefile uz-Rhelp
	-@$(MAKE) -C R-ex  -f $(RHOME)/src/gnuwin32/help/Makefile uz-Rex
	-@$(MAKE) -C latex -f $(RHOME)/src/gnuwin32/help/Makefile uz-Rhelp
	-$(MAKE) -C $(RHOME)/src/gnuwin32/help help-$(PKG)
	@$(RM) -f help/Rhelp.zip latex/Rhelp.zip R-ex/Rex.zip

zipup:
	-zip -${ZIPFLAGS} help/Rhelp help/* -x help/00Titles help/AnIndex 2> nul
	-zip -${ZIPFLAGS} R-ex/Rex R-ex/*.R 2> nul
	-zip -${ZIPFLAGS} latex/Rhelp latex/*.tex 2> nul

uz-%:
	@unzip -qou $*
