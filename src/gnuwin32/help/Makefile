# default target builds indices
DOS=# -dosnames   # for file-name mapping
PERL=perl
CAT=cat
CP=cp
RM=rm
MKDIR=mkdir

RHOME=$(shell perl ../pwd.pl ../../..)
PKGDIR=../../library
RLIB=$(RHOME)/library

indices:
	PERL5LIB=$(RHOME)/share/perl $(PERL) build.help ${DOS} -htmllists

pkg-% help-%:
	PERL5LIB=$(RHOME)/share/perl $(PERL) build.help ${DOS} -txt -html -example -latex $(PKGDIR)/$* $(RLIB)

chm-%:
	PERL5LIB=$(RHOME)/share/perl $(PERL) build.help ${DOS} -chm $(PKGDIR)/$* $(RLIB)
# hhc always gives an error code
	$(MAKE)  --no-print-directory -C $(PKGDIR)/$*/chm -f $(RHOME)/src/gnuwin32/help/Makefile $*.chm
	$(MKDIR) -p $(RLIB)/$*/chtml
	$(CP) $(PKGDIR)/$*/chm/$*.chm $(RLIB)/$*/chtml

winhlp-%: chm-%
	$(MAKE) --no-print-directory -C $(PKGDIR)/$*/chm -f $(RHOME)/src/gnuwin32/help/Makefile $*.hlp
	$(MKDIR) -p $(RLIB)/$*/winhlp
	$(CP) $(PKGDIR)/$*/chm/$*.cnt $(PKGDIR)/$*/chm/$*.hlp $(RLIB)/$*/winhlp

# need not have HTML installed when building packages
contents-%:
	$(PERL) Rd2contents $(PKGDIR)/$* $(RLIB)/$*/CONTENTS
	@$(MKDIR) -p $(RHOME)/doc/html/search
	$(CAT) $(RHOME)/library/*/CONTENTS > $(RHOME)/doc/html/search/index.txt
	PERL5LIB=$(RHOME)/share/perl $(PERL) build.help ${DOS} -htmllists

ziphelp-%:
	@$(MAKE) --no-print-directory -C $(RLIB)/$* -f $(RHOME)/src/gnuwin32/help/Makefile ZIPFLAGS=jqX  PKG=$* zipit

ziponlyhelp-%:
	@$(MAKE) --no-print-directory -C $(RLIB)/$* -f $(RHOME)/src/gnuwin32/help/Makefile ZIPFLAGS=jqmX PKG=$* zipit


winhelp-%:
	$(MAKE) --no-print-directory -C $(PKGDIR)/$* -f $(RHOME)/src/gnuwin32/help/Makefile PKG=$* win

latex-%:
	@$(MAKE) --no-print-directory -C $(RLIB)/$* -f $(RHOME)/src/gnuwin32/help/Makefile PKG=$* unzipit
	PERL5LIB=$(RHOME)/share/perl $(PERL) build.help ${DOS} -latex $(PKGDIR)/$*

clean:
	$(RM) -fr *~ *.o *.obj *.dll *.def *.a

# to be run from src/library/pkg/chm

HTM = $(filter-out 00Index.html, $(wildcard *.html))
%.chm: $(HTM) Rchm.css logo.jpg
	-hhc $*.hhp

%.hlp: %.rtf logo.bmp
	hcrtf /x $*.hpj

%.rtf: $(HTM)
	perl $(RHOME)/share/perl/hhw-to-rtf.pl $*.hhp

Rchm.css: $(RHOME)/src/gnuwin32/help/Rchm.css
	$(CP) $^ $@

logo.jpg: $(RHOME)/doc/html/logo.jpg
	$(CP) $^ $@

logo.bmp: $(RHOME)/src/gnuwin32/help/logo.bmp
	$(CP) $^ $@

# following are designed to be run from the library/pkg directory:

zipit: help/Rhelp.zip zipup
unzipit:
	-@$(MAKE) --no-print-directory -C latex -f $(RHOME)/src/gnuwin32/help/Makefile uz-Rhelp

help/Rhelp.zip: $(wildcard ../../src/library/$(PKG)/man/*.Rd \
../../src/library/$(PKG)/man/windows/*.Rd)
	-@$(MAKE) --no-print-directory -C help  -f $(RHOME)/src/gnuwin32/help/Makefile uz-Rhelp
	-@$(MAKE) --no-print-directory -C R-ex  -f $(RHOME)/src/gnuwin32/help/Makefile uz-Rex
	-@$(MAKE) --no-print-directory -C latex -f $(RHOME)/src/gnuwin32/help/Makefile uz-Rhelp
	-$(MAKE) --no-print-directory -C $(RHOME)/src/gnuwin32/help help-$(PKG)
	@$(RM) -f help/Rhelp.zip latex/Rhelp.zip R-ex/Rex.zip

zipup:
	-zip -${ZIPFLAGS} help/Rhelp help/* -x help/AnIndex 2> nul
	-zip -${ZIPFLAGS} R-ex/Rex R-ex/*.R 2> nul
	-zip -${ZIPFLAGS} latex/Rhelp latex/*.tex 2> nul

uz-%:
	@if test -f $*.zip ; then \
	  unzip -qou $*; \
	fi
