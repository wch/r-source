# default target builds indices
DOS=# -dosnames   # for file-name mapping
PERL=R_OSTYPE=windows perl
CAT=cat
CP=cp
RM=rm
MKDIR=mkdir

R_HOME = ../../..
RHOME=$(shell perl ../pwd.pl ../../..)
PKGDIR=../../library
RLIB=$(RHOME)/library
CHM=

index-%:
	@if test -d $(PKGDIR)/$*/man ; then \
	  PERL5LIB=$(RHOME)/share/perl $(PERL) \
	    $(R_HOME)/share/perl/build-help-windows.pl ${DOS} \
	    -index $(PKGDIR)/$* $(RLIB) '$(DPKG)' $* ; \
	fi

pkg-% help-%:
	@if test -d $(PKGDIR)/$*/man ; then \
	  PERL5LIB=$(RHOME)/share/perl $(PERL) \
	    $(R_HOME)/share/perl/build-help-windows.pl ${DOS} \
	    $(HELPTYPES) $(PKGDIR)/$* $(RLIB) '$(DPKG)' $* ; \
	fi

pkgfake-%:
	@if test -d $(PKGDIR)/$*/man ; then \
	  PERL5LIB=$(RHOME)/share/perl $(PERL) \
	    $(R_HOME)/share/perl/build-help-windows.pl ${DOS} \
	    -txt -latex $(PKGDIR)/$* $(RLIB) '$(DPKG)' $* ; \
	fi

chm-%:
	@if test -d $(PKGDIR)/$*/man ; then \
	  if ! test -d $(PKGDIR)/$*/chm ; then \
	    PERL5LIB=$(RHOME)/share/perl $(PERL) \
	    $(R_HOME)/share/perl/build-help-windows.pl ${DOS} \
	    -chm $(PKGDIR)/$* $(RLIB) '$(DPKG)' $* ; \
	  fi; \
	  $(MAKE)  --no-print-directory -C $(PKGDIR)/$*/chm \
	    -f $(RHOME)/src/gnuwin32/help/Makefile $*.chm ; \
	  $(MKDIR) -p $(DPKG)/chtml ;\
	  $(CP) $(PKGDIR)/$*/chm/$*.chm $(DPKG)/chtml ; \
	fi

ziphelp-%:
	@$(MAKE) --no-print-directory -C $(RLIB)/$* \
	  -f $(RHOME)/src/gnuwin32/help/Makefile ZIPFLAGS=jqX  PKG=$* zipit

ziponlyhelp-%:
	@$(MAKE) --no-print-directory -C $(RLIB)/$* \
	  -f $(RHOME)/src/gnuwin32/help/Makefile ZIPFLAGS=jqmX PKG=$* zipit


latex-%:
	@$(MAKE) --no-print-directory -C $(RLIB)/$* \
	-f $(RHOME)/src/gnuwin32/help/Makefile PKG=$* unzipit
	PERL5LIB=$(RHOME)/share/perl $(PERL) \
	  $(R_HOME)/share/perl/build-help-windows.pl ${DOS} \
	  -latex $(PKGDIR)/$* $(RLIB)

clean:
	$(RM) -fr *~ *.o *.obj *.dll *.def *.a

# to be run from src/library/pkg/chm

HTM = $(filter-out 00Index.html, $(wildcard *.html))
%.chm: $(HTM) Rchm.css logo.jpg
	@hhc $*.hhp || exit 0

Rchm.css: $(RHOME)/src/gnuwin32/help/Rchm.css
	@$(CP) $^ $@

logo.jpg: $(RHOME)/doc/html/logo.jpg
	@$(CP) $^ $@

# following are designed to be run from the library/pkg directory:

zipit: 
	-@$(MAKE) --no-print-directory -f $(RHOME)/src/gnuwin32/help/Makefile help/Rhelp.zip
	-@$(MAKE) --no-print-directory -f $(RHOME)/src/gnuwin32/help/Makefile zipup
unzipit:
	-@$(MAKE) --no-print-directory -C latex \
	  -f $(RHOME)/src/gnuwin32/help/Makefile uz-Rhelp

help/Rhelp.zip: $(wildcard ../../src/library/$(PKG)/man/*.Rd \
../../src/library/$(PKG)/man/windows/*.Rd)
	-@if test -d help ; then \
	  $(MAKE) --no-print-directory -C help  \
            -f $(RHOME)/src/gnuwin32/help/Makefile uz-Rhelp; \
	fi
	-@if test -d R-ex ; then \
	  $(MAKE) --no-print-directory -C R-ex \
	    -f $(RHOME)/src/gnuwin32/help/Makefile uz-Rex; \
	fi
	-@if test -d latex ; then \
	  $(MAKE) --no-print-directory -C latex \
	    -f $(RHOME)/src/gnuwin32/help/Makefile uz-Rhelp; \
	fi
	-@$(MAKE) --no-print-directory -C $(RHOME)/src/gnuwin32/help help-$(PKG)
	@$(RM) -f help/Rhelp.zip latex/Rhelp.zip R-ex/Rex.zip

zipup:
	@echo "  zipping help files"
	@zip -${ZIPFLAGS} help/Rhelp help/* -x help/AnIndex 2> /dev/null || exit 0
	@zip -${ZIPFLAGS} R-ex/Rex R-ex/*.R 2> /dev/null || exit 0
	@zip -${ZIPFLAGS} latex/Rhelp latex/*.tex 2> /dev/null || exit 0

uz-%:
	@if test -f $*.zip ; then \
	  unzip -qou $*; \
	fi
