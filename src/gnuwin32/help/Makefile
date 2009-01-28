## Only ever called from src/library/Makefile

PERL = PERL5LIB=$(RHOME)/share/perl R_OSTYPE=windows perl
CAT = cat
CP = cp
RM = rm
MKDIR = mkdir

## We pass RHOME in
PKGDIR = ../../library
RLIB = $(RHOME)/library

BUILD_HELP=R_HOME=$(RHOME) $(PERL) $(RHOME)/share/perl/build-help.pl -os=windows

## HELPTYPES is set in src/library/Makefile
help-%:
	@if test -d $(PKGDIR)/$*/man ; then \
	   $(BUILD_HELP) $(HELPTYPES) $(PKGDIR)/$* $(RLIB) $(RLIB)/$* $* ; \
	fi

chm-%: ziponlyhelp-%
	@if test -d $(PKGDIR)/$*/man ; then \
	  if ! test -d $(PKGDIR)/$*/chm ; then \
	    $(BUILD_HELP) -chm $(PKGDIR)/$* $(RLIB) '$(DPKG)' $* ; \
	  fi; \
	  $(MAKE) --no-print-directory -C $(PKGDIR)/$*/chm \
	    -f $(RHOME)/src/gnuwin32/help/Makefile $*.chm ; \
	  if test -f $(PKGDIR)/$*/chm/$*.chm ; then \
	    $(MKDIR) -p $(RLIB)/$*/chtml ;\
	    $(CP) $(PKGDIR)/$*/chm/$*.chm $(RLIB)/$*/chtml ; \
	  else \
	    echo CHM compile failed:  HTML Help Workshop not installed? ; \
	  fi; \
	fi

ziponlyhelp-%:
	@$(MAKE) --no-print-directory -C $(RLIB)/$* \
	  -f $(RHOME)/src/gnuwin32/help/Makefile ZIPFLAGS=jqmX PKG=$* zipit

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

## FIXME combine these?
zipit: 
	-@$(MAKE) --no-print-directory -f $(RHOME)/src/gnuwin32/help/Makefile help/Rhelp.zip
	-@$(MAKE) --no-print-directory -f $(RHOME)/src/gnuwin32/help/Makefile zipup

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
