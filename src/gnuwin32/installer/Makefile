# Makefile for R for Windows Installers

include ../MkRules
include ../../../share/make/vars.mk

RPREFIX = $(shell perl ../fixed/rwver.pl ../../../VERSION)
LIBFILES = $(foreach i, $(R_PKGS_BASE), ../../../library/$i) ../../../library/R.css

INFO_FILES = AUTHORS COPYING FAQ NEWS README RESOURCES THANKS Y2K

all: $(RPREFIX).exe

imagedir:
	$(RM) -r $(RPREFIX)
	$(MKDIR) $(RPREFIX)
	$(CP) -pr ../../../bin ../../../modules ../../../afm $(RPREFIX)
	$(MKDIR) -p $(RPREFIX)/library
	$(CP) -pr $(LIBFILES) $(RPREFIX)/library
# morley.tab is needed for R-intro Appendix A
	$(CP) -p ../../library/base/data/morley.tab $(RPREFIX)/library/base/data
	$(RM) $(RPREFIX)/*/Makefile.in
	(cd $(RPREFIX)/bin; $(RM) INSTALL REMOVE SHLIB build check \
	   massage-Examples Rd2dvi.sh Rd2txt Rdconv Rdiff.sh Sd2Rd)
	$(MKDIR) -p $(RPREFIX)/etc
	$(CP) -p ../../../etc/Rprofile $(RPREFIX)/etc/Rprofile
	$(CP) -p ../../../etc/Rconsole $(RPREFIX)/etc/Rconsole
	$(CP) -p ../../../etc/Rdevga $(RPREFIX)/etc/Rdevga
	$(CP) -p ../../../etc/rgb.txt $(RPREFIX)/etc/rgb.txt
	$(MKDIR) -p $(RPREFIX)/doc/manual  $(RPREFIX)/share/texmf
	$(CP) -pr ../../../doc/html $(RPREFIX)/doc
	$(CP) -p ../../../doc/manual/*.html $(RPREFIX)/doc/manual
	$(CP) -pr ../../../share/texmf $(RPREFIX)/share
	$(CP) -p ../COPYRIGHTS $(RPREFIX)
	$(CP) -p $(foreach i,$(INFO_FILES),../../../$i) $(RPREFIX)
	$(SED) -e s/@RWVER@/$(RPREFIX)/g \
	  -e s/@RVER@/$(RVER)/g ../readme > $(RPREFIX)/README.$(RPREFIX)
	$(CP) -p ../rw-faq $(RPREFIX)/rw-FAQ
	$(CP) -p ../CHANGES ../README.Rterm $(RPREFIX)
# convert docs and batch files to CRLF: might already be on Windows, though.
ifeq ($(strip $(BUILD)),CROSS)
	zip -l dosfiles.zip $(RPREFIX)/* $(RPREFIX)/bin/*.bat
	unzip -ao dosfiles.zip
else
	zip dosfiles.zip $(RPREFIX)/* $(RPREFIX)/bin/*.bat
	unzip -o dosfiles.zip
endif
	$(RM) dosfiles.zip
	$(RM) $(RPREFIX)/doc/manual/R-FAQ.html
	$(MKDIR) -p $(RPREFIX)/doc/manual
	$(CP) -pr ../../../doc/manual/*.pdf $(RPREFIX)/doc/manual
	$(RM) $(RPREFIX)/doc/manual/R-FAQ.pdf
	$(MKDIR) -p $(RPREFIX)/bin $(RPREFIX)/etc \
	  $(RPREFIX)/doc $(RPREFIX)/doc/html $(RPREFIX)/share \
	  $(RPREFIX)/include $(RPREFIX)/src/library \
	  $(RPREFIX)/src/gnuwin32/fixed
	$(CP) ../../../doc/html/logo.jpg $(RPREFIX)/doc/html
	$(CP) ../../../doc/KEYWORDS* $(RPREFIX)/doc
	$(CP) -p ../exported-vars ../R.exp ../Rblas.def ../MkRules ../MakePkg ../MakeDll \
	  ../Rpwd.exe ../pwd.pl ../makeDllRes.pl ../install-save.sh ../man-install.sh $(RPREFIX)/src/gnuwin32
	$(SED) -e s/@RVER@/$(RVER)/g ../Makefile.packages > $(RPREFIX)/src/gnuwin32/Makefile
	$(CP) ../fixed/dobundle.pl $(RPREFIX)/src/gnuwin32/fixed
	$(CP) -pr ../help ../check $(RPREFIX)/src/gnuwin32
	$(RM) $(RPREFIX)/src/gnuwin32/help/*.c \
	  $(RPREFIX)/src/gnuwin32/help/MkChmDll
	(cd ..; $(MAKE) --no-print-directory -C installer/$(RPREFIX)/src/gnuwin32/help clean)
	$(CP) -pr ../../../include $(RPREFIX)
	$(SED) -e 3,5d ../readme.packages > $(RPREFIX)/readme.packages
	$(CP) ../../../COPYING.LIB $(RPREFIX)
	$(CP) -r ../../../share/make ../../../share/perl ../../../share/texmf \
	   $(RPREFIX)/share
	$(CP) ../../../bin/INSTALL ../../../bin/REMOVE ../../../bin/SHLIB \
	  ../../../bin/build ../../../bin/check ../../../bin/massage-Examples \
	  ../../../bin/Rd2dvi.sh ../../../bin/Rd2txt ../../../bin/Rdconv \
	  ../../../bin/Rdiff.sh ../../../bin/Sd2Rd  \
          $(RPREFIX)/bin
	$(CP) -pr ../../../lib $(RPREFIX)
	$(CP) ../../../share/make/wintests.mk $(RPREFIX)/share/make
	$(CP) -pr ../../../share/R $(RPREFIX)/share
	$(CP) -p ../graphapp/graphapp.h ../graphapp/ga.h $(RPREFIX)/include
	(cd ..; $(MAKE) --no-print-directory PKGDIR=. pkgclean-windlgs)
	$(CP) -pr ../windlgs $(RPREFIX)/src/library
	@for p in $(R_PKGS_RECOMMENDED); do \
	echo Copying $$p ; \
	$(MKDIR) -p $(RPREFIX)/library/$$p ; \
	$(CP) -pr ../../../library/$$p $(RPREFIX)/library ; \
	done	
#
	find $(RPREFIX) -name CVS -prune -exec rm -rf \{\} \;

R.iss Rsmall.iss: JRins.pl imagedir
	rm -rf $(RPREFIX)/library/*/man
	$(CAT) $(RPREFIX)/library/*/CONTENTS >  \
	  $(RPREFIX)/doc/html/search/index.txt
	echo "make.packages.html(.Library)" | $(RPREFIX)/bin/rterm --vanilla --slave LC_COLLATE=C
	$(CP) -pr ../../../Tcl $(RPREFIX)
	perl JRins.pl $(RPREFIX) 

$(RPREFIX).exe: R.iss
	"$(ISDIR)/iscc" R.iss > $(RPREFIX).log
	"$(ISDIR)/iscc" Rsmall.iss > miniR.log

clean:
	rm -rf R.iss Rsmall.iss *.log $(RPREFIX) 

distclean: clean
	rm -f $(RPREFIX).exe miniR*
