# Makefile for R for Windows Installers

include ../MkRules
RPREFIX = $(shell perl ../fixed/rwver.pl ../../../VERSION)
PACKAGES = base ctest eda lqs methods modreg mva nls splines stepfun tools ts
ALLPACKAGES = $(PACKAGES) tcltk
LIBFILES = $(foreach i, $(ALLPACKAGES), ../../../library/$i) ../../../library/R.css

all: $(RPREFIX).exe

#taken from ../include/Makefile.in
SRC_HEADERS = R.h S.h Rdefines.h Rdevices.h Rgraphics.h Rinternals.h Rmath.h
OBJ_HEADERS = Rconfig.h Rversion.h
GW32_HEADERS =

imagedir:
	$(RM) -r $(RPREFIX)
	$(MKDIR) $(RPREFIX)
#
# the next lines were formerly the bindist target
#
	$(CP) -pr ../../../bin ../../../modules ../../../afm $(RPREFIX)
	$(MKDIR) -p $(RPREFIX)/library
	$(CP) -pr $(LIBFILES) $(RPREFIX)/library
# morley.tab is needed for R-intro Appendix A
	$(CP) -p ../../library/base/data/morley.tab $(RPREFIX)/library/base/data
	$(RM) $(RPREFIX)/*/Makefile.in
	(cd $(RPREFIX)/bin; $(RM) INSTALL REMOVE SHLIB build check \
	   massage-Examples Rd2dvi.sh Rd2txt Rdconv Rdiff.sh Rdindex \
           Sd2Rd)
	$(MKDIR) -p $(RPREFIX)/etc
	$(CP) -p ../../../etc/Rprofile $(RPREFIX)/etc/Rprofile
	$(CP) -p ../../../etc/Rconsole $(RPREFIX)/etc/Rconsole
	$(CP) -p ../../../etc/Rdevga $(RPREFIX)/etc/Rdevga
	$(CP) -p ../../../etc/rgb.txt $(RPREFIX)/etc/rgb.txt
	$(MKDIR) -p $(RPREFIX)/doc/manual  $(RPREFIX)/share/texmf
	$(CP) -pr ../../../doc/html $(RPREFIX)/doc
	$(CP) -p ../../../doc/manual/*.html $(RPREFIX)/doc/manual
	$(CP) -pr ../../../share/texmf $(RPREFIX)/share
	$(CP) -p ../../../AUTHORS ../../../COPYING ../COPYRIGHTS ../../../FAQ ../../../NEWS \
		../../../README ../../../RESOURCES ../../../THANKS ../../../Y2K $(RPREFIX)
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
#
# the next lines were formerly the docsdist target
#
	$(MKDIR) -p $(RPREFIX)/doc/manual
	$(CP) -pr ../../../doc/manual/*.pdf $(RPREFIX)/doc/manual
	$(RM) $(RPREFIX)/doc/manual/R-FAQ.pdf
#
# the next lines were formerly the packagedist target
#
	$(MKDIR) -p $(RPREFIX)/bin $(RPREFIX)/etc \
	  $(RPREFIX)/doc $(RPREFIX)/doc/html $(RPREFIX)/share \
	  $(RPREFIX)/src/include $(RPREFIX)/src/library \
	  $(RPREFIX)/src/gnuwin32/fixed
	$(CP) ../../../doc/html/logo.jpg $(RPREFIX)/doc/html
	$(CP) ../../../doc/KEYWORDS* $(RPREFIX)/doc
	$(CP) -p ../exported-vars ../R.exp ../Rblas.def ../MkRules ../MakePkg ../MakeDll \
	  ../Rpwd.exe ../pwd.pl ../makeDllRes.pl ../install-save.sh $(RPREFIX)/src/gnuwin32
	$(SED) -e s/@RVER@/$(RVER)/g ../Makefile.packages > $(RPREFIX)/src/gnuwin32/Makefile
	$(CP) ../fixed/dobundle.pl $(RPREFIX)/src/gnuwin32/fixed
	$(CP) -pr ../help ../check $(RPREFIX)/src/gnuwin32
	$(RM) $(RPREFIX)/src/gnuwin32/help/*.c \
	  $(RPREFIX)/src/gnuwin32/help/MkChmDll
	(cd ..; $(MAKE) --no-print-directory -C installer/$(RPREFIX)/src/gnuwin32/help clean)
	$(CP) -p $(foreach i,$(SRC_HEADERS) $(OBJ_HEADERS) $(GW32_HEADERS), \
	  ../../include/$i) $(RPREFIX)/src/include
	$(CP) -pr ../../include/R_ext $(RPREFIX)/src/include
	$(SED) -e 3,5d ../readme.packages > $(RPREFIX)/readme.packages
	$(CP) ../../../COPYING.LIB $(RPREFIX)
	$(CP) -r ../../../share/make ../../../share/perl ../../../share/texmf \
	   $(RPREFIX)/share
	$(CP) ../../../bin/INSTALL ../../../bin/REMOVE ../../../bin/SHLIB \
	  ../../../bin/build ../../../bin/check ../../../bin/massage-Examples \
	  ../../../bin/Rd2dvi.sh ../../../bin/Rd2txt ../../../bin/Rdconv \
	  ../../../bin/Rdindex ../../../bin/Rdiff.sh \
	  ../../../bin/Sd2Rd  \
          $(RPREFIX)/bin
	$(CP) -pr ../../../lib $(RPREFIX)
	$(CP) ../../../share/make/wintests.mk $(RPREFIX)/share/make
	$(CP) -pr ../../../share/R $(RPREFIX)/share
	$(CP) -p ../graphapp/graphapp.h ../graphapp/ga.h $(RPREFIX)/src/include
	(cd ..; $(MAKE) --no-print-directory PKGDIR=. pkgclean-windlgs)
	$(CP) -pr ../windlgs $(RPREFIX)/src/library
#
# the next lines were formerly the dist-recommended target
#
	@for p in $(REC_PKGS); do \
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
	echo "make.packages.html(.Library)" | $(RPREFIX)/bin/rterm --vanilla --slave
	$(CP) -pr ../../../Tcl $(RPREFIX)
	perl JRins.pl $(RPREFIX) 

$(RPREFIX).exe: R.iss
	"$(ISDIR)/iscc" R.iss > $(RPREFIX).log
	"$(ISDIR)/iscc" Rsmall.iss > miniR.log

clean:
	rm -rf R.iss Rsmall.iss *.log $(RPREFIX) 

distclean: clean
	rm -f $(RPREFIX).exe miniR*

# for historical reasons
ISclean: clean
ISdistclean: distclean
