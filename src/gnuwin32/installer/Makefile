# Makefile for R for Windows Installers

include ../MkRules
R_HOME = ../../..
include $(R_HOME)/share/make/vars.mk

## use next to install other packages from main library
EXTRA_PKGS =

RPREFIX = $(shell perl ../fixed/rwver.pl $(R_HOME)/VERSION)
LIBFILES = $(foreach i, $(R_PKGS_BASE) $(R_PKGS_RECOMMENDED) $(EXTRA_PKGS), $(R_HOME)/library/$i) $(R_HOME)/library/R.css

INFO_FILES = AUTHORS COPYING COPYING.LIB FAQ NEWS ONEWS README RESOURCES \
	THANKS Y2K
GW_INFO_FILES = CHANGES CHANGES1 COPYRIGHTS README.Rterm rw-FAQ

ETC_FILES = Rprofile Rconsole Rdevga rgb.txt

GW32_FILES = R.exp Rblas.def MkRules MakePkg MakeDll \
	  Rpwd.exe pwd.pl makeDllRes.pl install-save.sh man-install.sh

# we don't want share/{Makefile.in,glade,sh} installed
SHARE_DIRS = R make perl texmf licenses

all: $(RPREFIX).exe miniR.exe

imagedir:
	$(RM) -r $(RPREFIX)
	$(MKDIR) $(RPREFIX)
	$(CP) -pr $(R_HOME)/bin $(R_HOME)/modules $(R_HOME)/afm $(RPREFIX)
# copy base and recommended packages
	$(MKDIR) -p $(RPREFIX)/library
	$(CP) -pr $(LIBFILES) $(RPREFIX)/library
	$(RM) $(RPREFIX)/library/*/MD5
# morley.tab is needed for R-intro Appendix A
	$(CP) -p $(R_HOME)/src/library/datasets/data/morley.tab $(RPREFIX)/library/datasets/data
	$(RM) $(RPREFIX)/*/Makefile.in
	$(MKDIR) -p $(RPREFIX)/etc
	$(CP) -p $(foreach i,$(ETC_FILES),$(R_HOME)/etc/$i) $(RPREFIX)/etc
# copy info and documentation files
	$(MKDIR) -p $(RPREFIX)/doc
	$(CP) -p $(R_HOME)/doc/CRAN_mirrors.csv $(RPREFIX)/doc
	$(MKDIR) -p $(RPREFIX)/doc/manual
	$(CP) -pr $(R_HOME)/doc/html $(RPREFIX)/doc
	$(CP) -p $(R_HOME)/doc/manual/*.html $(R_HOME)/doc/manual/*.pdf \
	  $(RPREFIX)/doc/manual
#	html copies of these are in doc/html, and we don't have PDFs on menus
	$(RM) $(RPREFIX)/doc/manual/R-FAQ.pdf
	$(RM) $(RPREFIX)/doc/html/R-admin.html $(RPREFIX)/doc/manual/R-admin.pdf
	$(RM) $(RPREFIX)/doc/html/index.html $(RM) $(RPREFIX)/doc/html/index-default.html
#
	$(CP) -p $(foreach i,$(INFO_FILES),$(R_HOME)/$i) $(RPREFIX)
	$(CP) -p $(foreach i,$(GW_INFO_FILES),../$i) $(RPREFIX)
	$(SED) -e s/@RWVER@/$(RPREFIX)/g \
	  -e s/@RVER@/$(RVER)/g ../README > $(RPREFIX)/README.$(RPREFIX)
	$(SED) -e 3,5d ../README.packages > $(RPREFIX)/README.packages
# convert docs and batch files to CRLF: might already be on Windows, though.
ifeq ($(strip $(BUILD)),CROSS)
	zip -l dosfiles.zip $(RPREFIX)/* $(RPREFIX)/bin/*.bat
	unzip -o dosfiles.zip
else
	zip dosfiles.zip $(RPREFIX)/* $(RPREFIX)/bin/*.bat
	unzip -ao dosfiles.zip
endif
	$(RM) dosfiles.zip
#
# files to make source packages
	$(MKDIR) -p $(RPREFIX)/share \
	  $(RPREFIX)/src/library $(RPREFIX)/src/gnuwin32/fixed
	$(CP) $(R_HOME)/doc/html/logo.jpg $(RPREFIX)/doc/html
	$(CP) $(R_HOME)/doc/KEYWORDS* $(RPREFIX)/doc
	$(CP) -p $(foreach i,$(GW32_FILES),../$i) $(RPREFIX)/src/gnuwin32
	$(SED) -e s/@RVER@/$(RVER)/g ../Makefile.packages > $(RPREFIX)/src/gnuwin32/Makefile
	$(CP) ../fixed/dobundle.pl $(RPREFIX)/src/gnuwin32/fixed
	$(CP) -pr ../help ../check $(RPREFIX)/src/gnuwin32
	$(RM) $(RPREFIX)/src/gnuwin32/help/*.c \
	  $(RPREFIX)/src/gnuwin32/help/MkChmDll
	(cd ..; $(MAKE) --no-print-directory -C installer/$(RPREFIX)/src/gnuwin32/help clean)
	$(CP) -pr $(R_HOME)/include $(R_HOME)/lib $(RPREFIX)
	$(CP) -pr $(foreach i,$(SHARE_DIRS),$(R_HOME)/share/$i) $(RPREFIX)/share
	(cd ..; $(MAKE) --no-print-directory PKGDIR=. pkgclean-windlgs)
	$(CP) -pr ../windlgs $(RPREFIX)/src/library
#
	find $(RPREFIX) -name .svn -prune -exec rm -rf \{\} \;

md5sums:
	@echo "adding MD5 checksums"
	@(cd $(RPREFIX); echo "tools:::.installMD5sums(\".\")" | \
	  R_DEFAULT_PACKAGES=NULL LC_COLLATE=C R_OSTYPE=windows \
	  ../../../../bin/Rterm.exe --vanilla --slave)

fixups:
	$(CAT) $(RPREFIX)/library/*/CONTENTS >  \
	  $(RPREFIX)/doc/html/search/index.txt
	echo "make.packages.html(.Library)" | $(RPREFIX)/bin/rterm --vanilla --slave LC_COLLATE=C
	$(CP) -pr $(R_HOME)/Tcl $(RPREFIX)

R.iss Rsmall.iss: JRins.pl imagedir fixups md5sums
	perl JRins.pl $(RPREFIX) 

$(RPREFIX).exe: R.iss
	"$(ISDIR)/iscc" R.iss > $(RPREFIX).log
miniR.exe: Rsmall.iss
	"$(ISDIR)/iscc" Rsmall.iss > miniR.log

clean:
	rm -rf R.iss Rsmall.iss *.log $(RPREFIX) 

distclean: clean
	rm -f $(RPREFIX).exe miniR*
