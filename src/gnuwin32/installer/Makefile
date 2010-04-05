# Makefile for R for Windows Installers

include ../MkRules
R_HOME = ../../..
include $(R_HOME)/share/make/vars.mk

## use next to install other packages from main library
EXTRA_PKGS =

## Default display mode, 0=MDI, 1=SDI
MDISDI = 0

## Default help style, 0=plain text, 1=HTML
HelpStyle = 1

## Default internet connection, 0=Standard, 1=Internet2
Internet = 0

## Used to set Registry entries
Producer = R-core

## Limit installation to 64-bit systems
ifeq ($(strip $(WIN)),64)
bit64 = 1;
SUFF = win64
else
bit64 = 0;
SUFF = win32
endif

FULLVERSION=`cat ../../../VERSION`
RPREFIX = $(shell $(PERL) ../fixed/rwver.pl $(R_HOME)/VERSION)
LIBFILES = $(foreach i, $(R_PKGS_BASE) $(R_PKGS_RECOMMENDED) $(EXTRA_PKGS), $(R_HOME)/library/$i) $(R_HOME)/library/R.css

INFO_FILES = COPYING NEWS README
DOC_INFO_FILES = AUTHORS COPYING COPYING.LIB FAQ RESOURCES THANKS CHANGES
GW_INFO_FILES = COPYRIGHTS README.Rterm rw-FAQ

ETC_FILES = Rprofile.site Rconsole Rdevga rgb.txt repositories Rcmd_environ
ifneq ($(strip $(WIN)),64)
ETC_FILES += Makeconf
endif


# we don't want share/{Makefile.in,sh} installed
SHARE_DIRS = R licenses locale make perl texmf zoneinfo

all: $(RPREFIX)-$(SUFF).exe
msi: $(RPREFIX)-win32.msi

imagedir:
	$(RM) -r $(RPREFIX)
	$(MKDIR) $(RPREFIX)
	$(CP) -pr $(R_HOME)/$(BINDIR) $(R_HOME)/modules $(RPREFIX)
# copy base and recommended packages
	$(MKDIR) -p $(RPREFIX)/library
	$(CP) -pr $(LIBFILES) $(RPREFIX)/library
	$(RM) $(RPREFIX)/library/*/MD5
	$(RM) $(RPREFIX)/*/Makefile.in
	$(MKDIR) -p $(RPREFIX)/etc$(R_ARCH)
	$(CP) -p $(foreach i,$(ETC_FILES),$(R_HOME)/etc/$i) $(RPREFIX)/etc
ifeq ($(strip $(WIN)),64)
	$(CP) -p $(R_HOME)/etc$(R_ARCH)/Makeconf $(RPREFIX)/etc$(R_ARCH)
endif
# copy info and documentation files
	$(MKDIR) -p $(RPREFIX)/doc
	$(CP) -p $(R_HOME)/doc/CRAN_mirrors.csv $(RPREFIX)/doc
	$(MKDIR) -p $(RPREFIX)/doc/manual
	$(CP) -pr $(R_HOME)/doc/html $(RPREFIX)/doc
	$(CP) -p $(R_HOME)/doc/manual/*.html $(R_HOME)/doc/manual/*.pdf \
	  $(RPREFIX)/doc/manual
#	html copies of these are in doc/manual, and we don't have FAQ PDF on menus
	$(RM) $(RPREFIX)/doc/manual/R-FAQ.pdf
	$(RM) $(RPREFIX)/doc/html/R-admin.html 
	$(RM) $(RPREFIX)/doc/html/index-default.html
#
	$(CP) -p $(foreach i,$(INFO_FILES),$(R_HOME)/$i) $(RPREFIX)
	$(CP) -p $(foreach i,$(DOC_INFO_FILES),$(R_HOME)/doc/$i) $(RPREFIX)/doc
	$(CP) -p $(foreach i,$(GW_INFO_FILES),../$i) $(RPREFIX)/doc
	$(CP) -p $(RPREFIX)/doc/CHANGES $(RPREFIX)
	$(SED) -e s/@RWVER@/$(RPREFIX)/g \
	  -e s/@RVER@/$(RVER)/g \
	  -e "s/@FULLVERSION@/$(FULLVERSION)/g" ../README > $(RPREFIX)/README.$(RPREFIX)
	$(SED) -e 3,5d ../README.packages > $(RPREFIX)/doc/README.packages
# convert docs and batch files to CRLF: might already be on Windows, though.
	zip -q dosfiles.zip $(RPREFIX)/* $(RPREFIX)/doc/* $(RPREFIX)/$(BINDIR)/*.bat
	unzip -qao dosfiles.zip
	$(RM) dosfiles.zip
#
# files to make source packages
	$(MKDIR) -p $(RPREFIX)/share \
	  $(RPREFIX)/src/library $(RPREFIX)/src/gnuwin32
	$(CP) $(R_HOME)/doc/html/logo.jpg $(RPREFIX)/doc/html
	$(CP) $(R_HOME)/doc/KEYWORDS* $(RPREFIX)/doc
	$(CP) -pr $(R_HOME)/include $(RPREFIX)
	$(CP) -pr $(foreach i,$(SHARE_DIRS),$(R_HOME)/share/$i) $(RPREFIX)/share
# This is odd -- better to clean the copy
	(cd ..; $(MAKE) --no-print-directory PKGDIR=. pkgclean-windlgs)
	$(CP) -pr ../windlgs $(RPREFIX)/src/library
#
# test dir
	mkdir $(RPREFIX)/tests
	$(MAKE) --no-print-directory -C ../../../tests -f Makefile.win \
	  INST_TO=../src/gnuwin32/installer/$(RPREFIX)/tests install-tests
#
	$(FIND) $(RPREFIX) -name .svn -prune -exec rm -rf \{\} \;
	$(FIND) $(RPREFIX) -name \*~ -delete
#
	$(RM) -r $(RPREFIX)/library/*/man

md5sums:
	@echo "adding MD5 checksums"
	@(cd $(RPREFIX); echo "tools:::.installMD5sums(\".\")" | \
	  R_DEFAULT_PACKAGES=NULL LC_COLLATE=C R_OSTYPE=windows \
	  ../../../../$(BINDIR)/Rterm.exe --vanilla --slave)

fixups:
	echo "make.packages.html(.Library, verbose=FALSE)" | $(RPREFIX)/$(BINDIR)/rterm --vanilla --slave LC_COLLATE=C
	$(CP) -pr $(R_HOME)/Tcl $(RPREFIX)


## for Inno Setup installer

R.iss: JRins.pl CustomMsg.txt imagedir fixups md5sums \
  ../../../VERSION ../../../SVN-REVISION
	$(PERL) JRins.pl $(RPREFIX) $(RPREFIX) $(MDISDI) $(HelpStyle) $(Internet) $(Producer) $(bit64)

$(RPREFIX)-$(SUFF).exe: R.iss
	"$(ISDIR)/iscc" R.iss > $(RPREFIX).log 2>&1


## for MSI installer

uuidgen.exe: uuidgen.o
	$(CC) -o $@ $< -lole32

# currently use about 3200
uuids: uuidgen.exe
	uuidgen 10000 > $@

files.wxs: imagedir fixups md5sums
	"$(WIXDIR)/tallow" -1 -d $(RPREFIX) > $@

R.wxs: WIXins.pl uuids files.wxs
	$(PERL) WIXins.pl $(RPREFIX) $(RPREFIX) $(WIX_PERSONAL)

## To change compression with Wix 3.0 use
## light -dcl:high
## http://www.joyofsetup.com/2008/01/28/new-wix-feature-defaultcompressionlevel/
ifeq ($(strip $(WIN)),64)
$(RPREFIX)-win32.msi:
	@echo "MSI installs for 64-bit builds are not yet supported"
	@exit 1
else
$(RPREFIX)-win32.msi: R.wxs License.rtf
	"$(WIXDIR)/candle" R.wxs > $(RPREFIX).log 2>&1
	"$(WIXDIR)/light" -out $@ R.wixobj $(WIXDIR)/wixui.wixlib \
	  -loc $(WIXDIR)/WixUI_en-us.wxl >> $(RPREFIX).log 2>&1
endif



clean:
	rm -rf R.iss *.log $(RPREFIX) R.wxs files.wxs R.wixobj \
	  uuidgen.o uuidgen.exe uuids

distclean: clean
	rm -f $(RPREFIX)-win32.* $(RPREFIX)-win64.*

myR:
	echo "make.packages.html(.Library)" | $(IMAGEDIR)/$(BINDIR)/rterm --vanilla --slave LC_COLLATE=C
	@echo "adding MD5 checksums"
	@(cd $(IMAGEDIR); echo "tools:::.installMD5sums(\".\")" | \
	  R_DEFAULT_PACKAGES=NULL LC_COLLATE=C R_OSTYPE=windows \
	  bin/Rterm.exe --vanilla --slave)
	$(PERL) JRins.pl $(RPREFIX) $(IMAGEDIR) $(MDISDI) $(HelpStyle) $(Internet) $(Producer)
	"$(ISDIR)/iscc" R.iss > myR.log
