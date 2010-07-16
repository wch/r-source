# Makefile for R for Windows Installers

include ../MkRules
R_HOME = ../../..
## for TCL_HOME
RHOME = ../../..
include $(R_HOME)/share/make/vars.mk

## use this to install other packages from main library
EXTRA_PKGS =

## Default display mode, 0=MDI, 1=SDI
MDISDI = 0

## Default help style, 0=plain text, 1=HTML
HelpStyle = 1

## Default internet connection, 0=Standard, 1=Internet2
Internet = 0

## Used to set Registry entries
Producer = R-core

ifeq ($(strip $(WIN)),64)
ifneq ($(strip $(HOME32)),)
SUFF = win
else
SUFF = win64
endif
else
SUFF = win32
endif

FULLVERSION=`cat ../../../VERSION`
#RPREFIX = $(shell $(R_HOME)/tools/GETRWVERSION $(R_HOME)/VERSION)
RPREFIX = $(shell ../../../bin$(R_ARCH)/Rscript ../fixed/rwver.R)
LIBFILES = $(foreach i, $(R_PKGS_BASE) $(R_PKGS_RECOMMENDED) $(EXTRA_PKGS), $(R_HOME)/library/$i) $(R_HOME)/library/R.css

INFO_FILES = COPYING NEWS README
DOC_INFO_FILES = AUTHORS COPYING COPYING.LIB FAQ RESOURCES THANKS CHANGES
GW_INFO_FILES = COPYRIGHTS README.Rterm rw-FAQ

ETC_FILES = Rprofile.site Rconsole Rdevga rgb.txt repositories Rcmd_environ


# we don't want share/{Makefile.in,java,perl,sh} installed
SHARE_DIRS = R encodings licenses locale make texmf zoneinfo

all: $(RPREFIX)-$(SUFF).exe
msi: $(RPREFIX)-win32.msi

imagedir:
	$(RM) -r $(RPREFIX)
	$(MKDIR) $(RPREFIX)
	$(CP) -pr $(R_HOME)/bin $(R_HOME)/modules $(RPREFIX)
# copy base and recommended packages
	$(MKDIR) -p $(RPREFIX)/library
	$(CP) -pr $(LIBFILES) $(RPREFIX)/library
	$(RM) $(RPREFIX)/library/*/MD5
	$(RM) $(RPREFIX)/*/Makefile.in
	$(MKDIR) -p $(RPREFIX)/etc$(R_ARCH)
	$(CP) -p $(foreach i,$(ETC_FILES),$(R_HOME)/etc/$i) $(RPREFIX)/etc
	$(CP) -p $(R_HOME)/etc$(R_ARCH)/Makeconf $(RPREFIX)/etc$(R_ARCH)
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
	zip -q dosfiles.zip $(RPREFIX)/* $(RPREFIX)/doc/*
	unzip -qao dosfiles.zip
	$(RM) dosfiles.zip
	$(CP) -p $(R_HOME)/NEWS.pdf $(RPREFIX)
	$(CP) -p $(R_HOME)/doc/NEWS.rds $(R_HOME)/doc/CHANGES.rds $(RPREFIX)/doc
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

md5sums:
	@echo "adding MD5 checksums"
	@(cd $(RPREFIX); echo "tools:::.installMD5sums(\".\")" | \
	  R_DEFAULT_PACKAGES=NULL LC_COLLATE=C R_OSTYPE=windows \
	  ../../../../$(BINDIR)/Rterm.exe --vanilla --slave)

fixups:
	echo "make.packages.html(.Library, verbose=FALSE)" | $(RPREFIX)/$(BINDIR)/Rterm.exe --vanilla --slave LC_COLLATE=C
	$(CP) -prf "$(TCL_HOME)" $(RPREFIX)
	$(RM) $(RPREFIX)/Tcl*/lib/*.lib

32bit:
ifeq ($(strip $(WIN)),64)
ifneq ($(strip $(HOME32)),)
	@echo "------------ including 32-bit files"
	(cd "$(HOME32)"; zip -qr9X 32bit */i386 library/*/libs/i386 bin/R.exe bin/Rscript.exe)
	unzip -qo "$(HOME32)/32bit" -d $(RPREFIX)
	rm "$(HOME32)/32bit.zip"
	mkdir -p $(RPREFIX)/Tcl
	$(CP) -prf "$(HOME32)/Tcl/bin" "$(HOME32)/Tcl/lib" $(RPREFIX)/Tcl
	for f in $(R_PKGS_RECOMMENDED); do \
	   sed -e 's/Archs: x64/Archs: i386, x64/' \
             ../../../library/$${f}/DESCRIPTION \
             > $(RPREFIX)/library/$${f}/DESCRIPTION; \
	done
else
	@echo "------------ not including 32-bit files"
endif
endif

## for Inno Setup installer

R.iss: JRins.R header1.iss types3264.iss types32.iss code.iss \
   CustomMsg.txt imagedir fixups 32bit md5sums \
  ../../../VERSION ../../../SVN-REVISION
	../../../bin$(R_ARCH)/Rscript JRins.R \
	  $(RPREFIX) $(RPREFIX) $(MDISDI) $(HelpStyle) $(Internet) $(Producer)

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

## for WiX 3.0 light uses -ext WixUIExtension
## To change compression with WiX 3.0 use
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
	rm -f R*-win32.*exe R*-win32.msi R*-win64.exe R*-win.exe

myR:
	echo "make.packages.html(.Library, verbose=FALSE)" | "$(IMAGEDIR)/$(BINDIR)/Rterm.exe" --vanilla --slave LC_COLLATE=C
	@echo "adding MD5 checksums"
	@(cd "$(IMAGEDIR)"; echo "tools:::.installMD5sums(\".\")" | \
	  R_DEFAULT_PACKAGES=NULL LC_COLLATE=C R_OSTYPE=windows \
	  "$(IMAGEDIR)/$(BINDIR)/Rterm.exe" --vanilla --slave)
	../../../bin$(R_ARCH)/Rscript JRins.R \
	  $(RPREFIX) "$(IMAGEDIR)" $(MDISDI) $(HelpStyle) $(Internet) $(Producer)
	"$(ISDIR)/iscc" R.iss > myR.log
