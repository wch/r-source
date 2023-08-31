# Makefile for R for Windows Installers

include ../MkRules

## Formerly in MkRules
FIND = find

R_HOME = ../../..
include $(R_HOME)/share/make/vars.mk

## use this to install other packages from main library
EXTRA_PKGS =

## Default display mode, 0=MDI, 1=SDI
MDISDI = 0

## Default help style, 0=plain text, 1=HTML
HelpStyle = 1

## Used to set Registry entries
Producer = R-core

SUFF = win64

FULLVERSION=`cat ../../../VERSION`
RPREFIX = $(shell ../../../bin$(R_ARCH)/Rscript ../fixed/rwver.R)
LIBFILES = $(foreach i, $(R_PKGS_BASE) $(R_PKGS_RECOMMENDED) translations $(EXTRA_PKGS), $(R_HOME)/library/$i)

INFO_FILES = COPYING README
DOC_INFO_FILES = AUTHORS COPYING FAQ RESOURCES THANKS CHANGES \
  NEWS NEWS.0 NEWS.1 NEWS.2 
GW_INFO_FILES = COPYRIGHTS README.Rterm ../../doc/manual/rw-FAQ

ETC_FILES = Rprofile.site Rconsole Rdevga rgb.txt repositories Rcmd_environ


# we don't want share/{Makefile.in,java,sh} installed
SHARE_DIRS = R dictionaries encodings licenses make Rd texmf zoneinfo

all: $(RPREFIX)-win.exe
msi: $(RPREFIX)-$(SUFF).msi

imagedir:
	$(RM) -R $(RPREFIX)
	$(MKDIR) $(RPREFIX)
	$(CP) -pR $(R_HOME)/bin $(R_HOME)/modules $(RPREFIX)
# copy base and recommended packages
	$(MKDIR) -p $(RPREFIX)/library
	$(CP) -pR $(LIBFILES) $(RPREFIX)/library
	$(RM) $(RPREFIX)/library/*/MD5
	$(RM) $(RPREFIX)/*/Makefile.in
	$(MKDIR) -p $(RPREFIX)/etc$(R_ARCH)
	$(CP) -p $(foreach i,$(ETC_FILES),$(R_HOME)/etc/$i) $(RPREFIX)/etc
	if test -f "$(R_HOME)/etc/curl-ca-bundle.crt"; then \
	  $(CP) -p "$(R_HOME)/etc/curl-ca-bundle.crt" $(RPREFIX)/etc; \
	fi
	$(CP) -p $(R_HOME)/etc$(R_ARCH)/Makeconf $(RPREFIX)/etc$(R_ARCH)
# copy info and documentation files
	$(MKDIR) -p $(RPREFIX)/doc
	$(CP) -p $(R_HOME)/doc/CRAN_mirrors.csv $(RPREFIX)/doc
	$(CP) -p $(R_HOME)/doc/BioC_mirrors.csv $(RPREFIX)/doc
	$(MKDIR) -p $(RPREFIX)/doc/manual/images
	$(CP) -pR $(R_HOME)/doc/html $(RPREFIX)/doc
	$(CP) -p $(R_HOME)/doc/manual/*.html $(R_HOME)/doc/manual/*.pdf \
	  $(RPREFIX)/doc/manual
	$(CP) -p $(R_HOME)/doc/manual/images/*.png $(RPREFIX)/doc/manual/images
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
	$(CP) -p $(R_HOME)/doc/NEWS.pdf $(RPREFIX)/doc
	$(CP) -p $(R_HOME)/doc/NEWS.rds $(R_HOME)/doc/NEWS.2.rds  $(R_HOME)/doc/NEWS.3.rds $(R_HOME)/doc/CHANGES.rds $(RPREFIX)/doc
#
# files to make source packages
	$(MKDIR) -p $(RPREFIX)/share \
	  $(RPREFIX)/src/library $(RPREFIX)/src/gnuwin32
	$(CP) $(R_HOME)/doc/html/logo.jpg $(R_HOME)/doc/html/Rlogo.svg $(RPREFIX)/doc/html
	$(CP) $(R_HOME)/doc/KEYWORDS* $(RPREFIX)/doc
	$(CP) -pR $(R_HOME)/include $(RPREFIX)
	$(CP) -pR $(foreach i,$(SHARE_DIRS),$(R_HOME)/share/$i) $(RPREFIX)/share
# This is odd -- better to clean the copy
	(cd ..; $(MAKE) --no-print-directory PKGDIR=. pkgclean-windlgs)
	$(CP) -pR ../windlgs $(RPREFIX)/src/library

# Edit Makeconf
	$(SED) -i -e 's!# INSTALLER-BUILD:!!' $(RPREFIX)/etc$(R_ARCH)/Makeconf
# Edit Rcmd_environ
	$(SED) -i -e 's!# INSTALLER-BUILD:!!' $(RPREFIX)/etc/Rcmd_environ
# Edit base package profile
	$(SED) -i -e 's!# INSTALLER-BUILD:!!' $(RPREFIX)/library/base/R/Rprofile
# test dir
	mkdir $(RPREFIX)/tests
	$(MAKE) --no-print-directory -C ../../../tests -f Makefile.win \
	  INST_TO=../src/gnuwin32/installer/$(RPREFIX)/tests install-tests
#
	$(FIND) $(RPREFIX) -name .svn -prune -exec rm -Rf \{\} \;
	$(FIND) $(RPREFIX) -name \*~ -delete

md5sums:
	@echo "adding MD5 checksums"
	@(cd $(RPREFIX); echo "tools:::.installMD5sums(\".\")" | \
	  R_DEFAULT_PACKAGES=NULL LC_COLLATE=C R_OSTYPE=windows \
	  ../../../../$(BINDIR)/Rterm.exe --vanilla --no-echo)

PDF_FILES = $(wildcard $(RPREFIX)/library/*/doc/*.pdf)

fixups:
	echo "make.packages.html(.Library,temp=FALSE,verbose=FALSE)" | $(RPREFIX)/$(BINDIR)/Rterm.exe --vanilla --no-echo LC_COLLATE=C
	$(CP) -pRf $(R_HOME)/Tcl $(RPREFIX)
	$(RM) $(RPREFIX)/Tcl*/lib/*.lib
ifneq ($(strip $(QPDF)),)
	@for f in $(PDF_FILES); do \
	  "$(QPDF)/bin/qpdf" $${f} tmp.pdf; \
	  mv tmp.pdf $${f}; \
	done
endif

extras:
	for f in $(DLLs64); do \
	  cp "$$f" $(RPREFIX)/bin$(R_ARCH); \
	done

## for Inno Setup installer

R.iss: JRins.R header1.iss types64.iss code.iss reg64.iss \
   CustomMsg.iss imagedir fixups extras md5sums \
  ../../../VERSION ../../../SVN-REVISION
	../../../bin$(R_ARCH)/Rscript JRins.R \
	  $(RPREFIX) $(RPREFIX) $(MDISDI) \
	  $(HelpStyle) $(Producer) "$(ISDIR)"

$(RPREFIX)-win.exe: R.iss
	"$(ISDIR)/iscc" R.iss > $(RPREFIX).log 2>&1


## for MSI installer

uuidgen.exe: uuidgen.o
	$(CC) -o $@ $< -lole32

# currently use about 4200: allow for EXTRA_PKGS
uuids: uuidgen.exe
	./uuidgen 10000 > $@

files.wxs: imagedir fixups extras md5sums
	"$(WIX3DIR)/heat" dir $(RPREFIX) -sw5150 -nologo -o $@

R.wxs: WIXins.R uuids files.wxs
	../../../bin$(R_ARCH)/Rscript WIXins.R $(RPREFIX) $(RPREFIX) $(WIX_PERSONAL) || (rm -f $@;exit 1)

$(RPREFIX)-$(SUFF).msi: R.wxs License.rtf
	"$(WIX3DIR)/candle" R.wxs > $(RPREFIX).log 2>&1
	"$(WIX3DIR)/light" -out $@ R.wixobj -dcl:high \
	 -ext WixUIExtension -sw1076 -sval -spdb >> $(RPREFIX).log 2>&1


## too often this fails to empty directories.
clean:
	rm -Rf R.iss *.log R.wxs files.wxs R.wixobj uuidgen.o uuidgen.exe uuids
	-rm -Rf $(RPREFIX)

distclean: clean
	rm -f R*-win*.exe R*-win*.msi

myR:
	echo "make.packages.html(.Library, verbose=FALSE)" | "$(IMAGEDIR)/$(BINDIR)/Rterm.exe" --vanilla --no-echo LC_COLLATE=C
	@echo "adding MD5 checksums"
	@(cd "$(IMAGEDIR)"; echo "tools:::.installMD5sums(\".\")" | \
	  R_DEFAULT_PACKAGES=NULL LC_COLLATE=C R_OSTYPE=windows \
	  "$(IMAGEDIR)/$(BINDIR)/Rterm.exe" --vanilla --no-echo)
	../../../bin$(R_ARCH)/Rscript JRins.R \
	  $(RPREFIX) "$(IMAGEDIR)" $(MDISDI) \
	  $(HelpStyle) $(Producer) "$(ISDIR)"
	"$(ISDIR)/iscc" R.iss > myR.log
