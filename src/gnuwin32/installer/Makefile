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

ifeq "$(WIN)" "64"
ifneq ($(strip $(HOME32)),)
SUFF = win
else
SUFF = win64
endif
else
SUFF = win32
endif

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
	$(CP) -p $(R_HOME)/doc/NEWS.rds $(R_HOME)/doc/CHANGES.rds $(RPREFIX)/doc
#
# files to make source packages
	$(MKDIR) -p $(RPREFIX)/share \
	  $(RPREFIX)/src/library $(RPREFIX)/src/gnuwin32
	$(CP) $(R_HOME)/doc/html/logo.jpg $(RPREFIX)/doc/html
	$(CP) $(R_HOME)/doc/KEYWORDS* $(RPREFIX)/doc
	$(CP) -pR $(R_HOME)/include $(RPREFIX)
	$(CP) -pR $(foreach i,$(SHARE_DIRS),$(R_HOME)/share/$i) $(RPREFIX)/share
# This is odd -- better to clean the copy
	(cd ..; $(MAKE) --no-print-directory PKGDIR=. pkgclean-windlgs)
	$(CP) -pR ../windlgs $(RPREFIX)/src/library
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

PDF_FILES = $(wildcard $(RPREFIX)/library/*/doc/*.pdf)

fixups:
	echo "make.packages.html(.Library,temp=FALSE,verbose=FALSE)" | $(RPREFIX)/$(BINDIR)/Rterm.exe --vanilla --slave LC_COLLATE=C
	$(CP) -pRf $(R_HOME)/Tcl $(RPREFIX)
	$(RM) $(RPREFIX)/Tcl*/lib/*.lib
ifneq ($(strip $(QPDF)),)
	@for f in $(PDF_FILES); do \
	  $(QPDF)/bin/qpdf --stream-data=compress $${f} tmp.pdf; \
	  mv tmp.pdf $${f}; \
	done
endif

32bit:
ifeq "$(WIN)" "64"
ifneq ($(strip $(HOME32)),)
	@echo "------------ including 32-bit files"
	(cd "$(HOME32)"; zip -qr9X 32bit */i386 library/*/libs/i386 bin/R.exe bin/Rscript.exe)
	unzip -qo "$(HOME32)/32bit" -d $(RPREFIX)
	rm "$(HOME32)/32bit.zip"
	mkdir -p $(RPREFIX)/Tcl
	$(CP) -pRf "$(HOME32)/Tcl/bin" "$(HOME32)/Tcl/lib" $(RPREFIX)/Tcl
	for f in $(R_PKGS_RECOMMENDED); do \
	   sed -e 's/Archs: x64/Archs: i386, x64/' \
             ../../../library/$${f}/DESCRIPTION \
             > $(RPREFIX)/library/$${f}/DESCRIPTION; \
	done
else
	@echo "------------ not including 32-bit files"
endif
endif

extras:
	for f in $(DLLs32); do \
	  cp "$$f" $(RPREFIX)/bin/i386; \
	done
	for f in $(DLLs64); do \
	  cp "$$f" $(RPREFIX)/bin/x64; \
	done

## for Inno Setup installer

R.iss: JRins.R header1.iss types3264.iss types32.iss code.iss \
   CustomMsg.iss imagedir fixups 32bit extras md5sums \
  ../../../VERSION ../../../SVN-REVISION
	../../../bin$(R_ARCH)/Rscript JRins.R \
	  $(RPREFIX) $(RPREFIX) $(MDISDI) \
	  $(HelpStyle) $(Producer) "$(ISDIR)"

$(RPREFIX)-win.exe: R.iss
	"$(ISDIR)/iscc" R.iss > $(RPREFIX).log 2>&1


## for MSI installer

uuidgen.exe: uuidgen.o
	$(CC) -o $@ $< -lole32

# currently use about 3500: allow for EXTRA_PKGS
uuids: uuidgen.exe
	./uuidgen 10000 > $@

files.wxs: imagedir fixups 32bit extras md5sums
	"$(WIX3DIR)/heat" dir $(RPREFIX) -sw5150 -nologo -o $@

R.wxs: WIXins.R uuids files.wxs
	../../../bin$(R_ARCH)/Rscript WIXins.R $(RPREFIX) $(RPREFIX) $(WIX_PERSONAL) || (rm -f $@;exit 1)

$(RPREFIX)-$(SUFF).msi: R.wxs License.rtf
	"$(WIX3DIR)/candle" R.wxs > $(RPREFIX).log 2>&1
	"$(WIX3DIR)/light" -out $@ R.wixobj -dcl:high \
	 -ext WixUIExtension -sw1076 -sval -spdb >> $(RPREFIX).log 2>&1


## too often this fails to empty directories.
clean:
	rm -rf R.iss *.log R.wxs files.wxs R.wixobj uuidgen.o uuidgen.exe uuids
	-rm -rf $(RPREFIX)

distclean: clean
	rm -f R*-win*.exe R*-win*.msi

myR:
	echo "make.packages.html(.Library, verbose=FALSE)" | "$(IMAGEDIR)/$(BINDIR)/Rterm.exe" --vanilla --slave LC_COLLATE=C
	@echo "adding MD5 checksums"
	@(cd "$(IMAGEDIR)"; echo "tools:::.installMD5sums(\".\")" | \
	  R_DEFAULT_PACKAGES=NULL LC_COLLATE=C R_OSTYPE=windows \
	  "$(IMAGEDIR)/$(BINDIR)/Rterm.exe" --vanilla --slave)
	../../../bin$(R_ARCH)/Rscript JRins.R \
	  $(RPREFIX) "$(IMAGEDIR)" $(MDISDI) \
	  $(HelpStyle) $(Producer) "$(ISDIR)"
	"$(ISDIR)/iscc" R.iss > myR.log
