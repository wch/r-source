include MkRules
include RWVERSION

all: rbuild rpackage rdocs

## configuration macros
HELP = YES
WINHELP = CHM# NO, CHM, BOTH

LEA_MALLOC=YES

# Set to YES and specify the path if you want to use the ATLAS BLAS.
USE_ATLAS=NO
ATLAS_PATH=/R/ATLAS/lib/WinNT_PII

PACKAGES = base ctest eda lqs methods modreg mva nls splines stepfun tools ts
ALLPACKAGES = $(PACKAGES) tcltk

RHOME = $(shell perl pwd.pl ../..)# must be absolute path
RLIB = $(RHOME)/library
PKGDIR = $(RHOME)/src/library
PKGD = $(shell perl pwd.pl $(PKGDIR))# must be absolute path

OPTFLAGS=-O2 -Wall -pedantic
## use FOPTFLAGS where complex*16 is used.
FOPTFLAGS=-O2 -Wall
ifdef DEBUG
 OPTFLAGS+=-g
 FOPTFLAGS+=-g
 DLLFLAGS=
else
 DLLFLAGS=-s
endif

R-DLLFLAGS = -mwindows
R-DLLLIBS =-L. $(FLIBS) -lcomctl32 -lversion
ifeq ($(strip $(USE_ATLAS)),YES)
FLIBS +=-L$(ATLAS_PATH) -lcblas -lf77blas -latlas
endif
SOURCES = $(filter-out malloc.c,$(wildcard *.c *.f))
OBJS = $(SOURCES:.c=.o)
MAINLIBS = libmain.a libappl.a libmath.a
EXTRALIBS = ga.a gl.a xdr.a em.a bitmap/zlib/libz.a

LIBFILES = $(foreach i, $(ALLPACKAGES), ../../library/$i) ../../library/R.css
PKGS = $(foreach i,$(PACKAGES),pkg-$i)
PKGCLEAN = $(foreach i,$(ALLPACKAGES),pkgclean-$i)

# flags to ensure that Doug Lea's malloc is used:
ifdef LEA_MALLOC
extra-CFLAGS+=-DLEA_MALLOC
OBJS-EXTRA=malloc.o
endif

.PHONY:  clean veryclean  rlibs fixfiles front-ends rmodules


# separate as Tcl/Tk headers are needed.
tcl: 
	$(MAKE) STAMP=NO pkg-tcltk

CFLAGS = $(OPTFLAGS) -I../include -I. -DHAVE_CONFIG_H -DR_DLL_BUILD
FFLAGS = $(OPTFLAGS)

rbuild: fixfiles rlibs makeMakedeps ../../bin/R.dll libR.a \
	 CHTML front-ends COPYRIGHTS rmodules

rpackage: fixed/fixdesc ../../library/R.css
	$(MAKE) STAMP=NO $(PKGS)

rinstaller:
	$(MAKE) -C installer RPREFIX=$(RPREFIX)

rlibs:
	$(MAKE) -C ../appl  OPTFLAGS='$(OPTFLAGS)' USE_ATLAS=$(USE_ATLAS) -f Makefile.win
	$(MAKE) -C ../main  OPTFLAGS='$(OPTFLAGS)' -f Makefile.win
	$(MAKE) -C ../nmath OPTFLAGS='$(OPTFLAGS)' -f Makefile.win
	$(MAKE) -C ./graphapp
	$(MAKE) -C ./getline
	$(MAKE) -C ../extra/xdr
	$(MAKE) -C ./math

rmodules: libRm.a
	$(MAKE) -C ../modules OPTFLAGS='$(OPTFLAGS)' FOPTFLAGS='$(FOPTFLAGS)' \
	  USE_ATLAS=$(USE_ATLAS) ATLAS_PATH=$(ATLAS_PATH) -f Makefile.win

COPYRIGHTS: $(RHOME)/COPYRIGHTS COPYRIGHTS.win
	cat $^ > $@


fixfiles:
	$(MKDIR) -p ../../bin
	$(MAKE) -C ./fixed
	$(CP) -pr unzip ../..
	$(RM) ../../unzip/*.h

fixed/fixdesc:
	sh fixed/GETDESC $(ALLPACKAGES)

dllversion.o: ../include/Rversion.h

R.dll: R.a $(OBJS-EXTRA) $(MAINLIBS) $(EXTRALIBS) dllversion.o

Rdll: makeMakedeps ../../bin/R.dll

../../bin/R.dll: fixfiles rlibs R.dll
	$(MKDIR) -p ../../bin
	$(CP) R.dll ../../bin/

front-ends: libRm.a
	$(MAKE) -C front-ends

R.a: $(OBJS)
	@$(ECHO) -------- Building $@ --------
	$(AR) cr R.a $^
	$(RANLIB) R.a


# R.dll exports all global symbols BUT import library (libR.a)
# only (all functions) + (global vars listed in exported-vars))

libR.a:  R.exp
	$(DLLTOOL) $(DLLTOOLFLAGS) $(R-DLLTOOLFLAGS) --dllname R.dll --def R.exp --output-lib libR.a

R.exp: R.exp1
	(diff R.exp R.exp1 > /dev/null) || $(CP) R.exp1 R.exp

R.exp1: R.a $(MAINLIBS) $(EXTRALIBS) exported-vars
	$(ECHO) EXPORTS > $@
	$(NM) R.a $(MAINLIBS) $(EXTRALIBS) > RDefs
	$(SED) -n '/^........ [T] _/s/^........ [T] _/ /p' RDefs >> $@
	$(CAT) exported-vars >> $@
	$(RM) RDefs

# A version needed for the modules that uses R.dll's malloc/.../free

libRm.a:  Rm.exp
	$(DLLTOOL) $(DLLTOOLFLAGS) $(R-DLLTOOLFLAGS) --dllname R.dll --def Rm.exp --output-lib libRm.a

Rm.exp: Rm.exp1
	(diff Rm.exp Rm.exp1 > /dev/null) || $(CP) Rm.exp1 Rm.exp

Rm.exp1: R.a $(OBJS-EXTRA) $(MAINLIBS) $(EXTRALIBS) exported-vars
	$(ECHO) EXPORTS > $@
	$(NM) R.a $(OBJS-EXTRA) $(MAINLIBS) $(EXTRALIBS) > RDefs
	$(SED) -n '/^........ [T] _/s/^........ [T] _/ /p' RDefs >> $@
	$(CAT) exported-vars >> $@
	$(RM) RDefs

../../library/R.css: ../../doc/html/R.css
	$(CP) $< $@

bitmap/zlib/libz.a:
	$(MAKE) -C bitmap buildzlib

rdocs:
	@$(MAKE) -C ../../doc/manual -f Makefile.win html


#                          ===== Packages ======

CHTML:
ifeq ($(strip $(HELP)),YES)
ifeq ($(strip $(WINHELP)),CHM)
	$(MAKE) -C ./help -f MkChmDll
endif
ifeq ($(strip $(WINHELP)),BOTH)
	$(MAKE) -C ./help -f MkChmDll
endif
endif

base-HELP = ziponly
nls-HELP = ziponly
ts-HELP = ziponly
base-ZIPDATA = zip
ts-ZIPDATA = zip

MASS-HELP = ziponly
MASS-ZIPDATA = zip

bundle-%:
	$(PERL) fixed/dobundle.pl $* $(PKGD) $(RLIB)

pkg-%:
	$(MAKE) DLLNM=$($*-DLLNM)  FLIBS="$(FLIBS)" \
	  -C $(PKGDIR)/$* PKG=$* RHOME=$(RHOME) RLIB=$(RLIB) \
	  -f $(RHOME)/src/gnuwin32/MakePkg
	@if test -d $(RLIB)/$*/data ; then \
	  $(MAKE) $($*-ZIPDATA)data-$* ; \
	fi
ifeq ($(strip $(HELP)),YES)
	$(MAKE) -C ./help RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) $($*-HELP)help-$*
	$(MAKE) -C ./help RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) contents-$*
ifeq ($(strip $(WINHELP)),CHM)
	$(MAKE) -C ./help RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) chm-$*
endif
ifeq ($(strip $(WINHELP)),BOTH)
	$(MAKE) -C ./help RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) winhlp-$* # also makes chm
endif
endif

pkgclean-%:
	$(MAKE) DLLNM=$($*-DLLNM) RHOME=$(RHOME) \
	  -C $(PKGDIR)/$* PKG=$* -f $(RHOME)/src/gnuwin32/MakePkg clean

pkgcheck-%:
	@$(ECHO) -------- Testing package $* --------
	-@mkdir -p $(PKGDIR)/$*/check
	@$(MAKE) -C $(PKGDIR)/$*/check PKG=$* RHOME=$(RHOME) RLIB=$(RLIB) \
	  -f $(RHOME)/src/gnuwin32/check/PkgCheck
	@if test -d $(PKGDIR)/$*/tests ; then \
	  $(MAKE) -C $(PKGDIR)/$*/tests PKG=$* RHOME=$(RHOME) RLIB=$(RLIB) \
	    -f $(RHOME)/src/gnuwin32/check/PkgCheck tests;\
	fi

ziphelp-%:
	$(MAKE) -C ./help RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) ziphelp-$*

ziponly-%:
	$(MAKE) -C ./help RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) ziponlyhelp-$*

zipdata-%:
	@echo "  zipping data"
	@(cd $(RLIB)/$*/data; \
	  if test -f Rdata.zip ; then \
	    unzip -qou Rdata; \
	  fi; \
	  $(RM) Rdata.zip filelist ; \
	  ls -1 > filelist; \
	  zip -rmq9X Rdata * -x filelist 00Index)

data-%:
	@echo "  not zipping data"


#                          ===== cleaning ======

clean: cleanwin $(PKGCLEAN)
	$(RM) -f ../*/*.o $(WINOBJS) *.a ../*/*.d ../*/Makedeps fixed/fixdesc
	-$(MAKE) -C ../../doc/manual -f Makefile.win distclean
	-$(MAKE) -C ../nmath/standalone -f Makefile.win distclean
	-$(MAKE) -C ../modules -f Makefile.win clean
	-$(MAKE) -C ../../tests -f Makefile.win clean
	$(RM) -f ../library/methods/R/methods

distclean veryclean: clean
	$(RM) COPYRIGHTS R.exp Rm.exp 
	$(RM) -r ../../bin ../../library ../../modules
	$(RM) ../include/config.h ../include/psignal.h \
	../include/Rconfig.h ../include/version.h 
	$(RM) ../library/*/src/*.o ../library/*/src/*.a
	$(RM) ../library/*/src/*.dll
	$(RM) -r ../library/*/check
	$(RM) ../../doc/html/search/index.txt ../../doc/html/index.html \
	../../doc/html/packages.html ../../doc/html/rwin.html \
	../../doc/html/rw-FAQ.html ../../doc/html/search/SearchEngine.html
	$(RM) ../../etc/Rconsole ../../etc/Rdevga ../../etc/Rprofile \
	../../etc/rgb.txt ../../share/make/tests.mk \
	../include/Platform.h ../../tests/*.Rout ../../tests/*.Rout.fail
	$(MAKE) -C fixed distclean
	-$(MAKE) -C installer ISdistclean
	@for pkg in $(ALLPACKAGES); do \
	  $(RM) ../library/$${pkg}/DESCRIPTION; \
	done

cleanwin:
	$(RM) *.o *.exe *.dll *.a *~ \#*\# rw*.zip R.def R.exp1 Rm.exp1\
	   .RData .Rhistory Makedeps *.d
	$(MAKE) -C graphapp clean
	$(MAKE) -C fixed clean
	$(MAKE) -C getline clean
	$(MAKE) -C help clean
	$(MAKE) -C front-ends clean
	$(MAKE) -C ../extra/xdr clean
	$(MAKE) -C math clean
	$(MAKE) -C bitmap clean
	-$(MAKE) -C installer clean ISclean


#                          ===== testing ======

# tests
check test: test-Specific pkgtests

test-Specific:
	-@$(MAKE) -C $(RHOME)/tests -f $(RHOME)/tests/Makefile.win clean
	-@$(MAKE) -C $(RHOME)/tests -f $(RHOME)/tests/Makefile.win RHOME=$(RHOME)


PKGS-CHK = $(foreach i, $(PACKAGES), pkgcheck-$i)
pkgtests: $(PKGS-CHK)


#                          ===== documentation ======

docs manuals:
	@$(MAKE) -C $(RHOME)/doc/manual -f Makefile.win clean BUILD=$(BUILD)
	@$(MAKE) -C $(RHOME)/doc/manual -f Makefile.win \
	  R_PKGS='$(ALLPACKAGES)' BUILD=$(BUILD)
	@$(MAKE) ziponly-base


## ===================== Maintainer targets ========================

RVER = $(shell cut -d' ' -f1 ../../VERSION | sed -n 1p)

docfiles: INSTALL FAQs
SEDVER = -e s/@RVER@/$(RVER)/g -e s/@RWVER@/$(RPREFIX)/g

INSTALL: INSTALL.in ../../VERSION RWVERSION
	$(SED) $(SEDVER) INSTALL.in > INSTALL

FAQs: rw-faq fixed/html/rw-FAQ.html

fixed/html/rw-FAQ.html: rw-FAQ.texi ../../VERSION RWVERSION
	@$(SED) $(SEDVER) $< > tmp.texi
	makeinfo --no-split --html --no-headers --number-sections -o tmp.html tmp.texi
	-tidy tmp.html > $@ 2> /dev/null
	@$(RM) tmp.texi tmp.html

rw-faq: rw-FAQ.texi ../../VERSION RWVERSION
	$(SED) $(SEDVER) $< | \
	  makeinfo --no-headers --number-sections -o $@

bitmapdll:
	$(MAKE) -C bitmap

distribution: bindist packagedist docsdist

bindist: tcl bitmapdll docfiles
	$(RM) -r $(RPREFIX)
	$(MKDIR) $(RPREFIX)
	$(CP) -pr ../../bin ../../modules ../../afm ../../unzip $(RPREFIX)
	$(MKDIR) $(RPREFIX)/library
	$(CP) -pr $(LIBFILES) $(RPREFIX)/library
# morley.tab is needed for R-intro Appendix A
	$(CP) -p ../library/base/data/morley.tab $(RPREFIX)/library/base/data
	$(RM) $(RPREFIX)/*/Makefile.in
	(cd $(RPREFIX)/bin; $(RM) Rcmd.exe INSTALL REMOVE SHLIB build check \
	   massage-Examples Rd2dvi.sh Rd2txt Rdconv Rdiff.sh Rdindex \
           Sd2Rd Rprof)
	$(MKDIR) $(RPREFIX)/etc
	$(CP) -p ../../etc/Rprofile $(RPREFIX)/etc/Rprofile
	$(CP) -p ../../etc/Rconsole $(RPREFIX)/etc/Rconsole
	$(CP) -p ../../etc/Rdevga $(RPREFIX)/etc/Rdevga
	$(CP) -p ../../etc/rgb.txt $(RPREFIX)/etc/rgb.txt
	$(MKDIR) -p $(RPREFIX)/doc/manual  $(RPREFIX)/share/texmf
	$(CP) -pr ../../doc/html $(RPREFIX)/doc
	$(CP) -p ../../doc/manual/*.html $(RPREFIX)/doc/manual
	$(CP) -pr ../../share/texmf $(RPREFIX)/share
	$(CP) -p ../../AUTHORS ../../COPYING COPYRIGHTS ../../FAQ ../../NEWS \
		../../THANKS ../../Y2K $(RPREFIX)
	$(SED) -e s/@RWVER@/$(RPREFIX)/g \
	  -e s/@RVER@/$(RVER)/g readme > $(RPREFIX)/README
	$(CP) -p rw-faq $(RPREFIX)/rw-FAQ
	$(CP) -p CHANGES README.Rterm $(RPREFIX)
	-find $(RPREFIX) -name CVS -exec rm -rf \{\} \;
# convert docs and batch files to CRLF: might already be on Windows, though.
ifeq ($(strip $(BUILD)),CROSS)
	zip -l dosfiles.zip $(RPREFIX)/* $(RPREFIX)/bin/*.bat
	unzip -ao dosfiles.zip
else
	zip dosfiles.zip $(RPREFIX)/* $(RPREFIX)/bin/*.bat
	unzip -o dosfiles.zip
endif
	$(RM) dosfiles.zip
	$(RM) $(RPREFIX)b.zip $(RPREFIX)m.zip $(RPREFIX)h.zip \
	   $(RPREFIX)w.zip $(RPREFIX)l.zip $(RPREFIX)ch.zip
	$(RM) anindex.zip
	zip -mrX anindex.zip $(RPREFIX)/library/*/help/AnIndex
	zip -mr9X $(RPREFIX)h.zip $(RPREFIX)/library/*/help
	unzip anindex.zip
	$(RM) anindex.zip
	$(RM) $(RPREFIX)/doc/manual/R-FAQ.html
	zip -rm9X $(RPREFIX)w.zip $(RPREFIX)/doc/html \
          $(RPREFIX)/doc/manual/*.html \
	  $(RPREFIX)/library/*/html $(RPREFIX)/library/R.css
	-zip -rm9X $(RPREFIX)ch.zip $(RPREFIX)/library/*/chtml \
	  $(RPREFIX)/bin/Rchtml.dll
#	$(RM) $(RPREFIX)/library/*/winhlp/*.GID
#	-zip -rm9X $(RPREFIX)wh.zip $(RPREFIX)/library/*/winhlp
	zip -rm9X $(RPREFIX)l.zip $(RPREFIX)/library/*/latex $(RPREFIX)/share/texmf
	$(RM) -r $(RPREFIX)/doc
	mkdir -p  $(RPREFIX)/doc/html
	$(CP) -p ../../doc/html/faq.html ../../doc/html/rw-FAQ.html \
	  $(RPREFIX)/doc/html
	zip -rm9X $(RPREFIX)m.zip $(RPREFIX)/library/*/man
	zip -r9X $(RPREFIX)b.zip $(RPREFIX)
	$(RM) -rf $(RPREFIX)


#taken from ../include/Makefile.in
SRC_HEADERS = R.h S.h Rdefines.h Rgraphics.h Rinternals.h Rmath.h
OBJ_HEADERS = Rconfig.h Rversion.h
GW32_HEADERS =

packagedist:
	$(RM) -r $(RPREFIX)sp.zip $(RPREFIX)
	$(MKDIR) -p $(RPREFIX) $(RPREFIX)/bin $(RPREFIX)/etc \
	  $(RPREFIX)/doc $(RPREFIX)/doc/html $(RPREFIX)/share \
	  $(RPREFIX)/src/include $(RPREFIX)/src/library \
	  $(RPREFIX)/src/gnuwin32/fixed
	$(CP) ../../doc/html/logo.jpg $(RPREFIX)/doc/html
	$(CP) ../../doc/KEYWORDS* $(RPREFIX)/doc
	$(CP) -p exported-vars R.exp MkRules MakePkg MakeDll \
	  pwd.pl makeDllRes.pl install-save.sh $(RPREFIX)/src/gnuwin32
	$(CP) -p Makefile.packages $(RPREFIX)/src/gnuwin32/Makefile
	$(CP) fixed/dobundle.pl $(RPREFIX)/src/gnuwin32/fixed
	$(CP) -pr help check $(RPREFIX)/src/gnuwin32
	$(RM) $(RPREFIX)/src/gnuwin32/help/*.c \
	  $(RPREFIX)/src/gnuwin32/help/MkChmDll
	$(MAKE) -C $(RPREFIX)/src/gnuwin32/help clean
	$(CP) -p $(foreach i,$(SRC_HEADERS) $(OBJ_HEADERS) $(GW32_HEADERS), \
	  ../include/$i) $(RPREFIX)/src/include
	$(CP) -pr ../include/R_ext $(RPREFIX)/src/include
	$(SED) -e 3,5d readme.packages > $(RPREFIX)/readme.packages
	$(CP) ../../COPYING.LIB $(RPREFIX)
	$(CP) -r ../../share/make ../../share/perl ../../share/texmf \
	   $(RPREFIX)/share
	$(CP) ../../bin/INSTALL ../../bin/REMOVE ../../bin/SHLIB \
	  ../../bin/build ../../bin/check ../../bin/massage-Examples \
	  ../../bin/Rd2dvi.sh ../../bin/Rd2txt ../../bin/Rdconv \
	  ../../bin/Rdindex ../../bin/Rdiff.sh \
	  ../../bin/Sd2Rd ../../bin/Rprof ../../bin/Rcmd.exe \
          $(RPREFIX)/bin
	$(CP) ../../share/make/tests.mk $(RPREFIX)/share/make
	$(CP) -p graphapp/graphapp.h graphapp/ga.h $(RPREFIX)/src/include
	$(MAKE) PKGDIR=. pkgclean-windlgs
	$(CP) -pr windlgs $(RPREFIX)/src/library
	-find $(RPREFIX) -name CVS -exec rm -rf \{\} \;
	zip -r9X $(RPREFIX)sp.zip $(RPREFIX)
	$(RM) -r $(RPREFIX)

docsdist: docs
	$(RM) -r $(RPREFIX)
	$(RM) -f $(RPREFIX)d1.zip $(RPREFIX)d2.zip
	$(MKDIR) $(RPREFIX) $(RPREFIX)/doc $(RPREFIX)/doc/manual
	$(CP) -pr $(RHOME)/doc/manual/*.pdf $(RPREFIX)/doc/manual
	zip -rm9X $(RPREFIX)d2.zip $(RPREFIX)/doc/manual/refman.pdf
	zip -rm9X $(RPREFIX)d1.zip $(RPREFIX)/doc/manual \
	  -x $(RPREFIX)/doc/manual/R-FAQ.pdf
	$(RM) -r $(RPREFIX)

## ============= End of maintainer targets ========================

# Dependencies
CSOURCES=$(wildcard *.c)
DEPS=$(CSOURCES:.c=.d)

makeMakedeps: $(DEPS)
	@$(RM) Makedeps
	@cat $(DEPS) >> Makedeps

-include Makedeps
