include MkRules

HELP = YES
WINHELP = BOTH# NO, CHM, BOTH

PACKAGES = base ctest eda lqs modreg mva nls splines stepfun ts
ALLPACKAGES = $(PACKAGES) tcltk
RPREFIX = rw1020

RHOME = $(shell perl pwd.pl ../..)# must be absolute path
RLIB = $(RHOME)/library
PKGDIR = $(RHOME)/src/library
PKGD = $(shell perl pwd.pl $(PKGDIR))# must be absolute path

ifdef DEBUG
 OPTFLAGS=-g -O2 -Wall
 DLLFLAGS=
else
 OPTFLAGS=-O2 -Wall -pedantic
 DLLFLAGS=-s
endif

R-DLLFLAGS = -mwindows
R-DLLLIBS = -L. $(FLIBS) -lwsock32 -lcomctl32 -lversion
SOURCES = $(filter-out malloc.c,$(wildcard *.c *.f))
OBJS = $(foreach i, $(SOURCES), $(basename $i).o)
MAINLIBS = libmain.a libappl.a libmath.a
EXTRALIBS = ga.a gl.a xdr.a em.a

LIBFILES = $(foreach i, $(ALLPACKAGES), ../../library/$i) ../../library/R.css
PKGS = $(foreach i,$(PACKAGES),pkg-$i)
PKGCLEAN = $(foreach i,$(ALLPACKAGES),pkgclean-$i)

LEA_MALLOC=YES
# flags to ensure that Doug Lea's malloc is used:
ifdef LEA_MALLOC
extra-CFLAGS+=-DLEA_MALLOC
OBJS-EXTRA=malloc.o
endif

.PHONY:  clean veryclean  rlibs fixfiles front-ends

all: rbuild rpackage rdocs

# separate as Tcl/Tk headers are needed.
tcl: 
	$(MAKE) STAMP=NO pkg-tcltk

CFLAGS = $(OPTFLAGS) -I../include -I. -DHAVE_CONFIG_H
FFLAGS = $(OPTFLAGS)

rbuild: fixfiles makeMakedeps rlibs ../../bin/R.dll libR.a CHTML \
	../include/globalvar.h front-ends COPYRIGHTS

rpackage: fixed/fixdesc ../../library/R.css
	$(MAKE) STAMP=NO $(PKGS)

rinstaller:
	$(MAKE) -C installer PREFIX=$(RPREFIX)

rlibs:
	$(MAKE) -C ../appl  OPTFLAGS='$(OPTFLAGS)' -f Makefile.win
	$(MAKE) -C ../main  OPTFLAGS='$(OPTFLAGS)' -f Makefile.win
	$(MAKE) -C ../nmath OPTFLAGS='$(OPTFLAGS)' -f Makefile.win
	$(MAKE) -C ./graphapp
	$(MAKE) -C ./getline
	$(MAKE) -C ./xdr
	$(MAKE) -C ./math


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

R1.a: R.a $(OBJS-EXTRA)
	$(CP) R.a R1.a
	$(AR) cr R1.a $(OBJS-EXTRA)
	$(RANLIB) R1.a

R.dll: R1.a $(MAINLIBS) $(EXTRALIBS) dllversion.o

Rdll: makeMakedeps ../../bin/R.dll

../../bin/R.dll: fixfiles rlibs R.dll
	$(MKDIR) -p ../../bin
	$(CP) R.dll ../../bin/

front-ends: libR.a
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

# avoid building globalvar.h with errors
../include/globalvar.h: exported-vars
	perl -n makeimps.pl exported-vars > $@

../../library/R.css: ../../doc/html/R.css
	$(CP) $< $@

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

bundle-%:
	$(PERL) fixed/dobundle.pl $* $(PKGD) $(RLIB)

pkg-%:  libR.a #../include/globalvar.h
	$(MAKE) DLLNM=$($*-DLLNM)  EXTRADOCS=$($*-EXTRADOCS) \
	  -C $(PKGDIR)/$* PKG=$* RHOME=$(RHOME) RLIB=$(RLIB) \
	  -f $(RHOME)/src/gnuwin32/MakePkg
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
	$(MAKE) DLLNM=$($*-DLLNM)  EXTRADOCS=$($*-EXTRADOCS) RHOME=$(RHOME) \
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
	$(MAKE) -C ./help RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) contents-$*

ziponly-%:
	$(MAKE) -C ./help RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) ziponlyhelp-$*
	$(MAKE) -C ./help RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) contents-$*

zipdata-%:
	(cd $(RLIB)/$*/data; ls -1 > filelist; \
	  zip -rmq9X Rdata * -x filelist 00Index)


#                          ===== cleaning ======

clean: cleanwin $(PKGCLEAN)
	$(RM) -f ../*/*.o $(WINOBJS) *.a ../*/*.d ../*/Makedeps fixed/fixdesc
	-$(MAKE) -C ../../doc/manual -f Makefile.win distclean
	-$(MAKE) -C ../nmath/standalone -f Makefile.win distclean

distclean veryclean: clean
	$(RM) -rf R.exp globalvar.h ../../bin ../../library
	$(RM) -f ../include/config.h ../include/psignal.h \
	../include/Rconfig.h ../include/version.h 
	$(RM) -f ../library/*/src/*.o ../library/*/src/*.a
	$(RM) -f ../library/*/src/*.dll
	$(RM) -rf ../library/*/check
	$(RM) -f ../../doc/html/search/index.txt \
	../../doc/html/function.html ../../doc/html/function-head.html \
	../../doc/html/packages.html ../../doc/html/rwin.html \
	../../doc/html/rw-FAQ.html ../../doc/html/search/SearchEngine.html
	$(RM) -f ../../etc/Rconsole ../../etc/Rdevga ../../etc/Rprofile \
	../../etc/rgb.txt ../../etc/Makeconf-tests \
	../include/FFDecl.h ../include/FFTab.h ../include/Platform.h \
	../include/globalvar.h ../../tests/*.Rout ../../tests/*.Rout.fail

cleanwin:
	$(RM) *.o *.exe *.dll *.a *~ \#*\# *.zip R.def R.exp1 \
	   .RData .Rhistory Makedeps *.d
	$(MAKE) -C graphapp clean
	$(MAKE) -C fixed clean
	$(MAKE) -C getline clean
	$(MAKE) -C help clean
	$(MAKE) -C front-ends clean
	$(MAKE) -C xdr clean
	$(MAKE) -C math clean
	$(MAKE) -C bitmap clean
	-$(MAKE) -C installer clean


#                          ===== testing ======

# tests
check test: test-Specific pkgtests

test-Specific:
	-@$(MAKE) -C $(RHOME)/tests -f $(RHOME)/src/gnuwin32/check/Maketests clean
	-@$(MAKE) -C $(RHOME)/tests -f $(RHOME)/src/gnuwin32/check/Maketests RHOME=$(RHOME)

PKGS-CHK = $(foreach i, $(PACKAGES), pkgcheck-$i)
pkgtests: $(PKGS-CHK)


#                          ===== documentation ======

docs manuals:
	@$(MAKE) -C $(RHOME)/doc/manual -f Makefile.win clean
	@$(MAKE) -C $(RHOME)/doc/manual -f Makefile.win R_PKGS='$(ALLPACKAGES)'
	@$(MAKE) ziponly-base


## ===================== Maintainer targets ========================

RVER = $(shell cut -d' ' -f1 ../../VERSION | sed -n 1p)

docfiles: INSTALL FAQs
SEDVER = -e s/@RVER@/$(RVER)/g -e s/@RWVER@/$(RPREFIX)/g

INSTALL: INSTALL.in ../../VERSION
	$(SED) $(SEDVER) INSTALL.in > INSTALL

FAQs: rw-faq fixed/r/rw-FAQ.html

fixed/r/rw-FAQ.html: rw-FAQ.texi ../../VERSION
	@$(SED) $(SEDVER) $< > tmp.texi
	makeinfo --html --no-headers --number-sections -o tmp.html tmp.texi
	-tidy tmp.html > $@ 2> nul
	@$(RM) tmp.texi tmp.html

rw-faq: rw-FAQ.texi ../../VERSION
	$(SED) $(SEDVER) $< | \
	  makeinfo --no-headers --number-sections -o $@


distribution: bindist packagedist docsdist

bindist: docfiles
	$(RM) -rf $(RPREFIX)
	$(MKDIR) $(RPREFIX)
	$(CP) -pr ../../bin ../../afm ../../demos  ../../unzip $(RPREFIX)
	$(MKDIR) $(RPREFIX)/library
	$(CP) -pr $(LIBFILES) $(RPREFIX)/library
	$(RM) $(RPREFIX)/*/Makefile.in
	(cd $(RPREFIX)/bin; $(RM) Rcmd.exe INSTALL REMOVE SHLIB build check \
	   massage-Examples.sh Rd2dvi.sh Rd2txt Rdconv Rdiff.sh Rdindex \
           extract-usage Sd2Rd)
	$(MKDIR) $(RPREFIX)/etc
	$(CP) -p ../../etc/Rprofile $(RPREFIX)/etc/Rprofile
	$(CP) -p ../../etc/Rconsole $(RPREFIX)/etc/Rconsole
	$(CP) -p ../../etc/Rdevga $(RPREFIX)/etc/Rdevga
	$(CP) -p ../../etc/rgb.txt $(RPREFIX)/etc/rgb.txt
	$(MKDIR) $(RPREFIX)/doc $(RPREFIX)/doc/manual
	$(CP) -pr ../../doc/html $(RPREFIX)/doc
	$(CP) -p ../../doc/manual/Rd.sty $(RPREFIX)/doc/manual
	$(CP) -p ../../doc/manual/*.html $(RPREFIX)/doc/manual
	$(CP) -p ../../AUTHORS ../../COPYING COPYRIGHTS ../../FAQ ../../NEWS \
		../../THANKS ../../Y2K $(RPREFIX)
	$(SED) -e s/@RWVER@/$(RPREFIX)/g \
	  -e s/@RVER@/$(RVER)/g readme > $(RPREFIX)/README
	$(CP) -p rw-faq $(RPREFIX)/rw-FAQ
	$(CP) -p CHANGES README.Rterm $(RPREFIX)
# convert docs and batch files to CRLF: might already be on Windows, though.
ifeq ($(strip $(BUILD)),CROSS)
	zip -l dosfiles.zip $(RPREFIX)/* $(RPREFIX)/bin/*.bat
	unzip -ao dosfiles.zip
else
	zip dosfiles.zip $(RPREFIX)/* $(RPREFIX)/bin/*.bat
	unzip -o dosfiles.zip
endif
	$(RM) dosfiles.zip
	$(RM) -f $(RPREFIX)b1.zip $(RPREFIX)b2.zip $(RPREFIX)h.zip \
	   $(RPREFIX)w.zip $(RPREFIX)l.zip $(RPREFIX)ch.zip
	$(RM) anindex.zip
	zip -mrX anindex.zip $(RPREFIX)/library/*/help/AnIndex $(RPREFIX)/library/*/help/00Titles
	zip -mr9X $(RPREFIX)h.zip $(RPREFIX)/library/*/help
	unzip anindex.zip
	$(RM) anindex.zip
	$(RM) $(RPREFIX)/doc/manual/R-FAQ.html
	zip -rm9X $(RPREFIX)w.zip $(RPREFIX)/doc/html \
          $(RPREFIX)/doc/manual/*.html \
	  $(RPREFIX)/library/*/html $(RPREFIX)/library/R.css
	-zip -rm9X $(RPREFIX)ch.zip $(RPREFIX)/library/*/chtml \
	  $(RPREFIX)/bin/Rchtml.dll
	$(RM) $(RPREFIX)/library/*/winhlp/*.GID
	-zip -rm9X $(RPREFIX)wh.zip $(RPREFIX)/library/*/winhlp
	zip -rm9X $(RPREFIX)l.zip $(RPREFIX)/library/*/latex $(RPREFIX)/doc/manual
	$(RM) -r $(RPREFIX)/doc
	zip -rm9X $(RPREFIX)b2.zip $(RPREFIX)/library
	zip -r9X $(RPREFIX)b1.zip $(RPREFIX)
	$(RM) -rf $(RPREFIX)


#taken from ../include/Makefile.in
SRC_HEADERS = R.h S.h Rdefines.h Rgraphics.h Rinternals.h Rmath.h
OBJ_HEADERS = Rconfig.h Rversion.h
GW32_HEADERS = globalvar.h

packagedist:
	$(RM) -r $(RPREFIX)sp.zip $(RPREFIX)
	$(MKDIR) $(RPREFIX) $(RPREFIX)/bin \
	  $(RPREFIX)/doc $(RPREFIX)/doc/html $(RPREFIX)/doc/manual \
	  $(RPREFIX)/src $(RPREFIX)/src/include $(RPREFIX)/src/library \
	  $(RPREFIX)/src/gnuwin32 $(RPREFIX)/src/gnuwin32/fixed
	$(CP) ../../doc/html/logo.jpg $(RPREFIX)/doc/html
	$(CP) -p ../../doc/manual/Rd.sty $(RPREFIX)/doc/manual
	$(CP) -p exported-vars R.exp MkRules MakePkg MakeDll \
	  maketitle pwd.pl makeDllRes.pl $(RPREFIX)/src/gnuwin32
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
	$(MKDIR) $(RPREFIX)/share
	$(CP) -r ../../share/perl $(RPREFIX)/share
	$(CP) ../../bin/INSTALL ../../bin/REMOVE ../../bin/SHLIB \
	  ../../bin/build ../../bin/check ../../bin/massage-Examples.sh \
	  ../../bin/Rd2dvi.sh ../../bin/Rd2txt ../../bin/Rdconv \
	  ../../bin/Rdindex ../../bin/Rdiff.sh ../../bin/Sd2Rd \
	  ../../bin/Rcmd.exe \
          $(RPREFIX)/bin
	$(CP) -p graphapp/graphapp.h $(RPREFIX)/src/include
	$(CP) -pr windlgs $(RPREFIX)/src/library
	zip -rX $(RPREFIX)sp.zip $(RPREFIX)
	$(RM) -r $(RPREFIX)

docsdist: docs
	$(RM) -rf $(RPREFIX)
	$(RM) -f $(RPREFIX)d1.zip $(RPREFIX)d2.zip
	$(MKDIR) $(RPREFIX) $(RPREFIX)/doc $(RPREFIX)/doc/manual
	$(CP) -pr $(RHOME)/doc/manual/*.pdf $(RPREFIX)/doc/manual
	zip -rm9X $(RPREFIX)d2.zip $(RPREFIX)/doc/manual/refman.pdf
	zip -rm9X $(RPREFIX)d1.zip $(RPREFIX)/doc/manual \
	  -x $(RPREFIX)/doc/manual/R-FAQ.pdf
	$(RM) -rf $(RPREFIX)

## ============= End of maintainer targets ========================

# Dependencies
CSOURCES = $(wildcard *.c)
DEPS = $(foreach i, $(CSOURCES), $(basename $i).d)

.c.d:
	@echo "making $@ from $<"
	@$(CC)  -M $(CFLAGS) $($*-CFLAGS) $< -o $@

makeMakedeps: $(DEPS)
	@$(RM) Makedeps
	@cat $(DEPS) >> Makedeps

-include Makedeps
