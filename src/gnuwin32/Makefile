include MkRules

HELP=YES
WINHELP=BOTH # NO, CHM, BOTH

PACKAGES=base eda lqs modreg mva nls splines stepfun ts
RPREFIX=rw0901

RHOME=$(shell cd ../..; pwd)# must be absolute path
RLIB=$(RHOME)/library
PKGDIR=$(RHOME)/src/library
PKGD=$(shell cd $(PKGDIR); pwd)# must be absolute path

ifdef DEBUG
 OPTFLAGS=-g -O2 -Wall
 DLLFLAGS=
else
 OPTFLAGS=-O2 -Wall -pedantic
 DLLFLAGS=-s 
endif

R-DLLFLAGS= $(STRIPFLAG) -mwindows
R-DLLLIBS=-L. $(FLIBS) -lwsock32 -lcomctl32
SOURCES=$(wildcard *.c *.f)
OBJS=$(foreach i, $(SOURCES), $(basename $i).o)
MAINLIBS=libmain.a libappl.a libmath.a
EXTRALIBS=rx.a ga.a gl.a xdr.a em.a

LIBFILES=$(foreach i, $(PACKAGES), ../../library/$i)
PKGS=$(foreach i, $(PACKAGES), pkg-$i)
PKGCLEAN=$(foreach i, $(PACKAGES), pkgclean-$i)

.PHONY:  clean veryclean  rlibs fixfiles front-ends

all: rbuild rdemo rpackage

CFLAGS=$(OPTFLAGS) -I../include -I.
FFLAGS=$(OPTFLAGS)

rbuild: fixfiles makeMakedeps rlibs ../../bin/R.dll libR.a CHTML \
	../include/globalvar.h front-ends COPYRIGHTS

rpackage: fixed/fixdesc $(PKGS)

rinstaller:
	$(MAKE) -C installer PREFIX=$(RPREFIX)

rdemo:  libR.a
	make -C ../../demos/dynload -f $(RHOME)/src/gnuwin32/MkDyndemo

rlibs: 
	$(MAKE) -C ../appl OPTFLAGS="$(OPTFLAGS)" -f Makefile.win
	$(MAKE) -C ../main OPTFLAGS="$(OPTFLAGS)" -f Makefile.win
	$(MAKE) -C ../nmath OPTFLAGS="$(OPTFLAGS)" -f Makefile.win
	$(MAKE) -C ./regex
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
	sh fixed/GETDESC $(PACKAGES)

R.dll: R.a $(MAINLIBS) $(EXTRALIBS)

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
# only (all functions) + (global vars listed  in exported-vars))

libR.a:  R.exp
	$(DLLTOOL) $(DLLTOOLFLAGS) $(R-DLLTOOLFLAGS) --dllname R.dll --def R.exp --output-lib libR.a

# This fails on djtools make:
R.exp: R.exp1
	(diff R.exp R.exp1 > /dev/null) || $(CP) R.exp1 R.exp

R.exp1: R.a $(MAINLIBS) $(EXTRALIBS) exported-vars
	$(ECHO) EXPORTS > $@
	$(NM) R.a $(MAINLIBS) $(EXTRALIBS) > RDefs
	$(SED) -n "/^........ [T] _/s/^........ [T] _/ /p" RDefs >> $@
	$(CAT) exported-vars >> $@
	$(RM) RDefs

# avoid building globalvar.h with errors
../include/globalvar.h: exported-vars
	$(AWK) -- '//{if ($$1 != "") print "#define " $$1 " (*__imp_" $$1 ")"}' \
	           exported-vars > ../include/globalvar.h

CHTML:
ifeq ($(strip $(HELP)),YES)
ifeq ($(strip $(WINHELP)),CHM)
	$(MAKE) -C ./help -f MkChmDll
endif
ifeq ($(strip $(WINHELP)),BOTH)
	$(MAKE) -C ./help -f MkChmDll
endif
endif


base-HELP=ziponly

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

ziphelp-%:
	$(MAKE) -C ./help RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) ziphelp-$*
	$(MAKE) -C ./help RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) contents-$*

ziponly-%:
	$(MAKE) -C ./help RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) ziponlyhelp-$*
	$(MAKE) -C ./help RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) contents-$*

bootstrap-DLLNM=boott

clean: cleanwin $(PKGCLEAN)
	$(RM) -f ../*/*.o $(WINOBJS) *.a ../*/*.d ../*/Makedeps fixed/fixdesc
	-$(MAKE) -C ../../doc/manual -f Makefile.win distclean

distclean veryclean: clean
	$(RM) -rf R.exp ../../bin ../../library
	$(RM) -f ../library/*/src/*.o ../library/*/src/*.a
	$(RM) -f ../library/*/src/*.def ../library/*/src/*.dll
	$(RM) -rf ../library/*/check
	$(RM) -f ../../doc/html/search/index.txt \
	../../doc/html/function.html ../../doc/html/packages.html \
	../../demos/dynload/zero.def ../../demos/dynload/zero.dll \
	../../demos/dynload/zero.o \
	../../etc/R* ../../etc/rgb.txt \
	../include/FFDecl.h ../include/FFTab.h ../include/Platform.h \
	../include/globalvar.h ../../tests/*.Rout ../../tests/*.Rout.fail

cleanwin:
	$(RM) *.o *.exe *.dll *.a *~ \#*\# *.zip R.def R.exp1 \
	.RData .Rhistory Makedeps *.d
	$(MAKE) -C graphapp clean
	$(MAKE) -C fixed clean
	$(MAKE) -C regex clean
	$(MAKE) -C getline clean
	$(MAKE) -C help clean
	$(MAKE) -C front-ends clean
	$(MAKE) -C xdr clean
	$(MAKE) -C math clean
	$(MAKE) -C bitmap clean
	-$(MAKE) -C installer clean


bindist:
	$(RM) -rf $(RPREFIX)
	$(MKDIR) $(RPREFIX)
	$(CP) -pr ../../bin ../../afm ../../demos  ../../unzip $(RPREFIX)
	$(MKDIR) $(RPREFIX)/library
	$(CP) -pr $(LIBFILES) $(RPREFIX)/library
	$(RM) $(RPREFIX)/demos/dynload/*.o $(RPREFIX)/demos/dynload/*.def
	$(RM) $(RPREFIX)/*/Makefile.in
	$(MKDIR) $(RPREFIX)/etc
	$(CP) -p ../../etc/Rprofile $(RPREFIX)/etc/Rprofile
	$(CP) -p ../../etc/Rconsole $(RPREFIX)/etc/Rconsole
	$(CP) -p ../../etc/Rdevga $(RPREFIX)/etc/Rdevga
	$(CP) -p ../../etc/rgb.txt $(RPREFIX)/etc/rgb.txt
	$(MKDIR) $(RPREFIX)/doc $(RPREFIX)/doc/manual
	$(CP) -pr ../../doc/html $(RPREFIX)/doc
	$(CP) -p ../../doc/manual/Rd.sty $(RPREFIX)/doc/manual
	$(CP) -p ../../COPYING COPYRIGHTS ../../FAQ ../../NEWS \
		../../Y2K $(RPREFIX)
	$(CP) -p readme $(RPREFIX)/README
	$(CP) -p rw-faq $(RPREFIX)/rw-FAQ
#	$(CP) -p Announce $(RPREFIX)/Announce
	$(CP) -p CHANGES $(RPREFIX)/CHANGES
	zip docs.zip $(RPREFIX)/*
	unzip -a -o docs.zip
	$(RM) docs.zip
	$(RM) -rf $(RPREFIX)b.zip $(RPREFIX)h.zip $(RPREFIX)w.zip $(RPREFIX)l.zip
	$(RM) anindex.zip
	zip -mrX anindex.zip $(RPREFIX)/library/*/help/AnIndex $(RPREFIX)/library/*/help/00Titles
	zip -mr9X $(RPREFIX)h.zip $(RPREFIX)/library/*/help 
	unzip anindex.zip
	$(RM) anindex.zip
	zip -rm9X $(RPREFIX)w.zip $(RPREFIX)/doc/html $(RPREFIX)/library/*/html
	-zip -rm9X $(RPREFIX)ch.zip $(RPREFIX)/library/*/chtml  $(RPREFIX)/bin/Rchtml.dll
	$(RM)  $(RPREFIX)/library/*/winhlp/*.GID
	-zip -rm9X $(RPREFIX)wh.zip $(RPREFIX)/library/*/winhlp
	zip -rm9X $(RPREFIX)l.zip $(RPREFIX)/library/*/latex $(RPREFIX)/doc/manual
	$(RM) -r $(RPREFIX)/doc
	zip -rm9X $(RPREFIX)b2.zip $(RPREFIX)/library
	zip -r9X $(RPREFIX)b1.zip $(RPREFIX)
	$(RM) -rf $(RPREFIX)



srcdist:
	$(RM) -r ../src src
	$(MKDIR) ../src
	$(MKDIR)  ../src/gnuwin32
	$(CP)  -pr ./* ../src/gnuwin32
	$(RM) -r ../src/gnuwin32/old ../src/gnuwin32/*.zip
	$(RM) -r ../src/gnuwin32/installer/unzip
	$(RM) -r ../src/gnuwin32/bitmap/jpeg-6b
	$(RM) -r ../src/gnuwin32/bitmap/libpng
	$(RM) -r ../src/gnuwin32/bitmap/zlib 
	$(RM) -f ../src/gnuwin32/unzip/*.dll
	$(MAKE) -k -C ../src/gnuwin32 cleanwin
	mv ../src .
	$(RM) $(RPREFIX)s.zip
	zip -rX  $(RPREFIX)s.zip src
	$(RM) -r src


#taken from ../include/Makefile.in
SRC_HEADERS = Applic.h Arith.h Blas.h Complex.h Error.h Errormsg.h \
	Fortran.h Linpack.h Mathlib.h Memory.h PrtUtil.h Random.h \
	Rdefines.h Rinternals.h S.h S_compat.h Utils.h f2c.h
OBJ_HEADERS = Rconfig.h Rversion.h
GW32_HEADERS = globalvar.h psignal.h

packagedist:
	$(RM) -r $(RPREFIX)sp.zip $(RPREFIX)
	$(MKDIR) $(RPREFIX) $(RPREFIX)/src $(RPREFIX)/src/include 
	$(MKDIR) $(RPREFIX)/src/library $(RPREFIX)/src/gnuwin32 
	$(CP) -p ../../COPYING exported-vars R.exp MkRules MakePkg MakeDll $(RPREFIX)/src/gnuwin32
	$(CP) -p Makefile.packages $(RPREFIX)/src/gnuwin32/Makefile
	$(MKDIR) $(RPREFIX)/src/gnuwin32/fixed
	$(CP) fixed/dobundle.pl $(RPREFIX)/src/gnuwin32/fixed
	$(CP) -pr help check $(RPREFIX)/src/gnuwin32
	$(RM) $(RPREFIX)/src/gnuwin32/help/*.c
	$(MAKE) -C $(RPREFIX)/src/gnuwin32/help clean
	$(CP) -p $(foreach i,$(SRC_HEADERS) $(OBJ_HEADERS) $(GW32_HEADERS),../include/$i) $(RPREFIX)/src/include
	$(CP) readme.packages $(RPREFIX)
	zip -rX $(RPREFIX)sp.zip $(RPREFIX)
	$(RM) -r $(RPREFIX)


distribution: bindist packagedist docsdist

# tests
check: all test
test: stricttests pkgtests

stricttests:
	-@$(MAKE) -C $(RHOME)/tests -f $(RHOME)/src/gnuwin32/check/Maketests clean
	-@$(MAKE) -k -C $(RHOME)/tests -f $(RHOME)/src/gnuwin32/check/Maketests

PKGS-CHK=$(foreach i, $(PACKAGES), pkgcheck-$i)
pkgtests: $(PKGS-CHK)

pkgcheck-base:
	@$(ECHO) -------- Testing package base --------
	@(cd ../../library/base/R-ex; unzip -oq Rex)
	-@mkdir -p ../library/base/check
	-@$(MAKE) -C ../library/base/check \
	  PKG=base RHOME=$(RHOME) RLIB=$(RLIB) \
	  -f ../../../gnuwin32/check/PkgCheck
	@(cd ../../library/base/R-ex; rm -f *.R)

bundle-%:
	$(PERL) fixed/dobundle.pl $* $(PKGD) $(RLIB)

docs manuals:
	@$(MAKE) -C $(RHOME)/doc/manual -f Makefile.win clean
	@$(MAKE) -C $(RHOME)/doc/manual -f Makefile.win R_PKGS="$(PACKAGES)"
	$(RM) $(RHOME)/library/base/latex/*.tex

docsdist:
	$(RM) -rf $(RPREFIX)
	$(MKDIR) $(RPREFIX) $(RPREFIX)/doc $(RPREFIX)/doc/manuals 
	$(CP) -pr $(RHOME)/doc/manual/*.pdf $(RPREFIX)/doc/manuals
	zip -rm9X $(RPREFIX)d.zip $(RPREFIX)


# Dependencies
CSOURCES=$(wildcard *.c)
DEPS=$(foreach i, $(CSOURCES), $(basename $i).d)

.c.d:
	@echo "making $@ from $<"
	@$(CC)  -M $(CFLAGS) $($*-CFLAGS) $< -o $@

makeMakedeps: $(DEPS)
	@$(RM) Makedeps
	@cat $(DEPS) >> Makedeps

-include Makedeps
