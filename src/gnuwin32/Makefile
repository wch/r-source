include MkRules
include RWVERSION

## This is done so Rpwd.exe gets built first under a native build,
## but it is not used in Makefile under a cross-build.
ifneq ($(strip $(BUILD)),CROSS)
ifneq ($(strip $(wildcard Rpwd.exe)),)
all: Rpwd.exe rbuild rpackage rdocs
else
all: Rpwd.exe
	$(MAKE) rbuild rpackage rdocs
endif
else
all: Rpwd.exe rbuild rpackage rdocs
endif

PACKAGES = base ctest eda lqs methods modreg mva nls splines stepfun tools ts
ALLPACKAGES = $(PACKAGES) tcltk

## must be absolute and space-free paths
ifneq ($(strip $(BUILD)),CROSS)
RHOME = $(shell ./Rpwd.exe ../..)
PKGD = $(shell ./Rpwd.exe $(PKGDIR))
else
RHOME = $(shell perl pwd.pl ../..)
PKGD = $(shell perl pwd.pl $(PKGDIR))
endif

RLIB = $(RHOME)/library
PKGDIR = $(RHOME)/src/library

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
FLIBS+=-lRblas
SOURCES = $(filter-out malloc.c blas00.c,$(wildcard *.c *.f))
OBJS = $(SOURCES:.c=.o)
MAINLIBS = libmain.a libappl.a libmath.a
EXTRALIBS = ga.a gl.a xdr.a ../extra/zlib/libz.a ../extra/pcre/libRpcre.a\
   ../extra/bzip2/libbz2.a

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
	$(MAKE) pkg-tcltk

CFLAGS = $(OPTFLAGS) -I../include -I. -DHAVE_CONFIG_H -DR_DLL_BUILD
FFLAGS = $(OPTFLAGS)

Rpwd.exe: front-ends/Rpwd.exe
	$(CP) $< $@

front-ends/Rpwd.exe:
	$(MAKE) -C front-ends Rpwd

## watch out: R.dll and Rblas.dll depend on each other.
rbuild: fixfiles rlibs makeMakedeps libRblas.a ../../bin/R.dll libR.a \
	 ../../bin/Rblas.dll CHTML front-ends COPYRIGHTS rmodules

rpackage: fixed/fixdesc ../../library/R.css
	$(MAKE) $(PKGS)

rinstaller:
	$(MAKE) -C installer
	$(MAKE) -C installer clean

rlibs:
# need pcre.h built before main is built
	$(MAKE) -C ../extra/pcre -f Makefile.win
	$(MAKE) -C ../appl  OPTFLAGS='$(OPTFLAGS)' -f Makefile.win
	$(MAKE) -C ../main  OPTFLAGS='$(OPTFLAGS)' -f Makefile.win
	$(MAKE) -C ../nmath OPTFLAGS='$(OPTFLAGS)' -f Makefile.win
	$(MAKE) -C ./graphapp
	$(MAKE) -C ./getline
	$(MAKE) -C ../extra/xdr -f Makefile.win

rmodules: libRm.a
	$(MAKE) -C ../modules OPTFLAGS='$(OPTFLAGS)' FOPTFLAGS='$(FOPTFLAGS)' \
	  -f Makefile.win

COPYRIGHTS: $(RHOME)/COPYRIGHTS COPYRIGHTS.win
	cat $^ > $@


fixfiles:
	$(MKDIR) -p ../../bin
	$(MAKE) -C ./fixed

fixed/fixdesc:
	sh fixed/GETDESC $(ALLPACKAGES)

dllversion.o: ../include/Rversion.h

R.dll: R.a $(OBJS-EXTRA) $(MAINLIBS) $(EXTRALIBS) dllversion.o

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

# An additional import library needed for the modules that uses R.dll's malloc/.../free
libRm.a:  Rm.exp
	$(DLLTOOL) $(DLLTOOLFLAGS) $(R-DLLTOOLFLAGS) --dllname R.dll --def Rm.exp --output-lib libRm.a

Rm.exp: Rm.exp1
	(diff Rm.exp Rm.exp1 > /dev/null) || $(CP) Rm.exp1 Rm.exp

Rm.exp1: $(OBJS-EXTRA)
	$(ECHO) EXPORTS > $@
	$(NM) $(OBJS-EXTRA) > RDefs
	$(SED) -n '/^........ [T] _/s/^........ [T] _/ /p' RDefs >> $@
	$(RM) RDefs

../../library/R.css: ../../doc/html/R.css
	$(CP) $< $@

../extra/zlib/libz.a:
	$(MAKE) -C ../extra/zlib -f Makefile.win

../extra/pcre/libRpcre.a:
	$(MAKE) -C ../extra/pcre -f Makefile.win

../extra/bzip2/libbz2.a:
	$(MAKE) -C ../extra/bzip2  -f Makefile.win libbz2.a

rdocs:
	@$(MAKE) -C ../../doc/manual -f Makefile.win html BUILD=$(BUILD)


#                          ===== BLAS ======

Rblas: ../../bin/Rblas.dll

Rblas-clean:
	$(RM) ../../bin/Rblas.dll blas00.o blas00.d blas.o cmplxblas.o

Rblas-DLLLIBS=-L. -lR

libRblas.a:  Rblas.def
	$(DLLTOOL) $(DLLTOOLFLAGS) $(R-DLLTOOLFLAGS) --dllname Rblas.dll \
	  --def Rblas.def --output-lib libRblas.a

ifeq ($(strip $(USE_ATLAS)),YES)
../../bin/Rblas.dll: blas00.o dllversion.o
	@$(ECHO) -------- Building $@ --------
	$(DLL) -shared $(DLLFLAGS) -o $@ $^ Rblas.def -L $(ATLAS_PATH) \
	   -L. -lR -lg2c -lf77blas -latlas
## suppress lots of warnings: this is a dummy
blas00.o: blas00.c
	$(CC) -O2  -c $< -o $@
else
../../bin/Rblas.dll: blas.o cmplxblas.o dllversion.o
	@$(ECHO) -------- Building $@ --------
	$(DLL) -shared $(DLLFLAGS) -o $@ Rblas.def $^ -L. -lR

blas.o: ../appl/blas.f
	$(F77) $(FFLAGS) -c $< -o $@
cmplxblas.o: ../modules/lapack/cmplxblas.f
	$(F77) $(FOPTFLAGS) -c $< -o $@
endif


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
methods-HELP = ziponly
nls-HELP = ziponly
ts-HELP = ziponly
base-ZIPDATA = zip
ts-ZIPDATA = zip

MASS-HELP = ziponly
MASS-ZIPDATA = zip
boot-HELP = ziponly
boot-ZIPDATA = zip
cluster-HELP = ziponly
cluster-ZIPDATA = zip
grid-HELP = ziponly
grid-ZIPDATA = zip
lattice-HELP = ziponly
lattice-ZIPDATA = zip
nlme-HELP = ziponly
nlme-ZIPDATA = zip
rpart-HELP = ziponly
rpart-ZIPDATA = zip
survival-HELP = ziponly
survival-ZIPDATA = zip

bundle-%:
	$(PERL) fixed/dobundle.pl $* $(PKGD) $(RLIB)

pkg-%:
	$(MAKE) DLLNM=$($*-DLLNM)  FLIBS="$(FLIBS)" BUILD=$(BUILD) \
	  -C $(PKGDIR)/$* PKG=$* RHOME=$(RHOME) RLIB=$(RLIB) RVER=$(RVER) \
	  -f $(RHOME)/src/gnuwin32/MakePkg
	@if test -d $(RLIB)/$*/data ; then \
	  $(MAKE) $($*-ZIPDATA)data-$* ; \
	fi
ifeq ($(strip $(HELP)),YES)
	@$(MAKE) -C ./help RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) $($*-HELP)help-$*
	@$(MAKE) -C ./help RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) contents-$*
ifeq ($(strip $(WINHELP)),CHM)
	@$(MAKE) -C ./help RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) chm-$*
endif
ifeq ($(strip $(WINHELP)),BOTH)
	@$(MAKE) -C ./help RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) winhlp-$* # also makes chm
endif
endif

pkgclean-%:
	$(MAKE)  --no-print-directory DLLNM=$($*-DLLNM) RHOME=$(RHOME) BUILD=$(BUILD) \
	  -C $(PKGDIR)/$* PKG=$* -f $(RHOME)/src/gnuwin32/MakePkg clean

pkgcheck-%:
	@$(ECHO) -------- Testing package $* --------
	-@mkdir -p $(PKGDIR)/$*/check
	@$(MAKE) --no-print-directory -C $(PKGDIR)/$*/check PKG=$* RHOME=$(RHOME) RLIB=$(RLIB) \
	  -f $(RHOME)/src/gnuwin32/check/PkgCheck
	@if test -d $(PKGDIR)/$*/tests ; then \
	  $(MAKE) --no-print-directory -C $(PKGDIR)/$*/tests PKG=$* RHOME=$(RHOME) RLIB=$(RLIB) \
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
	for p in $(REC_PKGS); do \
	  $(RM) -rf ../library/$$p; \
	done

distclean veryclean: clean clean-recommended
	$(RM) -r ../../bin ../../library ../../modules
	$(RM) ../include/config.h ../include/psignal.h \
	../include/Rconfig.h ../include/Rversion.h ../include/Rmath.h
	$(RM) ../library/*/src/*.o ../library/*/src/*.a
	$(RM) ../library/*/src/*.d ../library/*/src/Makedeps
	$(RM) ../library/*/src/*.dll
	$(RM) -r ../library/*/check
	$(RM) R.exp Rm.exp COPYRIGHTS
	$(RM) ../../doc/html/search/index.txt ../../doc/html/index.html \
	../../doc/html/packages.html ../../doc/html/rwin.html \
	../../doc/html/rw-FAQ.html ../../doc/html/search/SearchEngine.html
	$(RM) ../../etc/Rconsole ../../etc/Rdevga ../../etc/Rprofile \
	../../etc/rgb.txt ../../share/make/wintests.mk \
	../include/Platform.h ../../tests/*.Rout ../../tests/*.Rout.fail
	$(MAKE) -C fixed distclean
	-$(MAKE) -C installer distclean
	@for pkg in $(ALLPACKAGES); do \
	  $(RM) ../library/$${pkg}/DESCRIPTION; \
	done
	$(RM) Rpwd.exe # do this last of all

cleanwin:
	$(RM) *.o *.dll *.a *~ \#*\# rw*.zip R.def R.exp1 Rm.exp1\
	   .RData .Rhistory Makedeps *.d
	$(MAKE) -C graphapp clean
	$(MAKE) -C fixed clean
	$(MAKE) -C getline clean
	$(MAKE) -C help clean
	$(MAKE) -C front-ends clean
	$(MAKE) -C ../extra/xdr -f Makefile.win clean
	$(MAKE) -C ../extra/pcre -f Makefile.win distclean
	$(MAKE) -C ../extra/bzip2 -f Makefile.win clean
	$(MAKE) -C bitmap clean
	-$(MAKE) -C installer clean


#                          ===== testing ======

# tests
check test: test-Specific pkgtests

test-Specific:
	-@$(MAKE) -C $(RHOME)/tests -f $(RHOME)/tests/Makefile.win clean
	-@$(MAKE) -C $(RHOME)/tests -f $(RHOME)/tests/Makefile.win RHOME=$(RHOME)


PKGS-CHK = $(foreach i, $(PACKAGES), pkgcheck-$i)
pkgtests: $(PKGS-CHK)

fullcheck: check check-recommended

REC_PKGS-CHK = $(foreach i, $(REC_PKGS), pkgcheck-$i)
check-recommended: $(REC_PKGS-CHK)

#                          ===== documentation ======

docs manuals:
	@$(MAKE) -C $(RHOME)/doc/manual -f Makefile.win clean BUILD=$(BUILD)
	@$(MAKE) -C $(RHOME)/doc/manual -f Makefile.win \
	  R_PKGS='$(ALLPACKAGES)' BUILD=$(BUILD)
	@$(MAKE) ziponly-base


## ===================== Maintainer targets ========================

RVER = $(shell cut -d' ' -f1 ../../VERSION | sed -n 1p)

docfiles:
	$(MAKE) -f Makefile.docfiles

bitmapdll:
	$(MAKE) -C bitmap

distribution: bindist packagedist docsdist

bindist: tcl bitmapdll docfiles
	$(RM) -r $(RPREFIX)
	$(MKDIR) $(RPREFIX)
	$(CP) -pr ../../bin ../../modules ../../afm $(RPREFIX)
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
	find $(RPREFIX) -name CVS -prune -exec rm -rf \{\} \;
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
SRC_HEADERS = R.h S.h Rdefines.h Rdevices.h Rgraphics.h Rinternals.h Rmath.h
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
	$(CP) -p exported-vars R.exp Rblas.def MkRules MakePkg MakeDll \
	  Rpwd.exe pwd.pl makeDllRes.pl install-save.sh $(RPREFIX)/src/gnuwin32
	$(SED) -e s/@RVER@/$(RVER)/g Makefile.packages > $(RPREFIX)/src/gnuwin32/Makefile
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
	$(CP) ../../share/make/wintests.mk $(RPREFIX)/share/make
	$(CP) -p graphapp/graphapp.h graphapp/ga.h $(RPREFIX)/src/include
	$(MAKE) PKGDIR=. pkgclean-windlgs
	$(CP) -pr windlgs $(RPREFIX)/src/library
	find $(RPREFIX) -name CVS -prune -exec rm -rf \{\} \;
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

install:
	cp *.zip /pub/bdr/RWin/Rpre

CRANREC = cran.r-project.org::CRAN/src/contrib/$(RVER)/Recommended

rsync-recommended:
	(cd ../library; \
	rsync -rvCt --delete --exclude=Makefile.in --exclude=.cvsignore \
	  --include=*.tar.gz $(CRANREC) . ); \
	(cd ../library/Recommended; for i in ${RECOMMENDED} ; do cp -p $${i}*.tar.gz $${i}_tgz ; done)

../library/%/DESCRIPTION: ../library/RECOMMENDED/%_tgz
	@echo "---- $*"
	(cd ../library; tar zxf Recommended/$*_tgz; touch $@)

unpack-recommended:
	@echo "--- Unpacking recommended packages"
	@for i in ${RECOMMENDED} ; do $(MAKE) -s --no-print-directory ../library/$$i/DESCRIPTION ; done

clean-recommended:
	@for i in ${RECOMMENDED} ; do $(RM) -r ../library/$${i} ; done

recommended: unpack-recommended
	@echo "--- Making recommended packages"
	@$(MAKE) --no-print-directory bundle-VR
	@for p in $(REC_NONBUNDLES); do \
	  $(MAKE) --no-print-directory pkg-$$p; \
	done

dist-recommended:
	@for p in $(REC_PKGS); do \
	(cd ../..; zip -r9X src/gnuwin32/Recommended.zip library/$$p) ; \
	done

## ============= End of maintainer targets ========================

# Dependencies
CSOURCES=$(filter-out malloc.c blas00.c,$(wildcard *.c))
DEPS=$(CSOURCES:.c=.d)

makeMakedeps: $(DEPS)
	@$(RM) Makedeps
	@cat $(DEPS) >> Makedeps

-include Makedeps
