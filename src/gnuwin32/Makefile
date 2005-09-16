include MkRules
include ../../share/make/vars.mk

all:
	@$(MAKE) --no-print-directory Rpwd.exe
	@if test -f  ../../SVN-REVISION ; then \
	  cp ../../SVN-REVISION ../../SVN-REVISION.bak ; \
	fi
	@$(MAKE) --no-print-directory rbuild
	@$(MAKE) --no-print-directory rpackage
	@$(MAKE) --no-print-directory htmldocs
	@$(MAKE) --no-print-directory  -C ../../po -f Makefile.win

## must be absolute and space-free paths
ifneq ($(strip $(BUILD)),CROSS)
RHOME = $(shell ./Rpwd.exe ../..)
PKGD = $(shell ./Rpwd.exe "$(PKGDIR)")
R_EXE = $(RHOME)/bin/rterm.exe
else
RHOME = $(shell perl pwd.pl ../..)
PKGD = $(shell perl pwd.pl $(PKGDIR))
endif

REXE = $(R_EXE) --vanilla --slave

RPREFIX = $(shell perl fixed/rwver.pl ../../VERSION)

FULLVERSION=`cat ../../VERSION`

DATE = $(shell date +%F)

RLIB = $(RHOME)/library
PKGDIR = $(RHOME)/src/library

OPTFLAGS=-O2 -Wall -pedantic --std=gnu99
FOPTFLAGS=-O2 -Wall
ifdef DEBUG
 OPTFLAGS+=-g
 FOPTFLAGS+=-g
 DLLFLAGS=
else
 DLLFLAGS=-s
endif
OPTFLAGS_M=$(OPTFLAGS)

ifneq ($(strip $(BUILD)),CROSS)
R-DLLFLAGS = -mwindows
else
R-DLLFLAGS = -mwindows
endif

## we may not need msvcp60, but no harm to include it.
R-DLLLIBS =-L. $(FLIBS) -lRblas -lcomctl32 -lversion -lmsvcp60 $(LIBEXTRAS)
dllversion-RESFLAGS =--include-dir ../include
SOURCES = \
  console.c dataentry.c dynload.c edit.c editor.c extra.c \
  opt.c pager.c preferences.c psignal.c rhome.c rui.c run.c shext.c \
  sys-win32.c system.c
OBJS = $(SOURCES:.c=.o) e_pow.o
MAINLIBS = ../main/libmain.a ../appl/libappl.a ../nmath/libnmath.a
EXTRALIBS = graphapp/ga.a getline/gl.a ../extra/xdr/libxdr.a \
   ../extra/zlib/libz.a ../extra/pcre/libpcre.a ../extra/bzip2/libbz2.a \
   ../extra/intl/libintl.a

R_PKGS0 = $(filter-out base tools, $(R_PKGS_BASE))
PKGCLEAN = $(foreach i,$(R_PKGS_BASE),pkgclean-$i)

# flags to ensure that Doug Lea's malloc is used:
ifdef LEA_MALLOC
extra-CFLAGS+=-DLEA_MALLOC
OBJS-EXTRA=malloc.o
OPTFLAGS_M+=-DLEA_MALLOC
endif

.PHONY:  clean veryclean  rlibs fixfiles front-ends rmodules


CFLAGS = $(OPTFLAGS) -I../include -I. -DHAVE_CONFIG_H -DR_DLL_BUILD
FFLAGS = $(FOPTFLAGS)

Rpwd.exe: front-ends/Rpwd.exe
	$(CP) $< $@

front-ends/Rpwd.exe:
	$(MAKE) --no-print-directory -C front-ends Rpwd

## watch out: R.dll and Rblas.dll depend on each other.
rbuild:
	@$(MAKE) --no-print-directory fixfiles
	@$(MAKE) --no-print-directory -C ../include -f Makefile.win
	@$(MAKE) --no-print-directory -C ../scripts -f Makefile.win
	@$(MAKE) --no-print-directory libRblas.a
	@$(MAKE) --no-print-directory ../../bin/R.dll
	@$(MAKE) --no-print-directory libR.a
	@$(MAKE) --no-print-directory ../../bin/Rblas.dll CHTML
	@$(MAKE) --no-print-directory front-ends COPYRIGHTS rmodules Rcode

## used to build skeletal versions of packages, enough to run
## package tools to install them properly
INITPACKAGES = base tools
Rcode: fixed/fixdesc
	@$(ECHO) "  making bootstrap versions of packages ..."
	@for pkg in $(INITPACKAGES); do \
	  if ! test -f $(RHOME)/library/$${pkg}/DESCRIPTION ; then \
	  $(ECHO) "  --" $$pkg; \
	  $(MAKE) --no-print-directory -C $(PKGDIR)/$${pkg} PKG=$${pkg} \
	    RHOME=$(RHOME) -f $(RHOME)/src/gnuwin32/MakePkg BUILD=$(BUILD) Rcode  || exit 1; \
	  fi; \
	done
	@$(ECHO) "  ... done"

# methods-LAZY is done directly
graphics-LAZY=true
grDevices-LAZY=true
grid-LAZY=true
splines-LAZY=true
stats-LAZY=true
tcltk-LAZY=true
utils-LAZY=true
datasets-LAZYDATA=true

rpackage: ../../library/R.css rpackageindices
	@R_START_PACKAGES=NULL $(MAKE) --no-print-directory pkg-base
ifneq ($(strip $(BUILD)),CROSS)
	@if ! test -f ../../library/base/R/base.rdb ; then \
	  echo "preparing package base for lazy loading" ; \
	  (echo ".libPaths('../../library')"; \
	    cat ../library/base/makebasedb.R) | \
	    R_DEFAULT_PACKAGES=NULL LC_COLLATE=C $(REXE) > /dev/null; \
          cp ../library/base/baseloader.R ../../library/base/R/base; \
	fi
endif
	@R_START_PACKAGES=NULL $(MAKE) --no-print-directory pkg-tools
	@if ! test -f ../../library/tools/R/tools.rdb ; then \
	  echo "preparing package tools for lazy loading" ; \
	  (cat ../library/tools/R/makeLazyLoad.R; \
	    echo "makeLazyLoading(\"tools\", lib.loc=\"$(RLIB)\")") | \
	    R_DEFAULT_PACKAGES=NULL LC_COLLATE=C $(R_EXE) --vanilla --slave > /dev/null ; \
	fi
	@$(ECHO)
	@for pkg in $(R_PKGS0); do \
	  R_START_PACKAGES=NULL $(MAKE) --no-print-directory pkg-$${pkg} || exit 1; \
	done
	@$(CP) ../library/datasets/data/morley.tab ../../library/datasets/data

rpackageindices:
ifeq ($(strip $(HELP)),YES)
	@$(ECHO)
	@$(ECHO) -n "writing help indices for package:"
	@for pkg in $(R_PKGS_BASE); do \
	  $(ECHO) -n " $${pkg}" ; \
	  $(MAKE) --no-print-directory  -C ./help -s RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) DPKG=$(RLIB)/$${pkg} index-$${pkg} || exit 1; \
	done
	@$(ECHO)
endif


rlibs:
	$(MAKE) --no-print-directory -C ../extra/intl -f Makefile.win
	$(MAKE) --no-print-directory -C ../appl  OPTFLAGS='$(OPTFLAGS)' FOPTFLAGS='$(FOPTFLAGS)' -f Makefile.win
	$(MAKE) --no-print-directory -C ../nmath OPTFLAGS='$(OPTFLAGS)' -f Makefile.win
	$(MAKE) --no-print-directory -C ../main  OPTFLAGS='$(OPTFLAGS_M)' -f Makefile.win
	$(MAKE) --no-print-directory -C ./graphapp
	$(MAKE) --no-print-directory -C ./getline
	@for ex in pcre bzip2 zlib xdr; do \
	  $(MAKE) --no-print-directory -C ../extra/$${ex} -f Makefile.win || exit 1; \
	done

rmodules:
	$(MAKE) --no-print-directory -C ../modules \
	  OPTFLAGS='$(OPTFLAGS)' FOPTFLAGS='$(FOPTFLAGS)' -f Makefile.win
	$(CP) unicode/iconv.dll ../../modules/

COPYRIGHTS: $(RHOME)/COPYRIGHTS COPYRIGHTS.win
	cat $^ > $@


fixfiles: docfiles 
	@$(MKDIR) -p ../../bin
	@$(MAKE) --no-print-directory -C ./fixed BUILD=$(BUILD)

fixed/fixdesc:
	@sh fixed/GETDESC $(R_PKGS_BASE)

dllversion.o: ../include/Rversion.h

R.dll: R.a $(OBJS-EXTRA) $(MAINLIBS) $(EXTRALIBS) dllversion.o

Rdll: makeMakedeps libRblas.a ../../bin/R.dll

../../bin/R.dll: FORCE
	@$(MAKE) --no-print-directory fixfiles
	@$(MAKE) --no-print-directory -C ../include -f Makefile.win
	@$(MAKE) --no-print-directory rlibs
	@$(MAKE) --no-print-directory makeMakedeps
	@$(MAKE) --no-print-directory R.dll
	@$(MKDIR) -p ../../bin
	$(CP) R.dll ../../bin/
	@$(MKDIR) -p ./../modules/
ifdef USE_UNICOWS
	$(CP) unicode/opencow.dll ../../bin/
endif

FORCE:

front-ends: libR.a
	$(MAKE) --no-print-directory -C front-ends

R.a: $(OBJS)
	@$(ECHO) -------- Building $@ --------
	@$(RM) R.a
	$(AR) cr R.a $^
	$(RANLIB) R.a


# R.dll exports all global symbols BUT import library (libR.a)
# only (all functions) + (global vars listed in exported-vars))

libR.a:  R.exp
	$(DLLTOOL) $(DLLTOOLFLAGS) $(R-DLLTOOLFLAGS) --dllname R.dll --def R.exp --output-lib libR.a

R.exp: R.exp1
	@(diff R.exp R.exp1 > /dev/null 2>&1) || $(CP) R.exp1 R.exp

R.exp1: R.a $(MAINLIBS) $(EXTRALIBS) exported-vars
	@$(ECHO) EXPORTS > $@
	@$(NM) R.a $(MAINLIBS) $(EXTRALIBS) > RDefs
	@$(SED) -n '/^........ [T] _/s/^........ [T] _/ /p' RDefs >> $@
	@$(CAT) exported-vars >> $@
	@$(RM) RDefs

../../library/R.css: ../../doc/html/R.css
	$(CP) $< $@

../extra/zlib/libz.a:
	$(MAKE) --no-print-directory -C ../extra/zlib -f Makefile.win

../extra/pcre/libpcre.a:
	$(MAKE) --no-print-directory -C ../extra/pcre -f Makefile.win

../extra/bzip2/libbz2.a:
	$(MAKE) --no-print-directory -C ../extra/bzip2  -f Makefile.win libbz2.a

htmldocs:
	@$(ECHO)
	@$(ECHO) "------ Making HTML documentation ------"
	@$(MAKE) --no-print-directory -C ../../doc/manual -f Makefile.win html BUILD=$(BUILD)


#                          ===== BLAS ======

Rblas: ../../bin/Rblas.dll

Rblas-clean:
	$(RM) ../../bin/Rblas.dll blas00.o blas00.d blas.o cmplxblas.o \
	  zgemm.o blasGoto.o libgoto.a

Rblas-DLLLIBS=-L. $(LIBUNICOWS) -lR

libRblas.a:  Rblas.def
	$(DLLTOOL) $(DLLTOOLFLAGS) $(R-DLLTOOLFLAGS) --dllname Rblas.dll \
	  --def Rblas.def --output-lib libRblas.a

## suppress lots of warnings: this is a dummy
blas00.o: blas00.c
	$(CC) -O2  -c $< -o $@
blas.o: ../appl/blas.f
	$(F77) $(FFLAGS) -c $< -o $@
zgemm.o: ../appl/zgemm.f
	$(F77) $(FOPTFLAGS) -c $< -o $@
cmplxblas.o: ../modules/lapack/cmplxblas.f
	$(F77) $(FOPTFLAGS) -c $< -o $@

ifeq ($(strip $(USE_GOTO)),YES)
libgoto.a:  libgoto.def
	$(DLLTOOL) $(DLLTOOLFLAGS) --dllname $(GOTO_DLL) --def $< --output-lib $@
../../bin/Rblas.dll: blas00.o blasGoto.o cmplxblas.o zgemm.o dllversion.o libgoto.a
	@$(ECHO) -------- Building $@ --------
	$(DLL) -shared $(DLLFLAGS) -o $@ $^ Rblas.def -L. $(LIBUNICOWS) -lR -lg2c -lgoto
else
ifeq ($(strip $(USE_ATLAS)),YES)
../../bin/Rblas.dll: blas00.o dllversion.o
	@$(ECHO) -------- Building $@ --------
	$(DLL) -shared $(DLLFLAGS) -o $@ $^ Rblas.def -L $(ATLAS_PATH) \
	   -L. $(LIBUNICOWS) -lR -lg2c -lf77blas -latlas
else
../../bin/Rblas.dll: blas.o cmplxblas.o zgemm.o dllversion.o
	@$(ECHO) -------- Building $@ --------
	$(DLL) -shared $(DLLFLAGS) -o $@ Rblas.def $^ -L. $(LIBUNICOWS) -lR
endif
endif


#                          ===== Packages ======

CHTML:
ifeq ($(strip $(HELP)),YES)
ifeq ($(strip $(WINHELP)),CHM)
	$(MAKE) --no-print-directory -C ./help -f MkChmDll
endif
endif

base-HELP = ziponly
datasets-HELP = ziponly
graphics-HELP = ziponly
grDevices-HELP = ziponly
grid-HELP = ziponly
methods-HELP = ziponly
stats-HELP = ziponly
stats4-HELP = ziponly
utils-HELP = ziponly
stats4-SAVE = true

MASS-HELP = ziponly
boot-HELP = ziponly
cluster-HELP = ziponly
lattice-HELP = ziponly
mgcv-HELP = ziponly
nlme-HELP = ziponly
rpart-HELP = ziponly
survival-HELP = ziponly

pkg-%:
	@$(MAKE) --no-print-directory \
	  SAVE=$($*-SAVE) LAZY=$($*-LAZY) LAZYDATA=$($*-LAZYDATA) \
          DLLNM=$($*-DLLNM) FLIBS="$(FLIBS)" BUILD=$(BUILD) \
	  PKG=$* RHOME=$(RHOME) RLIB=$(RLIB) RVER=$(RVER) \
	  -C $(PKGD)/$* -f $(RHOME)/src/gnuwin32/MakePkg
	@if test -d $(RLIB)/$*/data ; then \
	  $(MAKE) --no-print-directory $($*-ZIPDATA)data-$* ; \
	fi
ifeq ($(strip $(HELP)),YES)
	@$(ECHO) "  installing help"
  ifeq ($(strip $(DPKG)),)
    ifeq ($(strip $(WINHELP)),CHM)
	@$(MAKE) --no-print-directory -C ./help -s RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) DPKG=$(RLIB)/$* CHM=-chm $($*-HELP)help-$*
	@$(MAKE) --no-print-directory -C ./help -s RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) DPKG=$(RLIB)/$* CHM=-chm chm-$*
    else
	@$(MAKE) --no-print-directory -C ./help -s RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) DPKG=$(RLIB)/$* $($*-HELP)help-$*
    endif
  else
    ifeq ($(strip $(WINHELP)),CHM)
	@$(MAKE) --no-print-directory -C ./help -s RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) DPKG=$(DPKG) CHM=-chm $($*-HELP)help-$*
	@$(MAKE) --no-print-directory -C ./help -s RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) DPKG=$(DPKG) CHM=-chm  chm-$*
    else
	@$(MAKE) --no-print-directory -C ./help -s RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) DPKG=$(DPKG) $($*-HELP)help-$*
    endif
  endif
endif

pkgfake-%:
	@$(MAKE) --no-print-directory DLLNM=$($*-DLLNM)  FLIBS="$(FLIBS)" BUILD=$(BUILD) \
	  -C $(PKGD)/$* PKG=$* RHOME=$(RHOME) RLIB=$(RLIB) RVER=$(RVER) \
	  -f $(RHOME)/src/gnuwin32/MakePkg fake
	@$(ECHO) "  installing help"
	@$(MAKE) --no-print-directory -C ./help RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) DPKG=$(DPKG) pkgfake-$*


pkgclean-%:
	$(MAKE) --no-print-directory DLLNM=$($*-DLLNM) RHOME=$(RHOME) BUILD=$(BUILD) \
	  -C $(PKGD)/$* PKG=$* -f $(RHOME)/src/gnuwin32/MakePkg clean

pkgcheck-%:
	@$(ECHO) -------- Testing package $* --------
	-@$(MKDIR) -p $(PKGD)/$*/check
	@$(MAKE) --no-print-directory -C $(PKGD)/$*/check PKG=$* RHOME=$(RHOME) RLIB=$(RLIB) \
	  -f $(RHOME)/src/gnuwin32/check/PkgCheck
	@if test -d $(PKGD)/$*/tests ; then \
	  $(MAKE) --no-print-directory -C $(PKGD)/$*/tests PKG=$* RHOME=$(RHOME) RLIB=$(RLIB) \
	    -f $(RHOME)/src/gnuwin32/check/PkgCheck tests;\
	fi

lazyload-%:
	@$(MAKE) --no-print-directory PKG=$* RHOME=$(RHOME) RLIB=$(RLIB) BUILD=$(BUILD) -C $(PKGD)/$* SPKG=$(PKGD)/$* -f $(RHOME)/src/gnuwin32/MakePkg lazyload

lazydata-%:
	@$(MAKE) --no-print-directory PKG=$* RHOME=$(RHOME) RLIB=$(RLIB) BUILD=$(BUILD) -f $(RHOME)/src/gnuwin32/MakePkg lazydata

ziphelp-%:
	$(MAKE) --no-print-directory -C ./help RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) DPKG=$(DPKG) ziphelp-$*

ziponly-%:
	$(MAKE) --no-print-directory -C ./help RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) DPKG=$(DPKG) ziponlyhelp-$*

## must be careful not to zip up a dir setup for lazy-data
zipdata-%:
	@if ! test -f $(RLIB)/$*/data/Rdata.rdb ; then \
	  $(ECHO) "  zipping data" ; \
	  (cd $(RLIB)/$*/data; \
	    if test -f Rdata.zip ; then \
	      unzip -qou Rdata; \
	    fi; \
	    $(RM) Rdata.zip filelist ; \
	    ls -1 > filelist; \
	    zip -rmq9X Rdata * -x filelist 00Index) ;\
	fi

data-%:
	@if ! test -f $(RLIB)/$*/data/Rdata.rdb ; then \
	  $(ECHO) "  not zipping data" ; \
	fi


#                          ===== cleaning ======

clean: cleanwin $(PKGCLEAN)
	$(RM) -f ../*/*.o ../*/*.a *.a ../*/*.d ../*/Makedeps fixed/fixdesc
	-$(MAKE) --no-print-directory -C ../../doc/manual -f Makefile.win distclean
	-$(MAKE) --no-print-directory -C ../nmath/standalone -f Makefile.win distclean
	-$(MAKE) --no-print-directory -C ../modules -f Makefile.win clean
	-$(MAKE) --no-print-directory -C ../../tests -f Makefile.win clean
	$(RM) -f ../library/methods/all.R
	for p in $(R_PKGS_RECOMMENDED); do \
	  $(RM) -rf ../library/$$p; \
	done

distclean veryclean: clean clean-recommended
	$(RM) -r ../../bin ../../include ../../lib ../../library ../../modules
	$(RM) ../include/config.h ../include/iconv.h ../include/psignal.h \
	../include/Rconfig.h ../include/Rversion.h ../include/Rmath.h \
	../include/libintl.h
	$(RM) ../library/*/src/*.o ../library/*/src/*.a
	$(RM) ../library/*/src/*.d ../library/*/src/Makedeps
	$(RM) ../library/*/src/*.dll
	$(RM) ../library/grid/tests/Rplots.ps
	$(RM) ../library/grid/tests/*.Rout
	$(RM) -r ../library/*/check
	$(RM) ../library/tcltk/src/tcl$(TCL_VERSION).def \
	../library/tcltk//src/tk$(TCL_VERSION).def
	$(RM) R.exp Rm.exp COPYRIGHTS
	$(RM) ../../doc/html/search/index.txt ../../doc/html/index.html \
	../../doc/html/packages.html ../../doc/html/rwin.html \
	../../doc/html/rw-FAQ.html ../../doc/html/search/SearchEngine.html
	$(RM) ../../etc/Rconsole ../../etc/Rdevga ../../etc/Rprofile.site \
	../../etc/rgb.txt ../../share/make/wintests.mk \
	../include/Platform.h ../../tests/*.Rout ../../tests/*.Rout.fail
	$(MAKE) -C fixed distclean
	$(MAKE) -C ../include -f Makefile.win distclean
	-$(MAKE) -C installer distclean
	@for pkg in $(R_PKGS_BASE); do \
	  $(RM) ../library/$${pkg}/DESCRIPTION; \
	done
	$(RM) rw-FAQ fixed/html/rw-FAQ.html
	$(RM) -r cran/*.html cran/*.htm cran/ReadMe cran/README* cran/*.exe \
		cran/md5sum.txt cran/*.$(RPREFIX) 
	$(RM) -r .vignettes # it gets left behind sometimes
	$(RM) ../../FAQ ../../RESOURCES ../../doc/html/resources.html \
	  ../../SVN-REVISION # not in SVN sources
	$(RM) -r ../../share/locale
	$(RM) XINSTALL # from cross-building
	$(RM) Rpwd.exe # do this last of all

cleanwin:
	$(RM) *.o *.dll *.a *~ \#*\# rw*.zip R.def R.exp1 Rm.exp1\
	   .RData .Rhistory Makedeps *.d
	$(MAKE) -C graphapp clean
	$(MAKE) -C fixed clean
	$(MAKE) -C getline clean
	$(MAKE) -C help clean
	$(MAKE) -C front-ends clean
	$(MAKE) -C ../extra/xdr -f Makefile.win distclean
	$(MAKE) -C ../extra/pcre -f Makefile.win distclean
	$(MAKE) -C ../extra/bzip2 -f Makefile.win clean
	$(MAKE) -C ../extra/zlib -f Makefile.win distclean
	$(MAKE) -C ../extra/intl -f Makefile.win clean
	$(MAKE) -C bitmap clean
	-$(MAKE) -C installer clean


#                          ===== testing ======

# tests
check test: pkgtests test-Basic

test-Basic:
	-@$(MAKE) --no-print-directory -C $(RHOME)/tests -f Makefile.win clean
	-@$(MAKE) --no-print-directory -C $(RHOME)/tests -f Makefile.win


PKGS-CHK = $(foreach i, $(R_PKGS_BASE), pkgcheck-$i)
pkgtests: $(PKGS-CHK)

check-devel: check
	-@$(MAKE) --no-print-directory -C $(RHOME)/tests -f Makefile.win $@

check-all fullcheck: check-devel check-recommended

check-recommended: $(foreach i, $(R_PKGS_RECOMMENDED), pkgcheck-$i)


#                          ===== documentation ======

pdfdocs manuals:
	@$(MAKE) --no-print-directory -C $(RHOME)/doc/manual -f Makefile.win clean BUILD=$(BUILD)
	@$(MAKE) --no-print-directory -C $(RHOME)/doc/manual -f Makefile.win BUILD=$(BUILD)
#	@$(MAKE) --no-print-directory ziponly-base

vignettes:
	@for pkg in $(R_PKGS_BASE); do \
	  if test -d "../library/$${pkg}/inst/doc"; then \
	    echo "building/updating vignettes for package '$${pkg}' ..."; \
	    (echo "tools:::.install_package_vignettes(\"../library/$${pkg}\", \"../../library/$${pkg}\")") | R_DEFAULT_PACKAGES="utils,tools" $(REXE) > /dev/null; \
	  fi; \
	done
	@rm -rf .vignettes # force cleanup

## ===================== Maintainer targets ========================

RVER = $(shell cut -d' ' -f1 ../../VERSION | sed -n 1p)
RVER-PAT = $(shell version=`cut -d' ' -f1 ../../VERSION | sed -n 1p`; if test "`cut -f2 -d' ' ../../VERSION`" = "Patched"; then version=`echo $${version} | sed 's/\.[0-9]*$$//'`; echo "$${version}-patched"; else echo "$${version}"; fi)

docfiles:
	$(MAKE) -f Makefile.docfiles

bitmapdll:
	$(MAKE) -C bitmap

CRANREC = cran.r-project.org::CRAN/src/contrib/$(RVER-PAT)/Recommended

## Use -c here to avoid re-downloading the same versions of files
rsync-recommended:
	@(cd ../library; \
	rsync -rcvC --delete --exclude=Makefile.in --exclude=.cvsignore \
	  --include=*.tar.gz --exclude=*.tgz --exclude=".svn" $(CRANREC) . ); \
	$(RM) ../library/Recommended/*.tgz; \
	(cd ../library/Recommended; for i in ${R_PKGS_RECOMMENDED_SOURCES} ; do cp -p $${i}*.tar.gz $${i}.tgz ; done)

link-recommended:
	@$(RM) ../library/Recommended/*.tgz
	@(cd ../library/Recommended; for i in ${R_PKGS_RECOMMENDED_SOURCES} ; do cp -p $${i}*.tar.gz $${i}.tgz ; done)

../library/%/DESCRIPTION: ../library/Recommended/%.tgz
	@$(ECHO) "---- $*"
	(cd ../library; rm -rf $*; tar zxf Recommended/$*.tgz; touch $@)

unpack-recommended:
	@$(ECHO) "--- Unpacking recommended packages"
	@for i in ${R_PKGS_RECOMMENDED_SOURCES} ; do $(MAKE) -s --no-print-directory ../library/$$i/DESCRIPTION || exit 1; done

clean-recommended:
	@for i in ${R_PKGS_RECOMMENDED_SOURCES} ; do $(RM) -r ../library/$${i} ; done

ifeq ($(strip $(HELP)),YES)
ifeq ($(strip $(WINHELP)),CHM)
DOCSTYPE=chm
else
DOCSTYPE=normal
endif
else
DOCSTYPE=none
endif

recommended: unpack-recommended
	@$(ECHO) "--- Making recommended packages"
ifeq ($(strip $(BUILD)),CROSS)
	@for p in $(R_PKGS_RECOMMENDED_SOURCES); do \
          $(R_EXE) CMD perl XINSTALL --unsafe --docs=normal ../library/$${p} || exit 1; \
        done
else
	@for p in $(R_PKGS_RECOMMENDED_SOURCES); do \
	  $(RHOME)/bin/rcmd.exe INSTALL --unsafe ../library/$${p} --docs=$(DOCSTYPE) || exit 1; \
	done
endif

EXTRA_PKGS=
rinstaller:
	@$(MAKE) -C installer EXTRA_PKGS='$(EXTRA_PKGS)'
	@$(MAKE) -C installer clean

crandir:
	$(CP) installer/$(RPREFIX)-win32.exe cran
	$(CP) CHANGES cran/CHANGES.$(RPREFIX)
	$(CP) ../../NEWS cran/NEWS.$(RPREFIX)
	(cd cran;\
	 $(ECHO) 'cat(md5sum("$(RPREFIX).exe"),"*$(RPREFIX)-win32.exe")' \
         | $(RHOME)/bin/rterm --vanilla --slave R_DEFAULT_PACKAGES=tools >md5sum.txt)
	$(CP) ../../doc/html/rw-FAQ.html cran
	$(SED) -e s/@RWVER@/$(RPREFIX)/g \
	  -e s/@RVER@/$(RVER)/g \
	  -e "s/@FULLVERSION@/$(FULLVERSION)/g" README > cran/README.$(RPREFIX)
	$(SED) -e s/@RWVER@/$(RPREFIX)/g \
	  -e s/@RVER@/$(RVER)/g \
	  -e "s/@FULLVERSION@/$(FULLVERSION)/g" cran/ReadMe.in > cran/ReadMe	  
	$(SED) -e s/@RWVER@/$(RPREFIX)/g \
	  -e s/@RVER@/$(RVER)/g \
	  -e "s/@DATE@/$(DATE)/g" \
	  -e "s/@FULLVERSION@/$(FULLVERSION)/g" cran/index.in > cran/index.html
	$(SED) -e s/@RWVER@/$(RPREFIX)/g \
	  -e s/@RVER@/$(RVER)/g \
	  -e "s/@DATE@/$(DATE)/g" \
	  -e "s/@FULLVERSION@/$(FULLVERSION)/g" cran/rdevel.in > cran/rdevel.html
	$(SED) -e s/@RWVER@/$(RPREFIX)/g \
	  -e s/@RVER@/$(RVER)/g \
	  -e "s/@DATE@/$(DATE)/g" \
	  -e "s/@FULLVERSION@/$(FULLVERSION)/g" cran/rpatched.in > cran/rpatched.html	  
	$(SED) -e s/@RWVER@/$(RPREFIX)/g cran/release.in > cran/release.htm

rbuild-no-mbcs:
	@$(MAKE) --no-print-directory fixfiles
	@$(MAKE) --no-print-directory -C ../include -f Makefile.win
	@$(MAKE) --no-print-directory -C ../scripts -f Makefile.win
	@$(SED) -e '/SUPPORT_MBCS/d' fixed/h/config.h > ../include/config.h
	@$(MAKE) --no-print-directory libRblas.a
	@$(MAKE) --no-print-directory ../../bin/R.dll
	@mv ../../bin/R.dll ../../bin/Rnombcs.dll
	@$(RM) ../include/config.h ../include/Rconfig.h

distribution:
	@$(MAKE) Rpwd.exe
	-mv ../../SVN-REVISION ../../SVN-REVISION.bak 2> /dev/null
	@$(MAKE) rbuild-no-mbcs
	@$(MAKE) rbuild
	@$(MAKE) rpackage
	@$(MAKE) htmldocs
	@$(MAKE) -C ../../po -f Makefile.win
	$(MAKE) bitmapdll
	$(MAKE) recommended
	$(MAKE) vignettes
	$(MAKE) manuals
	$(MAKE) rinstaller
	$(MAKE) crandir
	@$(RM) -f $(RHOME)/SVN-REVISION.bak


## ============= End of maintainer targets ========================

# Dependencies : blas00.c malloc.c don't have any
CSOURCES=$(SOURCES) blasGoto.c
DEPS=$(CSOURCES:.c=.d)

makeMakedeps: $(DEPS)
	@$(RM) Makedeps
	@cat $(DEPS) >> Makedeps

-include Makedeps
