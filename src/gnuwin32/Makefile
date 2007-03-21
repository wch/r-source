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

OPTFLAGS=-O3 -Wall -pedantic $(C99FLAG)
FOPTFLAGS=-O3 -Wall
ifdef DEBUG
 OPTFLAGS+=$(G_FLAG)
 FOPTFLAGS+=$(G_FLAG)
 DLLFLAGS=
else
 DLLFLAGS=-s
endif
ifdef R_MEMORY_PROFILING
 OPTFLAGS+=-DR_MEMORY_PROFILING
endif

# used when compiling src/main: needed for memory.c
OPTFLAGS_M=$(OPTFLAGS)

ifneq ($(strip $(BUILD)),CROSS)
R-DLLFLAGS = -mwindows
else
R-DLLFLAGS = -mwindows
endif

R-DLLLIBS =-L. $(FLIBS) -lRblas -lcomctl32 -lversion $(LIBEXTRAS)
dllversion-RESFLAGS =-I../include
SOURCES = \
  console.c dataentry.c dynload.c edit.c editor.c embeddedR.c extra.c \
  opt.c pager.c preferences.c psignal.c rhome.c rui.c run.c shext.c \
  sys-win32.c system.c dos_glob.c
OBJS = $(SOURCES:.c=.o) e_pow.o
MAINLIBS = ../main/libmain.a ../appl/libappl.a ../nmath/libnmath.a
EXTRALIBS = graphapp/ga.a getline/gl.a ../extra/xdr/libxdr.a \
   ../extra/zlib/libz.a ../extra/pcre/libpcre.a ../extra/bzip2/libbz2.a \
   ../extra/intl/libintl.a ../extra/trio/libtrio.a

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
## so build import library R.dll then Rblas.dll
rbuild:
	@$(MAKE) --no-print-directory fixfiles
	@$(MAKE) --no-print-directory -C ../include -f Makefile.win
	@$(MAKE) --no-print-directory -C ../scripts -f Makefile.win
	@$(MAKE) --no-print-directory libRblas.dll.a
	@$(MAKE) --no-print-directory ../../bin/R.dll
	@$(MAKE) --no-print-directory Rblas CHTML
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
	    R_DEFAULT_PACKAGES=NULL LC_ALL=C $(R_EXE) --vanilla --slave > /dev/null ; \
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
	$(MAKE) --no-print-directory -C ../extra/intl OPTFLAGS='$(OPTFLAGS)' -f Makefile.win
	$(MAKE) --no-print-directory -C ../appl OPTFLAGS='$(OPTFLAGS)' FOPTFLAGS='$(FOPTFLAGS)' -f Makefile.win
	$(MAKE) --no-print-directory -C ../nmath OPTFLAGS='$(OPTFLAGS)' -f Makefile.win
	$(MAKE) --no-print-directory -C ../main OPTFLAGS='$(OPTFLAGS_M)' FFLAGS='$(FOPTFLAGS)' -f Makefile.win
	$(MAKE) --no-print-directory -C ./graphapp OPTFLAGS='$(OPTFLAGS)'
	$(MAKE) --no-print-directory -C ./getline OPTFLAGS='$(OPTFLAGS)'
	@for ex in pcre bzip2 zlib xdr trio; do \
	  $(MAKE) --no-print-directory -C ../extra/$${ex} OPTFLAGS='$(OPTFLAGS)' -f Makefile.win || exit 1; \
	done

rmodules:
	$(MAKE) --no-print-directory -C ../modules \
	  OPTFLAGS='$(OPTFLAGS)' FOPTFLAGS='$(FOPTFLAGS)' -f Makefile.win
	$(CP) unicode/iconv.dll ../../modules/

COPYRIGHTS: $(RHOME)/doc/COPYRIGHTS COPYRIGHTS.win
	cat $^ > $@


fixfiles: docfiles 
	@$(MKDIR) -p ../../bin
	@$(MAKE) --no-print-directory -C ./fixed BUILD=$(BUILD)

fixed/fixdesc:
	@sh fixed/GETDESC $(R_PKGS_BASE)

dllversion.o: ../include/Rversion.h

R.dll: $(OBJS) $(OBJS-EXTRA) $(MAINLIBS) $(EXTRALIBS) dllversion.o
	@$(ECHO) EXPORTS > R.def
	@$(NM) $^ | $(SED) -n 's/^.* [BCDRT] _/ /p' | sort | uniq > R0.def
	@comm -23 R0.def Rdll.hide >> R.def
	$(DLL) -shared $(DLLFLAGS) $($*-DLLFLAGS) -o $@ R.def $^ $($*-DLLLIBS) $(DLLLIBS)
	@$(RM) R.def R0.def

R.exp: $(OBJS) $(OBJS-EXTRA) $(MAINLIBS) $(EXTRALIBS) dllversion.o
	@$(ECHO) LIBRARY R.dll > R.exp
	@$(ECHO) EXPORTS >> R.exp
	@$(NM) $^ | $(SED) -n 's/^.* [BCDRT] _/ /p' | sort | uniq > R0.def
	@comm -23 R0.def Rdll.hide >> R.exp
	@$(RM) R0.def


Rdll: makeMakedeps libRblas.dll.a ../../bin/R.dll

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

front-ends:
	$(MAKE) --no-print-directory -C front-ends

../../library/R.css: ../../doc/html/R.css
	$(CP) $< $@

../extra/zlib/libz.a:
	$(MAKE) --no-print-directory -C ../extra/zlib -f Makefile.win

../extra/pcre/libpcre.a:
	$(MAKE) --no-print-directory -C ../extra/pcre -f Makefile.win

../extra/bzip2/libbz2.a:
	$(MAKE) --no-print-directory -C ../extra/bzip2  -f Makefile.win libbz2.a
../extra/trio/libtrio.a:
	$(MAKE) --no-print-directory -C ../extra/trio -f Makefile.win


htmldocs:
	@$(ECHO)
	@$(ECHO) "------ Making HTML documentation ------"
	@$(MAKE) --no-print-directory -C ../../doc/manual -f Makefile.win html BUILD=$(BUILD)


#                          ===== BLAS ======

Rblas:
	@$(MAKE) --no-print-directory -C ../extra/blas -f Makefile.win \
	  FFLAGS='$(FOPTFLAGS)'

Rblas-clean:
	@$(MAKE) --no-print-directory -C ../extra/blas -f Makefile.win clean

libRblas.dll.a:  ../extra/blas/Rblas.def
	$(DLLTOOL) $(DLLTOOLFLAGS) $(R-DLLTOOLFLAGS) --dllname Rblas.dll \
	  --def $< --output-lib $@


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
splines-HELP = ziponly
stats-HELP = ziponly
stats4-HELP = ziponly
tcltk-HELP = ziponly
tools-HELP = ziponly
utils-HELP = ziponly
stats4-SAVE = true

KernSmooth-HELP = ziponly
MASS-HELP = ziponly
class-HELP = ziponly
nnet-HELP = ziponly
spatial-HELP = ziponly
boot-HELP = ziponly
cluster-HELP = ziponly
foreign-HELP = ziponly
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
	@$(MAKE) --no-print-directory -C ./help -s RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) DPKG=$(RLIB)/$* HELPTYPES='$(HELPTYPES) -chm' $($*-HELP)help-$*
	@$(MAKE) --no-print-directory -C ./help -s RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) DPKG=$(RLIB)/$* chm-$*
    else
	@$(MAKE) --no-print-directory -C ./help -s RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) DPKG=$(RLIB)/$* HELPTYPES='$(HELPTYPES)' $($*-HELP)help-$*
    endif
  else
    ifeq ($(strip $(WINHELP)),CHM)
	@$(MAKE) --no-print-directory -C ./help -s RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) DPKG=$(DPKG) HELPTYPES='$(HELPTYPES) -chm' $($*-HELP)help-$*
	@$(MAKE) --no-print-directory -C ./help -s RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) DPKG=$(DPKG) chm-$*
    else
	@$(MAKE) --no-print-directory -C ./help -s RHOME=$(RHOME) PKGDIR=$(PKGD) RLIB=$(RLIB) DPKG=$(DPKG) HELPTYPES='$(HELPTYPES)' $($*-HELP)help-$*
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

## used for standard packages whose tests are run in make check-devel, 
## not make check
pkgcheck2-%:
	@$(ECHO) -------- Testing package $* --------
	-@$(MKDIR) -p $(PKGD)/$*/check
	@$(MAKE) --no-print-directory -C $(PKGD)/$*/check PKG=$* RHOME=$(RHOME) RLIB=$(RLIB) \
	  -f $(RHOME)/src/gnuwin32/check/PkgCheck

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

EXTRA_DIRS = blas bzip2 intl pcre trio xdr zlib

clean: cleanwin $(PKGCLEAN)
	$(RM) -f ../*/*.o ../*/*.a *.a ../*/*.d ../*/Makedeps fixed/fixdesc
	@for d in $(EXTRA_DIRS); do \
	  $(MAKE) -C ../extra/$${d} -f Makefile.win clean; \
	done
	-$(MAKE) --no-print-directory -C ../../doc/manual -f Makefile.win distclean
	-$(MAKE) --no-print-directory -C ../nmath/standalone -f Makefile.win distclean
	-$(MAKE) --no-print-directory -C ../modules -f Makefile.win clean
	-$(MAKE) --no-print-directory -C ../../tests -f Makefile.win clean
	$(RM) -f ../library/methods/all.R
	for p in $(R_PKGS_RECOMMENDED); do \
	  $(RM) -rf ../library/$$p; \
	done

distclean: clean clean-recommended
	@for d in $(EXTRA_DIRS); do \
	  $(MAKE) -C ../extra/$${d} -f Makefile.win distclean; \
	done
	$(RM) -r ../../bin ../../include ../../lib ../../library ../../modules
	$(RM) ../include/config.h ../include/iconv.h ../include/psignal.h \
	../include/Rconfig.h ../include/Rversion.h ../include/Rmath.h \
	../include/libintl.h
	$(RM) ../library/*/src/*.o ../library/*/src/*.a
	$(RM) ../library/*/src/*.d ../library/*/src/Makedeps
	$(RM) ../library/*/src/*.dll
	$(RM) -r ../library/*/check
	$(RM) ../library/*/tests/*.ps
	$(RM) ../library/tcltk/src/tcl$(TCL_VERSION).def \
	../library/tcltk//src/tk$(TCL_VERSION).def
	$(RM) R.exp COPYRIGHTS
	$(RM) ../../doc/html/search/index.txt ../../doc/html/index.html \
	../../doc/html/packages.html ../../doc/html/rwin.html \
	../../doc/html/rw-FAQ.html ../../doc/html/search/SearchEngine.html
	$(RM) ../../etc/Makeconf ../../etc/Rconsole ../../etc/Rdevga \
	../../etc/Rprofile.site ../../etc/rgb.txt \
	../../share/make/wintests.mk \
	../../tests/*.Rout ../../tests/*.Rout.fail
	$(MAKE) -C fixed distclean
	$(MAKE) -C ../include -f Makefile.win distclean
	-$(MAKE) -C installer distclean
	@for pkg in $(R_PKGS_BASE); do \
	  $(RM) ../library/$${pkg}/DESCRIPTION; \
	done
	$(RM) ../../SVN-REVISION.bak
	$(RM) rw-FAQ fixed/html/rw-FAQ.html
	$(RM) -r cran/*.html cran/*.htm cran/ReadMe cran/README* cran/*.exe \
		cran/md5sum.txt cran/*.$(RPREFIX) 
	$(RM) -r .vignettes # it gets left behind sometimes
	$(RM) -r ../../share/locale
	$(RM) XINSTALL # from cross-building
	$(RM) Rpwd.exe # do this last of all

veryclean: distclean
	$(RM) ../../doc/FAQ ../../doc/RESOURCES ../../doc/html/resources.html \
	  ../../SVN-REVISION # not in SVN sources

cleanwin:
	$(RM) *.o *.dll *.a *~ \#*\# .RData .Rhistory Makedeps *.d
	$(MAKE) -C graphapp clean
	$(MAKE) -C fixed clean
	$(MAKE) -C getline clean
	$(MAKE) -C help clean
	$(MAKE) -C front-ends clean
	$(MAKE) -C bitmap clean
	-$(MAKE) -C installer clean


#                          ===== testing ======

# tests
check test: pkgtests test-Basic

test-Basic:
	-@$(MAKE) --no-print-directory -C $(RHOME)/tests -f Makefile.win clean
	-@$(MAKE) --no-print-directory -C $(RHOME)/tests -f Makefile.win


PKGS-CHK = $(foreach i, $(R_PKGS_BASE), pkgcheck2-$i)
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
SEDVER = -e s/@RVER@/$(RVER)/g -e s/@RWVER@/$(RPREFIX)/g

docfiles: rw-FAQ ../../doc/html/rw-FAQ.html

../../doc/html/rw-FAQ.html: rw-FAQ.texi ../../VERSION
	@echo "making rw-FAQ.html"
	@$(SED) $(SEDVER) $< | \
	  makeinfo --no-split --html --no-headers --number-sections -o $@ 

rw-FAQ: rw-FAQ.texi ../../VERSION
	@echo "making rw-FAQ"
	@$(SED) $(SEDVER) $< | \
	  makeinfo --no-headers --number-sections -o $@


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
          $(R_EXE) CMD perl XINSTALL --unsafe -l $(RHOME)/library --docs=normal ../library/$${p} || exit 1; \
        done
else
	@for p in $(R_PKGS_RECOMMENDED_SOURCES); do \
	  $(RHOME)/bin/rcmd.exe INSTALL --unsafe -l $(RHOME)/library ../library/$${p} --docs=$(DOCSTYPE) || exit 1; \
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
	 $(ECHO) 'cat(md5sum("$(RPREFIX)-win32.exe"),"*$(RPREFIX)-win32.exe")' \
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
	$(SED) -e s/@RWVER@/$(RPREFIX)/g \
	  -e s/@RVER@/$(RVER)/g \
	  -e "s/@DATE@/$(DATE)/g" \
	  -e "s/@FULLVERSION@/$(FULLVERSION)/g" cran/rtest.in > cran/rtest.html	  	  
	$(SED) -e s/@RWVER@/$(RPREFIX)/g cran/release.in > cran/release.htm
	(cd cran;\
	 zip dosfiles.zip CHANGES.$(RPREFIX) NEWS.$(RPREFIX) README.$(RPREFIX) ReadMe;\
	 unzip -ao dosfiles.zip;\
	 $(RM) dosfiles.zip)

distribution:
	@$(MAKE) Rpwd.exe
	-mv ../../SVN-REVISION ../../SVN-REVISION.bak 2> /dev/null
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
CSOURCES=$(SOURCES)
DEPS=$(CSOURCES:.c=.d)

makeMakedeps: $(DEPS)
	@$(RM) Makedeps
	@cat $(DEPS) >> Makedeps

-include Makedeps
