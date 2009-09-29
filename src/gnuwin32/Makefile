include MkRules
include ../../share/make/vars.mk

MK = $(MAKE) --no-print-directory

all:
	@$(MK) Rpwd.exe
	@if test -f  ../../SVN-REVISION ; then \
	  cp ../../SVN-REVISION ../../SVN-REVISION.bak ; \
	fi
	@$(MK) rbuild
	@$(MK) -C ../library -f Makefile.win all
	@$(MK) -C ../library -f Makefile.win docs
	@$(MK) htmldocs
	@$(MK)  -C ../../po -f Makefile.win

rpackages:
	@$(MK) -C ../library -f Makefile.win all
	@$(MK) -C ../library -f Makefile.win docs

## must be absolute and space-free.  
## NB: will not be set correctly first time through.
RHOME = $(shell ./Rpwd.exe ../..)

REXE = $(RHOME)/bin/rterm.exe --vanilla --slave

RPREFIX = $(shell $(PERL) fixed/rwver.pl ../../VERSION)

FULLVERSION=`cat ../../VERSION`

DATE = $(shell date +%F)

SVNREVISION=`sed -e 1s/Revision:.// -e q <$(RHOME)/SVN-REVISION`

RLIB = $(RHOME)/library

OPTFLAGS=-O3 -Wall -pedantic
FOPTFLAGS=-O3
ifdef DEBUG
 OPTFLAGS+=$(G_FLAG)
 FOPTFLAGS+=$(G_FLAG)
 DLLFLAGS=
else
 DLLFLAGS=-s
endif

# used when compiling src/main: needed for memory.c, regex.c
OPTFLAGS_M=$(OPTFLAGS)

R-DLLFLAGS=-mwindows
ifdef USE_ICU
LIBEXTRAS+= -licuin38 -licuuc38
endif

R-DLLLIBS =-L. $(FLIBS) -lRblas -L../../$(IMPDIR) -lRzlib -lRgraphapp -lRiconv -lcomctl32 -lversion $(LIBEXTRAS)
dllversion-RESFLAGS =-I../include
SOURCES = \
  console.c dataentry.c dynload.c edit.c editor.c embeddedR.c extra.c \
  opt.c pager.c preferences.c psignal.c rhome.c rt_complete.c \
  rui.c run.c shext.c sys-win32.c system.c dos_wglob.c
OBJS = $(SOURCES:.c=.o)
ifeq ($(strip $(WIN)),32)
 OBJS += e_pow.o
endif
MAINLIBS = ../main/libmain.a ../appl/libappl.a ../nmath/libnmath.a
EXTRALIBS = getline/gl.a ../extra/xdr/libxdr.a \
   ../extra/pcre/libpcre.a ../extra/bzip2/libbz2.a \
   ../extra/intl/libintl.a ../extra/trio/libtrio.a ../extra/tzone/libtz.a \
   ../extra/tre/libtre.a ../extra/xz/liblzma.a

# flags to ensure that Doug Lea's malloc is used:
ifdef LEA_MALLOC
extra-CFLAGS+=-DLEA_MALLOC
OBJS-EXTRA=malloc.o
OPTFLAGS_M+=-DLEA_MALLOC
endif

.PHONY:  clean veryclean rlibs fixfiles front-ends rmodules


CPPFLAGS = -I../include -I. -I../extra -DHAVE_CONFIG_H -DR_DLL_BUILD
CFLAGS = $(OPTFLAGS)
FFLAGS = $(FOPTFLAGS)
extra-CPPFLAGS = -I../library/grDevices/src

Rpwd.exe: front-ends/Rpwd.exe
	$(CP) $< $@

front-ends/Rpwd.exe:
	$(MK) -C front-ends Rpwd

## watch out: R.dll and Rblas.dll depend on each other.
rbuild:
	@$(MK) fixfiles
	@$(MK) -C ../include -f Makefile.win
	@$(MK) -C ../scripts -f Makefile.win
ifdef USE_IMPLIBS
	@$(MK) libiconv.dll.a
endif
	@$(MK) libRblas.dll.a
	@$(MK) ../../bin/R.dll
ifdef USE_IMPLIBS
	@$(MK) implibs
endif
	@$(MK) Rblas
	@$(MK) front-ends COPYRIGHTS rmodules fixed/fixdesc


rlibs:
	$(MK) -C ../extra/intl OPTFLAGS='$(OPTFLAGS)' -f Makefile.win
	$(MK) -C ../appl RHOME=$(RHOME) OPTFLAGS='$(OPTFLAGS)' FOPTFLAGS='$(FOPTFLAGS)' -f Makefile.win
	$(MK) -C ../nmath OPTFLAGS='$(OPTFLAGS)' -f Makefile.win
	$(MK) -C ../main RHOME=$(RHOME) OPTFLAGS='$(OPTFLAGS_M)' FFLAGS='$(FOPTFLAGS)' -f Makefile.win
	$(MK) -C ./getline OPTFLAGS='$(OPTFLAGS)'
	@for ex in graphapp pcre bzip2 zlib xdr tre trio tzone win_iconv xz; do \
	  $(MK) -C ../extra/$${ex} OPTFLAGS='$(OPTFLAGS)' -f Makefile.win || exit 1; \
	done

rmodules:
	$(MK) -C ../modules -f Makefile.win \
	  RHOME=$(RHOME) OPTFLAGS='$(OPTFLAGS)' FOPTFLAGS='$(FOPTFLAGS)'

COPYRIGHTS: ../../doc/COPYRIGHTS COPYRIGHTS.win
	cat $^ > $@


fixfiles: docfiles 
	@$(MKDIR) -p ../../bin
	@$(MK) -C ./fixed

fixed/fixdesc:
	@sh fixed/GETDESC $(R_PKGS_BASE)

dllversion.o: ../include/Rversion.h

R.dll: $(OBJS) $(OBJS-EXTRA) $(MAINLIBS) $(EXTRALIBS) dllversion.o
	@$(ECHO) EXPORTS > R.def
	@$(NM) $^ | $(SED) -n 's/^.* [BCDRT] _/ /p' | $(SORT) | uniq > R0.def
	@comm -23 R0.def Rdll.hide >> R.def
	$(DLL) -shared $(DLLFLAGS) $($*-DLLFLAGS) -o $@ R.def $^ $($*-DLLLIBS) $(DLLLIBS)
	@$(RM) R.def R0.def

R.exp: $(OBJS) $(OBJS-EXTRA) $(MAINLIBS) $(EXTRALIBS) dllversion.o
	@$(ECHO) LIBRARY R.dll > R.exp
	@$(ECHO) EXPORTS >> R.exp
	@$(NM) $^ | $(SED) -n 's/^.* [BCDRT] _/ /p' | $(SORT) | uniq > R0.def
	@comm -23 R0.def Rdll.hide >> R.exp
	@$(RM) R0.def


Rdll: makeMakedeps libRblas.dll.a ../../bin/R.dll

../../bin/R.dll: FORCE
	@$(MK) fixfiles
	@$(MK) -C ../include -f Makefile.win
	@$(MK) rlibs
	@$(MK) makeMakedeps
	@$(MK) R.dll
	@$(MKDIR) -p ../../bin
	$(CP) R.dll ../../bin/
	@$(MKDIR) -p ./../modules/

FORCE:

implibs: libR.dll.a libRblas.dll.a
	@$(MKDIR) -p ../../lib
	@$(CP) $^ ../../lib

libR.dll.a: R.exp
	$(DLLTOOL) $(DLLTOOLFLAGS) $(R-DLLTOOLFLAGS) --dllname R.dll \
	  --input-def $< --output-lib $@

libiconv.dll.a: unicode/iconv.def
	$(DLLTOOL) $(DLLTOOLFLAGS) $(R-DLLTOOLFLAGS) --dllname iconv.dll \
	  --input-def $< --output-lib $@


front-ends:
	$(MK) -C front-ends

../extra/zlib/libz.a:
	$(MK) -C ../extra/zlib -f Makefile.win

../extra/pcre/libpcre.a:
	$(MK) -C ../extra/pcre -f Makefile.win

../extra/tre/libtre.a:
	$(MK) -C ../extra/tre -f Makefile.win

../extra/bzip2/libbz2.a:
	$(MK) -C ../extra/bzip2  -f Makefile.win libbz2.a

../extra/trio/libtrio.a:
	$(MK) -C ../extra/trio -f Makefile.win

../extra/xz/liblzma.a:
	$(MK) -C ../extra/xz -f Makefile.win


htmldocs:
	@$(ECHO)
	@$(ECHO) "------ Making HTML documentation ------"
	@$(MK) -C ../../doc/manual -f Makefile.win html


#                          ===== BLAS ======

Rblas:
	@$(MK) -C ../extra/blas -f Makefile.win \
	  RHOME=$(RHOME) FFLAGS='$(FOPTFLAGS)'

Rblas-clean:
	@$(MK) -C ../extra/blas -f Makefile.win clean

libRblas.dll.a:  ../extra/blas/Rblas.def
	@$(DLLTOOL) $(DLLTOOLFLAGS) --dllname Rblas.dll \
	  --input-def $< --output-lib $@


#                          ===== cleaning ======

## used in installer/Makefile
PKGDIR=../library
pkgclean-%:
	@(cd $(PKGDIR)/$*/src; rm -f *.d *.o *.dll *_res.rc Makedeps)
	@rm -rf $(PKGDIR)/$*/chm

EXTRA_DIRS = blas bzip2 graphapp intl pcre tre trio tzone xdr xz win_iconv zlib

clean: cleanwin
	@$(MAKE) -C ../library -f Makefile.win clean
	$(RM) -f ../*/*.o ../*/*.a *.a ../*/*.d ../*/Makedeps fixed/fixdesc
	@for d in $(EXTRA_DIRS); do \
	  $(MAKE) -C ../extra/$${d} -f Makefile.win clean; \
	done
	-$(MK) -C ../../doc/manual -f Makefile.win distclean
	-$(MK) -C ../nmath/standalone -f Makefile.win distclean
	-$(MK) -C ../modules -f Makefile.win clean
	-$(MK) -C ../../tests -f Makefile.win clean
	-$(MK) -C ../library/Recommended -f Makefile.win clean
	$(RM) -f ../library/methods/all.R
	for p in $(R_PKGS_RECOMMENDED); do \
	  $(RM) -rf ../library/$$p; \
	done

distclean: clean
	$(RM) -r ../library/*/chm
	@for d in $(EXTRA_DIRS); do \
	  $(MAKE) -C ../extra/$${d} -f Makefile.win distclean; \
	done
	@$(MAKE) -C ../../tests -f Makefile.win distclean RHOME=$(RHOME)
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
	$(RM) ../../doc/html/index.html \
	../../doc/html/packages.html ../../doc/html/rwin.html \
	../../doc/html/rw-FAQ.html
	$(RM) ../../etc/Makeconf ../../etc/Rconsole ../../etc/Rdevga \
	../../etc/Rprofile.site ../../etc/rgb.txt ../../etc/Rcmd_environ \
	../../tests/*.Rout ../../tests/*.Rout.fail
	$(RM) -r ../../share/zoneinfo
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
	$(RM) -r .vignettes # it gets left behind if there is an error
	$(RM) -r ../../share/locale
	$(RM) Rpwd.exe # do this last of all

veryclean: distclean
	$(RM) ../../doc/FAQ ../../doc/RESOURCES ../../doc/html/resources.html \
	  ../../SVN-REVISION # not in SVN sources

cleanwin:
	$(RM) *.o *.dll *.a *~ \#*\# .RData .Rhistory Makedeps *.d
	$(MAKE) -C ../extra/graphapp -f Makefile.win clean
	$(MAKE) -C fixed clean
	$(MAKE) -C getline clean
	$(MAKE) -C front-ends clean
	$(MAKE) -C bitmap clean
	-$(MAKE) -C installer clean


#                          ===== testing ======

check check-devel check-all check-recommended:
	@$(MK) -C ../../tests -f Makefile.win RHOME=$(RHOME) $@

#                          ===== documentation ======

pdfdocs manuals:
	@$(MK) -C $(RHOME)/doc/manual -f Makefile.win clean
	@$(MK) -C $(RHOME)/doc/manual -f Makefile.win

vignettes:
	@for pkg in $(R_PKGS_BASE); do \
	  if test -d "../library/$${pkg}/inst/doc"; then \
	    echo "building/updating vignettes for package '$${pkg}' ..."; \
	    (echo "tools:::.install_package_vignettes(\"../library/$${pkg}\", \"../../library/$${pkg}\")") | R_DEFAULT_PACKAGES="utils,tools" $(REXE) > /dev/null; \
	  fi; \
	done
	@rm -rf .vignettes


## ===================== Maintainer targets ========================

RVER = $(shell cut -d' ' -f1 ../../VERSION | sed -n 1p)
RVER-PAT = $(shell version=`cut -d' ' -f1 ../../VERSION | sed -n 1p`; if test "`cut -f2 -d' ' ../../VERSION`" = "Patched"; then version=`echo $${version} | sed 's/\.[0-9]*$$//'`; echo "$${version}-patched"; else echo "$${version}"; fi)
SEDVER = -e s/@RVER@/$(RVER)/g -e s/@RWVER@/$(RPREFIX)/g

docfiles: rw-FAQ ../../doc/html/rw-FAQ.html

../../doc/html/rw-FAQ.html: rw-FAQ.texi ../../VERSION ../../doc/manual/Rman.css
	@echo "making rw-FAQ.html"
	@$(SED) $(SEDVER) $< | \
	  makeinfo --no-split --html --no-headers --number-sections --css-include=../../doc/manual/Rman.css -o $@ 

rw-FAQ: rw-FAQ.texi ../../VERSION
	@echo "making rw-FAQ"
	@$(SED) $(SEDVER) $< | \
	  makeinfo --no-headers --number-sections -o $@


bitmapdll:
	$(MAKE) -C bitmap

CRANREC = cran.r-project.org::CRAN/src/contrib/$(RVER-PAT)/Recommended

## Use -c here to avoid re-downloading the same versions of files
## A recent rsync seems to need -p
rsync-recommended:
	@(cd ../library; \
	  rsync --timeout=60 -rcvpC --delete \
	    --exclude=Makefile.in --exclude=Makefile.win --exclude=.cvsignore \
	    --include=*.tar.gz --exclude=*.tgz --exclude=*.ts \
	    --exclude=".svn" $(CRANREC) . )
	@$(RM) ../library/Recommended/*.tgz
	@(cd ../library/Recommended; for i in ${R_PKGS_RECOMMENDED}; do cp -p $${i}*.tar.gz $${i}.tgz ; done)

link-recommended:
	@$(RM) ../library/Recommended/*.tgz
	@(cd ../library/Recommended; for i in ${R_PKGS_RECOMMENDED} ; do cp -p $${i}*.tar.gz $${i}.tgz ; done)


recommended:
	@$(ECHO) "--- Making recommended packages"
	@$(MK) -C ../library/Recommended -f Makefile.win RHOME=$(RHOME)

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
         | $(REXE) R_DEFAULT_PACKAGES=tools >md5sum.txt)
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
	  -e "s/@SVNREVISION@/$(SVNREVISION)/g" \
	  -e "s/@FULLVERSION@/$(FULLVERSION)/g" cran/index.in > cran/index.html
	$(SED) -e s/@RWVER@/$(RPREFIX)/g \
	  -e s/@RVER@/$(RVER)/g \
	  -e "s/@DATE@/$(DATE)/g" \
	  -e "s/@SVNREVISION@/$(SVNREVISION)/g" \
	  -e "s/@FULLVERSION@/$(FULLVERSION)/g" cran/rdevel.in > cran/rdevel.html
	$(SED) -e s/@RWVER@/$(RPREFIX)/g \
	  -e s/@RVER@/$(RVER)/g \
	  -e "s/@DATE@/$(DATE)/g" \
	  -e "s/@SVNREVISION@/$(SVNREVISION)/g" \
	  -e "s/@FULLVERSION@/$(FULLVERSION)/g" cran/rpatched.in > cran/rpatched.html	  
	$(SED) -e s/@RWVER@/$(RPREFIX)/g \
	  -e s/@RVER@/$(RVER)/g \
	  -e "s/@DATE@/$(DATE)/g" \
	  -e "s/@SVNREVISION@/$(SVNREVISION)/g" \
	  -e "s/@FULLVERSION@/$(FULLVERSION)/g" cran/rtest.in > cran/rtest.html	  	  
	$(SED) -e s/@RWVER@/$(RPREFIX)/g cran/release.in > cran/release.htm
	(cd cran;\
	 zip dosfiles.zip CHANGES.$(RPREFIX) NEWS.$(RPREFIX) README.$(RPREFIX) ReadMe;\
	 unzip -ao dosfiles.zip;\
	 $(RM) dosfiles.zip)

distribution:
	@$(MK) all
	@$(MK) bitmapdll
	@$(MK) recommended
	@$(MK) vignettes
	@$(MK) manuals
	@$(MK) rinstaller
	@$(MK) crandir
	@$(RM) -f $(RHOME)/SVN-REVISION.bak


## ============= End of maintainer targets ========================

# Dependencies : blas00.c malloc.c don't have any
CSOURCES=$(SOURCES)
DEPS=$(CSOURCES:.c=.d)

makeMakedeps: $(DEPS)
	@$(RM) Makedeps
	@cat $(DEPS) >> Makedeps

-include Makedeps
