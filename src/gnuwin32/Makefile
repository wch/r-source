include MkRules

HELP=YES

PACKAGES=base eda lqs modreg mva stepfun
RPREFIX=rw0642

ifdef DEBUG
 OPTFLAGS=-g -O2
 DLLFLAGS=
else
 OPTFLAGS=-O2 -Wall
 DLLFLAGS=-s 
endif

DIRS=  . ../nmath ../appl ../main
R-DLLFLAGS= $(STRIPFLAG) -mwindows
R-DLLLIBS=-L. $(FLIBS) -lwsock32 -lcomctl32
SOURCES=$(foreach i, $(DIRS), $(wildcard $i/*.c $i/*.f))
UFILES= ../unix/devPS.c ../unix/devPicTeX.c ../unix/sock.c ../unix/Rsock.c
LIBFILES=$(foreach i, $(PACKAGES), ../../library/$i)

OBJSO=$(foreach i, $(SOURCES) $(UFILES) $(EXTRAFILES), $(basename $i).o)
OBJS-NO=
OBJS=$(filter-out $(OBJS-NO),$(OBJSO))
PKGS=$(foreach i, $(PACKAGES), pkg-$i)
DIRSOBJ=$(foreach i, $(DIRS) ../unix , $i/*.o)
EXTRALIBS=rx.a ga.a gl.a xdr.a em.a

.PHONY:  clean veryclean  rlibs fixfiles front-ends

all: rbuild rdemo rpackage

CFLAGS=$(OPTFLAGS) -I../include
FFLAGS=$(OPTFLAGS)

../main/saveload-CFLAGS=-Ixdr
../main/character-CFLAGS=-Iregex
../main/platform-CFLAGS=-Iregex
../unix/sock-CFLAGS=-DWin32
../unix/Rsock-CFLAGS=-DWin32

rbuild: fixfiles makeMakedeps rlibs ../../bin/R.dll libR.a \
	../include/globalvar.h front-ends COPYRIGHTS

rpackage: $(PKGS)

rinstaller:
	$(MAKE) -C installer PREFIX=$(RPREFIX)

rdemo:  libR.a
	make -C ../../demos/dynload -f ../../src/gnuwin32/MkDyndemo

rlibs $(EXTRALIBS):
	$(MAKE) -C ./regex
	$(MAKE) -C ./graphapp
	$(MAKE) -C ./getline
	$(MAKE) -C ./xdr
	$(MAKE) -C ./math

COPYRIGHTS: ../../COPYRIGHTS COPYRIGHTS.win
	cat $^ > $@

fixfiles:
	$(MKDIR) -p ../../bin 
	$(MAKE) -C ./fixed
	$(CP) -pr unzip ../..
	$(RM) ../../unzip/*.h

R.dll: R.a $(EXTRALIBS)

../../bin/R.dll: fixfiles rlibs R.dll
	$(MKDIR) -p ../../bin
	$(CP) R.dll ../../bin/

front-ends: libR.a
	$(MAKE) -C front-ends

R.a: $(OBJS)
	@$(ECHO) -------- Building $@ --------
	$(AR) cr R.a $(DIRSOBJ)
	$(RANLIB) R.a


# R.dll exports all global symbols BUT import library (libR.a)
# only (all functions) + (global vars listed  in exported-vars))

libR.a:  R.exp
	$(DLLTOOL) $(DLLTOOLFLAGS) $(R-DLLTOOLFLAGS) --dllname R.dll --def R.exp --output-lib libR.a

# This fails on djtools make:
R.exp: R.exp1
	(diff R.exp R.exp1 > /dev/null) || $(CP) R.exp1 R.exp

R.exp1: R.a $(EXTRALIBS) exported-vars
	$(ECHO) EXPORTS > $@
	$(NM) R.a $(EXTRALIBS) > RDefs
	$(SED) -n "/^........ [T] _/s/^........ [T] _/ /p" RDefs >> $@
	$(CAT) exported-vars >> $@
	$(RM) RDefs

../main/gram.c: ;

../nmath/fround.o: ../nmath/fround.c
	$(CC) -I../include  -o $@ -c $^

# avoid building globalvar.h with errors
../include/globalvar.h: exported-vars
	$(AWK) -- '//{if ($$1 != "") print "#define " $$1 " (*__imp_" $$1 ")"}' \
	           exported-vars > ../include/globalvar.h

base-HELP=ziponly

pkg-%:  libR.a #../include/globalvar.h
	$(MAKE) DLLNM=$($*-DLLNM)  EXTRADOCS=$($*-EXTRADOCS) -C ../library/$* PKG=$* -f ../../gnuwin32/MakePkg
ifeq ($(strip $(HELP)),YES)
	$(MAKE) -C ./help $($*-HELP)help-$*
	$(MAKE) -C ./help contents-$*
endif

pkgcheck-%:
	@$(ECHO) -------- Testing package $* --------
	-@mkdir -p ../library/$*/check
	@$(MAKE) -C ../library/$*/check PKG=$* -f ../../../gnuwin32/check/PkgCheck

ziphelp-%:
	$(MAKE) -C ./help ziphelp-$*
	$(MAKE) -C ./help contents-$*

ziponly-%:
	$(MAKE) -C ./help ziponlyhelp-$*
	$(MAKE) -C ./help contents-$*

bootstrap-DLLNM=boott

clean: cleanwin
	$(RM) -f $(OBJS) $(WINOBJS) *.a ../*/*.d

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
	-$(MAKE) -C installer clean


bindist:
	$(RM) -rf $(RPREFIX) library
	$(MKDIR) $(RPREFIX)
	$(CP) -pr ../../bin ../../afm ../../demos  ../../unzip $(RPREFIX)
	$(MKDIR) $(RPREFIX)/library
	$(CP) -pr $(LIBFILES) $(RPREFIX)/library
	$(RM) $(RPREFIX)/demos/dynload/*.o
	$(MKDIR) $(RPREFIX)/etc
	$(CP) -p ../../etc/Rprofile $(RPREFIX)/etc/Rprofile
	$(CP) -p ../../etc/Rconsole $(RPREFIX)/etc/Rconsole
	$(CP) -p ../../etc/Rdevga $(RPREFIX)/etc/Rdevga
	$(CP) -p ../../etc/rgb.txt $(RPREFIX)/etc/rgb.txt
	$(MKDIR) $(RPREFIX)/doc
	$(CP) -pr ../../doc/html $(RPREFIX)/doc
	$(CP) -p ../../COPYING COPYRIGHTS ../../FAQ ../../NEWS \
		../../Y2K $(RPREFIX)
	$(CP) -p readme $(RPREFIX)/README
	$(CP) -p rw-faq $(RPREFIX)/rw-faq
	$(CP) -p Announce $(RPREFIX)/Announce
	$(CP) -p CHANGES $(RPREFIX)/CHANGES
	zip -l docs.zip $(RPREFIX)/*
	unzip -o docs.zip
	$(RM) docs.zip
	$(RM) -rf $(RPREFIX)b.zip $(RPREFIX)h.zip $(RPREFIX)w.zip $(RPREFIX)l.zip
	$(RM) anindex.zip
	zip -mrX anindex.zip $(RPREFIX)/library/*/help/AnIndex $(RPREFIX)/library/*/help/00Titles
	zip -mrX $(RPREFIX)h.zip $(RPREFIX)/library/*/help \
		$(RPREFIX)/library/*/R-ex
	unzip anindex.zip
	$(RM) anindex.zip
	zip -rmX $(RPREFIX)w.zip $(RPREFIX)/doc/html $(RPREFIX)/library/*/html
	zip -rmX $(RPREFIX)l.zip $(RPREFIX)/library/*/latex
	zip -rX $(RPREFIX)b.zip $(RPREFIX)
	$(RM) -rf $(RPREFIX)



srcdist:
	$(RM) -r ../src src
	$(MKDIR) ../src
	$(MKDIR)  ../src/gnuwin32
	$(CP)  -pr ./* ../src/gnuwin32
	$(RM) -r ../src/gnuwin32/old ../src/gnuwin32/*.zip
	$(RM) -r ../src/gnuwin32/installer/unzip
	$(MAKE) -C ../src/gnuwin32 cleanwin
	mv ../src .
	$(RM) $(RPREFIX)s.zip
	zip -rX  $(RPREFIX)s.zip src
	$(RM) -r src


#taken from ../include/makefile.in
SRC_HEADERS = S.h S_compat.h f2c.h Arith.h Complex.h Error.h Errormsg.h \
	 Fortran.h Applic.h Blas.h Linpack.h Mathlib.h Memory.h PrtUtil.h \
	 Random.h Rinternals.h Utils.h Platform.h FFDecl.h FFTab.h globalvar.h

packagedist:
	$(RM) -r $(RPREFIX)sp.zip $(RPREFIX)
	$(MKDIR) $(RPREFIX) $(RPREFIX)/src $(RPREFIX)/src/include 
	$(MKDIR) $(RPREFIX)/src/library $(RPREFIX)/src/gnuwin32 
	$(MKDIR) $(RPREFIX)/src/gnuwin32/help
	$(CP) -p ../../COPYING exported-vars R.exp MkRules MakePkg MakeDll $(RPREFIX)/src/gnuwin32
	$(CP) -p Makefile.packages $(RPREFIX)/src/gnuwin32/Makefile
	$(CP) -p help/* $(RPREFIX)/src/gnuwin32/help
	$(MAKE) -C $(RPREFIX)/src/gnuwin32/help clean
	$(CP) -p $(foreach i,$(SRC_HEADERS),../include/$i) $(RPREFIX)/src/include
	$(CP) readme.packages $(RPREFIX)
	zip -r $(RPREFIX)sp.zip $(RPREFIX)
	$(RM) -r $(RPREFIX)


distribution: bindist packagedist

# tests
check: all test
test: stricttests pkgtests

stricttests:
	-@$(MAKE) -C ../../tests -f ../src/gnuwin32/check/Maketests clean
	-@$(MAKE) -C ../../tests -f ../src/gnuwin32/check/Maketests

PKGS-CHK=$(foreach i, $(PACKAGES), pkgcheck-$i)
pkgtests: $(PKGS-CHK)

pkgcheck-base:
	@$(ECHO) -------- Testing package base --------
	@(cd ../../library/base/R-ex; unzip -oq Rex)
	-@mkdir -p ../library/base/check
	-@$(MAKE) -C ../library/base/check PKG=base -f ../../../gnuwin32/check/PkgCheck
	@(cd ../../library/base/R-ex; rm -f *.R)

# Dependencies
CSOURCES=$(foreach i, $(DIRS), $(wildcard $i/*.c))
DEPS=$(foreach i, $(CSOURCES) $(UFILES), $(basename $i).d)

.c.d:
	@echo "making $@ from $<"
	@$(CC)  -M $(CFLAGS) $($*-CFLAGS) $< -o $@

makeMakedeps: $(DEPS)
	@$(RM) Makedeps
	@cat $(DEPS) >> Makedeps

-include Makedeps
