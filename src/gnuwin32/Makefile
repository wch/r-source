include MkRules

HELP=YES

PACKAGES=base eda lqs modreg mva stepfun
RPREFIX=rw0641-3

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
SOURCES=$(foreach  i,$(DIRS),$(wildcard $i/*.c $i/*.f))
UFILES= ../unix/devPS.c ../unix/devPicTeX.c ../unix/sock.c ../unix/Rsock.c
LIBFILES=$(foreach i, $(PACKAGES), ../../library/$i)

OBJSO=$(foreach i, $(SOURCES) $(UFILES) $(EXTRAFILES), $(basename $i).o)
OBJS-NO=
OBJS=$(filter-out $(OBJS-NO),$(OBJSO))
PKGS=$(foreach i, $(PACKAGES), pkg-$i)
DIRSOBJ=$(foreach i, $(DIRS) ../unix , $i/*.o)
EXTRALIBS=rx.a ga.a gl.a xdr.a em.a

.PHONY:  clean veryclean  rlibs fixfiles front-ends

all: rbuild rdemo rpackage rinstaller

CFLAGS=$(OPTFLAGS) -I../include
FFLAGS=$(OPTFLAGS)

../main/saveload-CFLAGS=-Ixdr
../main/character-CFLAGS=-Iregex
../main/platform-CFLAGS=-Iregex
../unix/sock-CFLAGS=-DWin32
../unix/Rsock-CFLAGS=-DWin32

rbuild: fixfiles rlibs ../../bin/R.dll libR.a ../include/globalvar.h \
	front-ends COPYRIGHTS

rpackage: $(PKGS)

rinstaller:
	$(MAKE) -C installer PREFIX=$(PREFIX)

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
	($(AWK) -- '//{if ($$1 != "") print "#define " $$1 " (*__imp_" $$1 ")"}' \
	           exported-vars > ../include/globalvar.h) || $(RM) $@

base-HELP=ziponly

pkg-%:  libR.a #../include/globalvar.h
	$(MAKE) DLLNM=$($*-DLLNM)  EXTRADOCS=$($*-EXTRADOCS) -C ../library/$* PKG=$* -f ../../gnuwin32/MakePkg
ifeq ($(strip $(HELP)),YES)
	$(MAKE) -C ./help $($*-HELP)help-$*
	$(MAKE) -C ./help contents-$*
endif

pkgcheck-%:
	$(MAKE) -C ../library/$* PKG=$* -f ../../gnuwin32/check/PkgCheck

ziphelp-%:
	$(MAKE) -C ./help ziphelp-$*
	$(MAKE) -C ./help contents-$*

ziponly-%:
	$(MAKE) -C ./help ziponlyhelp-$*
	$(MAKE) -C ./help contents-$*

integrate-DLLNM=adapt
bootstrap-DLLNM=boott

clean: cleanwin
	$(RM) -f $(OBJS) $(WINOBJS) *.a

veryclean: clean
	$(RM) -rf ../../bin/R.exe ../../library
	$(RM) -f ../library/*/src/*.o ../library/*/src/*.a
	$(RM) -f ../library/*/src/*.def ../library/*/src/*.dll

cleanwin:
	$(RM) *.o *.exe *.dll *.a *~ \#*\# *.zip R.def R.exp1 .RData .Rhistory
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
	$(MAKE) -C ../src/gnuwin32 cleanwin
	mv ../src .
	$(RM) $(RPREFIX)s.zip
	zip -rX  $(RPREFIX)s.zip src
	$(RM) -r src


#taken from ../include/makefile.in
SRC_HEADERS = S.h S_compat.h f2c.h Arith.h Complex.h Error.h Errormsg.h \
	 Fortran.h Applic.h Blas.h Linpack.h Mathlib.h Memory.h PrtUtil.h \
	 Random.h Utils.h Platform.h FFDecl.h FFTab.h globalvar.h

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


distribution: bindist srcdist packagedist

# Dependencies
console.o:		console.h rui.h graphapp/ga.h graphapp/stdimg.h
devga.o:  		console.h rui.h devga.h opt.h graphapp/ga.h \
			graphapp/stdimg.h
dynload.o: 		../include/FFDecl.h ../include/FFTab.h
edit.o: 		run.h
extra.o: 		graphapp/ga.h
rhome.o: 		../include/Platform.h
rui.o: 			console.h opt.h rui.h ../include/Platform.h \
			graphapp/ga.h graphapp/stdimg.h
run.o: 			run.h graphapp/ga.h
system.o: 		devga.h run.h console.h rui.h graphapp/ga.h \
                        ../include/Defn.h ../include/Platform.h


../main/colors.o:	../include/Graphics.h
../main/graphics.o:	../include/Graphics.h
../main/par.o:		../include/Graphics.h
../main/plot.o:		../include/Graphics.h

../main/arithmetic.o:	../include/Mathlib.h ../include/Arith.h \
			../include/Platform.h
../main/format.o:	../include/Mathlib.h ../include/Arith.h
../main/random.o:	../include/Mathlib.h ../include/Arith.h ../include/Random.h

../main/deparse.o:	../main/names.h
../main/names.o:	../main/names.h

../main/print.o:	../include/Print.h
../main/printarray.o:	../include/Print.h
../main/printutils.o:	../include/Print.h
../main/printvector.o:	../include/Print.h

../main/version.o:	../include/Platform.h

../nmath/snorm.o:	../include/Random.h
../nmath/sunif.o:	../include/Random.h


../unix/devPS.o:        ../include/Graphics.h ../include/Defn.h
../unix/devPicTeX.o:    ../include/Graphics.h ../include/Defn.h
