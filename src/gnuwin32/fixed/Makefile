include ../MkRules

MANOBJS= BATCH.Rd COMPILE.Rd INSTALL.Rd REMOVE.Rd RHOME.Rd SHLIB.Rd gnome.Rd hdf5.Rd x11.Rd

all: profiles fixh fixr fixRd fixbin fixmake

profiles: ../../../library/base/R/Rprofile ../../../etc/Rprofile

../../../library/base/R/Rprofile:  ../../library/profile/Common.R ./profile/rprofile
	@$(ECHO) -------- Building $@ from $^--------
	mkdir -p ../../../library/base/R
	$(CAT)  $^ > $@

../../../etc/Rprofile: ./profile/rprofile.site
	$(MKDIR) -p ../../../etc
	$(CP) -p $^ $@

SED0='/^\#/d'
SED01='/Rsockfork/d'
SED1='s/F77_SUBROUTINE(\(.*\))/int \1_();/'
SED2='s/C_FUNCTION(\(.*\))/int \1();/'
SED3='s/F77_SUBROUTINE(\(.*\))/	{ "\1_",	\1_},/'
SED4='s/C_FUNCTION(\(.*\))/{ "\1",	\1 },/'

fixh: h/Rconfig.h ../../../date-stamp ../../appl/ROUTINES
	$(CP) -p ./h/Rconfig.h ./h/psignal.h ../../include
	sh ./GETVERSION > ../../include/Rversion.h
	$(SED) -e $(SED0) -e $(SED01) -e $(SED1) -e $(SED2) ../../appl/ROUTINES > ../../include/FFDecl.h
	$(SED) -e $(SED0) -e $(SED01) -e $(SED3) -e $(SED4) ../../appl/ROUTINES > ../../include/FFTab.h
	$(ECHO) done > fixh

fixbin: $(filter-out ./bin/CVS,$(wildcard ./bin/*))
	$(CP) -p  $^ ../../../bin
	$(ECHO) done > fixbin

fixr: $(wildcard ./r/*.r) ./r/zero.R $(wildcard ./r/*.html)  \
      ./profile/rconsole ./profile/rgb.txt ./profile/rdevga
	$(CP) -p ./r/*.gnw.r ../../library/base/R
	$(CP) -p ./r/zero.R ../../../demos/dynload/zero.R
	$(CP) -p ./r/*.html ../../../doc/html
	$(CP) -p ./profile/rconsole ../../../etc/Rconsole
	$(CP) -p ./profile/rdevga ../../../etc/Rdevga	
	$(CP) -p ./profile/rgb.txt ../../../etc
	$(ECHO) done > fixr		

fixRd: $(wildcard man/*.Rd)
	-$(RM) $(foreach i, $(MANOBJS), ../../library/base/man/$i)
	$(CP) -p man/*.Rd  ../../library/base/man
	$(ECHO) done > fixRd	

fixmake: ../../appl/Makefile ../../main/Makefile ../../nmath/Makefile

../../appl/Makefile: make.appl
	$(CP) -p $^ $@

../../main/Makefile: make.main
	$(CP) -p $^ $@

../../nmath/Makefile: make.nmath
	$(CP) -p $^ $@

clean:
	$(RM) *~ */*~ fixr fixh fixRd fixbin
