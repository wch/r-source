include ../MkRules

MANOBJS= BATCH.Rd COMPILE.Rd INSTALL.Rd REMOVE.Rd RHOME.Rd SHLIB.Rd gnome.Rd hdf5.Rd x11.Rd

all: profiles fixh fixr fixbin fixetc

profiles: ../../../library/base/R/Rprofile

../../../library/base/R/Rprofile:  ../../library/profile/Common.R \
../../library/profile/Rprofile.gnw
	@$(ECHO) -------- Building $@ from $^--------
	mkdir -p ../../../library/base/R
	$(CAT)  $^ > $@


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

fixr: ./r/zero.R $(wildcard ./r/*.html)
	$(CP) -p ./r/zero.R ../../../demos/dynload/zero.R
	$(CP) -p ./r/*.html ../../../doc/html
	$(ECHO) done > fixr

fixetc: ./etc/Rconsole ./etc/rgb.txt ./etc/Rdevga ./etc/Rprofile
	$(CP) -p $^ ../../../etc
	$(ECHO) done > fixetc

fixRd: $(wildcard man/*.Rd)
	-$(RM) $(foreach i, $(MANOBJS), ../../library/base/man/$i)
	$(CP) -p man/*.Rd  ../../library/base/man
	$(ECHO) done > fixRd

clean:
	$(RM) *~ */*~ fixr fixh fixRd fixbin
