include ../MkRules

all: profiles fixh fixr fixbin fixetc

profiles: ../../../library/base/R/Rprofile

../../../library/base/R/Rprofile:  ../../library/profile/Common.R \
../../library/profile/Rprofile.gnw
	@$(ECHO) -------- Building $@ from $^--------
	mkdir -p ../../../library/base/R
	$(CAT)  $^ > $@


SED0='/^\#/d'
SED01='/Rsockfork/d'
SED1='s/F77_SUBROUTINE(\(.*\))/void * \1_();/'
SED2='s/C_FUNCTION(\(.*\))/void * \1();/'
SED3='s/F77_SUBROUTINE(\(.*\))/	{ "\1_",	\1_},/'
SED4='s/C_FUNCTION(\(.*\))/{ "\1",	\1 },/'

../../include/FFDecl.h: ../../appl/ROUTINES
	$(SED) -e $(SED0) -e $(SED01) -e $(SED1) -e $(SED2) ../../appl/ROUTINES > ../../include/FFDecl.h
../../include/FFTab.h: ../../appl/ROUTINES
	$(SED) -e $(SED0) -e $(SED01) -e $(SED3) -e $(SED4) ../../appl/ROUTINES > ../../include/FFTab.h


fixh: h/config.h ../../include/FFDecl.h ../../include/FFTab.h \
  ../../include/Rconfig.h ../../include/Rversion.h
	$(CP) -p ./h/config.h ./h/psignal.h ../../include
	$(ECHO) done > fixh

../../include/Rconfig.h: h/config.h
	sh ./GETCONFIG > ../../include/Rconfig.h

../../include/Rversion.h: ../../../date-stamp ../../../VERSION
	sh ./GETVERSION > ../../include/Rversion.h

fixbin: $(filter-out ./bin/CVS, $(wildcard ./bin/*))
	$(CP) -p  $^ ../../../bin
	$(ECHO) done > fixbin

fixr: $(wildcard ./r/*.html) ../../../doc/html/search/SearchEngine.html
	$(CP) -p ./r/*.html ../../../doc/html
	$(ECHO) done > fixr

fixetc: $(filter-out ./etc/CVS, $(wildcard ./etc/*))
	$(CP) -p $^ ../../../etc
	$(ECHO) done > fixetc

clean:
	$(RM) *~ */*~ fixr fixh fixbin fixetc

../../../doc/html/search/SearchEngine.html:../../../doc/KEYWORDS.db
	(cd ../../../doc; \
	cat html/search/SearchEngine-head.html > html/search/SearchEngine.html; \
	perl ../src/gnuwin32/fixed/keywords2html KEYWORDS.db >> html/search/SearchEngine.html; \
	cat html/search/SearchEngine-foot.html >> html/search/SearchEngine.html)
