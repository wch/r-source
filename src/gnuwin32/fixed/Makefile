include ../MkRules

all: profiles fixh fixhtml fixbin fixetc fixshare

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

../FFDecl.h: ../../appl/ROUTINES
	$(SED) -e $(SED0) -e $(SED01) -e $(SED1) -e $(SED2) $< > $@
../FFTab.h: ../../appl/ROUTINES
	$(SED) -e $(SED0) -e $(SED01) -e $(SED3) -e $(SED4) $< > $@


version: ../../include/Rversion.h
fixh: version h/config.h ../FFDecl.h ../FFTab.h \
  ../../include/Rconfig.h  ../../include/Rmath.h
	$(CP) -p ./h/config.h ./h/psignal.h ../../include
	$(ECHO) done > fixh

../../include/Rconfig.h: h/config.h
	$(SED) -e 1d ../../../tools/GETCONFIG > GC
	(cd h; sh ../GC > ../../../include/Rconfig.h)
	$(RM) GC

../../include/Rversion.h: ../../../date-stamp ../../../VERSION
	$(SED) -e 1d ../../../tools/GETVERSION > ../../../tools/GV
	sh ../../../tools/GV > $@
	$(RM) ../../../tools/GV

../../include/Rmath.h: ../../include/Rmath.h.in
	$(SED) -e /@RMATH_HAVE_LOG1P@/d  -e /@RMATH_HAVE_EXPM1@/d $< > $@

../../../bin/massage-Examples: ../../../share/perl/massage-Examples.pl
	$(SED) -e 's/"contr.poly")/"contr.poly"), pager="console"/' $< > $@

../../../bin/check: ../../scripts/check.in
	$(SED) -e 1d -e "s/R CMD/Rcmd/" -e "s/Rd2dvi /Rd2dvi.sh /" $< > $@

../../../bin/build: ../../scripts/build.in
	$(SED) -e 1d -e "s/R CMD/Rcmd/" $< > $@

../../../bin/Sd2Rd: ../../scripts/Sd2Rd.in
	$(SED) -e 1d -e "s/R CMD/Rcmd/" $< > $@

../../../bin/Rdconv: ../../scripts/Rdconv.in
	$(SED) -e 1d -e s/\"unix\"/\"windows\"/ -e "s/R CMD/Rcmd/" $< > $@

../../../bin/Rdindex: ../../scripts/Rdindex.in
	$(SED) -e 1d -e s/\"unix\"/\"windows\"/ -e "s/R CMD/Rcmd/" $< > $@

../../../bin/Rdiff.sh: ../../scripts/Rdiff
	$(SED) -e 1d $< > $@

../../../bin/Rprof: ../../scripts/Rprof.in
	$(SED) -e 1d -e "s/R CMD/Rcmd/" $< > $@

fixbin: ../../../bin/massage-Examples ../../../bin/build \
	   ../../../bin/check ../../../bin/Rdconv ../../../bin/Rdindex \
	   ../../../bin/Sd2Rd ../../../bin/Rdiff.sh ../../../bin/Rprof cp2bin
	$(ECHO) done > fixbin

cp2bin: $(filter-out ./bin/CVS, $(wildcard ./bin/*))
	zip -q bins $^
	unzip -oaq bins -d  ../../..
	$(RM) bins.zip

fixhtml: $(wildcard ./html/*.html) ../../../doc/html/search/SearchEngine.html
	$(CP) -p ./html/*.html ../../../doc/html
	$(CP) -p ./html/rwin.html ../../../doc/html/index.html
	$(ECHO) done > fixhtml

fixetc: $(filter-out ./etc/CVS, $(wildcard ./etc/*))
	$(CP) -p $^ ../../../etc
	$(ECHO) done > fixetc

fixshare: ./share/tests.mk
	$(MKDIR) -p ../../../share/make
	$(CP) -p $^ ../../../share/make/wintests.mk
	$(ECHO) done > fixshare	

clean:
	$(RM) *~ */*~ fixhtml fixh fixbin fixetc fixshare

distclean: clean
	$(RM) -f ../FFDecl.h ../FFTab.h ../../../tools/GV

../../../doc/html/search/SearchEngine.html:../../../doc/KEYWORDS.db
	(cd ../../../doc; \
	cat html/search/SearchEngine-head.html > html/search/SearchEngine.html; \
	perl ../tools/keywords2html.pl KEYWORDS.db >> html/search/SearchEngine.html; \
	cat html/search/SearchEngine-foot.html >> html/search/SearchEngine.html)
