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

../FFDecl.h: ../../appl/ROUTINES
	$(SED) -e $(SED0) -e $(SED01) -e $(SED1) -e $(SED2) $< > $@
../FFTab.h: ../../appl/ROUTINES
	$(SED) -e $(SED0) -e $(SED01) -e $(SED3) -e $(SED4) $< > $@


fixh: h/config.h ../FFDecl.h ../FFTab.h \
  ../../include/Rconfig.h ../../include/Rversion.h ../maketitle.awk
	$(CP) -p ./h/config.h ./h/psignal.h ../../include
	$(ECHO) done > fixh

../maketitle.awk: ../../scripts/maketitle.awk
	$(CP) $< $@

../../include/Rconfig.h: h/config.h
	$(SED) -e 1d ../../../tools/GETCONFIG > GC
	(cd h; sh ../GC > ../../../include/Rconfig.h)
	$(RM) GC

../../include/Rversion.h: ../../../date-stamp ../../../VERSION
	$(SED) -e 1d ../../../tools/GETVERSION > ../../../tools/GV
	sh ../../../tools/GV > $@
	$(RM) ../../../tools/GV

../../../bin/massage-Examples.sh: ../../scripts/massage-Examples
	$(SED) -e 1d  -e 's/"contr.poly")/"contr.poly"), pager="console"'/ $< > $@

../../../bin/check: ../../scripts/check.in
	$(SED) -e 1d -e "s/R CMD/Rcmd/" $< > $@

../../../bin/build: ../../scripts/build.in
	$(SED) -e 1d -e "s/R CMD/Rcmd/" $< > $@

../../../bin/Sd2Rd: ../../scripts/Sd2Rd.in
	$(SED) -e 1d -e "s/R CMD/Rcmd/" $< > $@

../../../bin/Rdconv: ../../scripts/Rdconv.in
	$(SED) -e 1d -e s/\"unix\"/\"windows\"/ -e "s/R CMD/Rcmd/" $< > $@

../../../bin/Rdindex: ../../scripts/Rdindex.in
	$(SED) -e 1d -e s/\"unix\"/\"windows\"/ -e "s/R CMD/Rcmd/" $< > $@

../../../bin/extract-usage: ../../scripts/extract-usage.in
	$(SED) -e 1d -e s/unix/windows/  $< > $@

../../../bin/Rdiff.sh: ../../scripts/Rdiff
	$(SED) -e 1d $< > $@

fixbin: ../../../bin/massage-Examples.sh ../../../bin/build \
	   ../../../bin/check ../../../bin/Rdconv ../../../bin/Rdindex \
	   ../../../bin/Sd2Rd ../../../bin/extract-usage \
	   ../../../bin/Rdiff.sh cp2bin
	$(ECHO) done > fixbin

cp2bin: $(filter-out ./bin/CVS, $(wildcard ./bin/*))
	zip -q bins $^
	unzip -oaq bins -d  ../../..
	$(RM) bins.zip

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
	perl ../tools/keywords2html.pl KEYWORDS.db >> html/search/SearchEngine.html; \
	cat html/search/SearchEngine-foot.html >> html/search/SearchEngine.html)
