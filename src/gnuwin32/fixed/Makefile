include ../MkRules

all: profiles fixh fixhtml fixbin fixetc fixshare

profiles: ../../../library/base/R/Rprofile

../../../library/base/R/Rprofile:  ../../library/profile/Common.R \
../../library/profile/Rprofile.windows
	@$(ECHO) -------- Building $@ from $^--------
	mkdir -p ../../../library/base/R
	@$(CAT)  $^ > $@


fixh: h/config.h h/psignal.h
	@$(CP) -p ./h/config.h ./h/psignal.h ../../include
	@$(ECHO) done > fixh

../../../bin/massage-Examples: ../../../share/perl/massage-Examples.pl
	$(SED) -e 's/"contr.poly")/"contr.poly"), pager="console"/' $< > $@

../../../bin/check: ../../scripts/check.in
	$(SED) -e 1d $< > $@

../../../bin/build: ../../scripts/build.in
	$(SED) -e 1d $< > $@

../../../bin/Sd2Rd: ../../scripts/Sd2Rd.in
	$(SED) -e 1d $< > $@

../../../bin/Rdconv: ../../scripts/Rdconv.in
	$(SED) -e 1d -e s/\"unix\"/\"windows\"/ $< > $@

../../../bin/Rdiff.sh: ../../scripts/Rdiff
	$(SED) -e 1d -e 's/\$${EGREP}/grep -E/' $< > $@

../../../bin/Rprof: ../../scripts/Rprof.in
	$(SED) -e 1d $< > $@

fixbin: ../../../bin/massage-Examples ../../../bin/build \
	   ../../../bin/check ../../../bin/Rdconv \
	   ../../../bin/Sd2Rd ../../../bin/Rdiff.sh ../../../bin/Rprof cp2bin
	@$(ECHO) done > fixbin

cp2bin: $(filter-out ./bin/CVS, $(wildcard ./bin/*))
ifeq ($(strip $(BUILD)),CROSS)
	@zip -ql bins $^
	@unzip -oq bins -d  ../../..
else
	@zip -q bins $^
	@unzip -oaq bins -d  ../../..
endif
	@$(RM) bins.zip

fixhtml: $(wildcard ./html/*.html) ../../../doc/html/search/SearchEngine.html
	$(CP) -p ./html/*.html ../../../doc/html
	$(CP) -p ./html/rwin.html ../../../doc/html/index.html
	@$(ECHO) done > fixhtml

fixetc: $(filter-out ./etc/CVS, $(wildcard ./etc/*))
	$(CP) -p $^ ../../../etc
	@$(ECHO) done > fixetc

fixshare: ./share/tests.mk
	$(MKDIR) -p ../../../share/make
	$(CP) -p $^ ../../../share/make/wintests.mk
	@$(ECHO) done > fixshare	

clean:
	$(RM) *~ */*~ fixhtml fixh fixbin fixetc fixshare

distclean:

../../../doc/html/search/SearchEngine.html: ../../../doc/html/search/SearchEngine-head.html ../../../doc/html/search/SearchEngine-foot.html ../../../doc/KEYWORDS.db
	(cd ../../../doc; \
	cat html/search/SearchEngine-head.html > html/search/SearchEngine.html; \
	perl ../tools/keywords2html.pl KEYWORDS.db >> html/search/SearchEngine.html; \
	cat html/search/SearchEngine-foot.html >> html/search/SearchEngine.html)
