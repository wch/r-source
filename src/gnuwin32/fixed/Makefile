include ../MkRules

R_HOME = ../../..

all: profiles fixh fixhtml fixbin fixetc fixshare

profiles: $(R_HOME)/library/base/R/Rprofile

$(R_HOME)/library/base/R/Rprofile:  ../../library/profile/Common.R \
../../library/profile/Rprofile.windows
	@$(ECHO) -------- Building $@ from $^--------
	mkdir -p $(R_HOME)/library/base/R
	@$(CAT)  $^ > $@


fixh: h/config.h h/psignal.h
	@$(CP) -p ./h/config.h ./h/psignal.h ../../include
	@$(ECHO) done > fixh

$(R_HOME)/bin/massage-Examples: $(R_HOME)/share/perl/massage-Examples.pl
	$(SED) -e 's/"contr.poly")/"contr.poly"), pager="console"/' $< > $@

$(R_HOME)/bin/check: ../../scripts/check.in
	$(SED) -e 1d $< > $@

$(R_HOME)/bin/build: ../../scripts/build.in
	$(SED) -e 1d $< > $@

$(R_HOME)/bin/Sd2Rd: ../../scripts/Sd2Rd.in
	$(SED) -e 1d $< > $@

$(R_HOME)/bin/Rdconv: ../../scripts/Rdconv.in
	$(SED) -e 1d -e s/\"unix\"/\"windows\"/ $< > $@

$(R_HOME)/bin/Rdiff.sh: ../../scripts/Rdiff
	$(SED) -e 1d -e 's/\$${EGREP}/grep -E/' $< > $@

$(R_HOME)/bin/Rprof: ../../scripts/Rprof.in
	$(SED) -e 1d $< > $@

fixbin: $(R_HOME)/bin/massage-Examples $(R_HOME)/bin/build \
	   $(R_HOME)/bin/check $(R_HOME)/bin/Rdconv \
	   $(R_HOME)/bin/Sd2Rd $(R_HOME)/bin/Rdiff.sh $(R_HOME)/bin/Rprof cp2bin
	@$(ECHO) done > fixbin

cp2bin: $(filter-out ./bin/CVS, $(wildcard ./bin/*))
ifeq ($(strip $(BUILD)),CROSS)
	@zip -ql bins $^
	@unzip -oq bins -d  $(R_HOME)
else
	@zip -q bins $^
	@unzip -oaq bins -d  $(R_HOME)
endif
	@$(RM) bins.zip

fixhtml:  html/rwin.html $(R_HOME)/doc/html/search/SearchEngine.html
	$(CP) -p html/rwin.html $(R_HOME)/doc/html
	$(CP) -p html/rwin.html $(R_HOME)/doc/html/index.html
	@$(ECHO) done > fixhtml

fixetc: $(filter-out ./etc/CVS, $(wildcard ./etc/*))
	$(CP) -p $^ $(R_HOME)/etc
	@$(ECHO) done > fixetc

fixshare: share/tests.mk
	$(MKDIR) -p $(R_HOME)/share/make
	$(CP) -p $^ $(R_HOME)/share/make/wintests.mk
	@$(ECHO) done > fixshare	

clean:
	$(RM) *~ */*~ fixhtml fixh fixbin fixetc fixshare

distclean:

$(R_HOME)/doc/html/search/SearchEngine.html: $(R_HOME)/doc/html/search/SearchEngine-head.html $(R_HOME)/doc/html/search/SearchEngine-foot.html $(R_HOME)/doc/KEYWORDS.db
	(cd $(R_HOME)/doc; \
	cat html/search/SearchEngine-head.html > html/search/SearchEngine.html; \
	perl ../tools/keywords2html.pl KEYWORDS.db >> html/search/SearchEngine.html; \
	cat html/search/SearchEngine-foot.html >> html/search/SearchEngine.html)
