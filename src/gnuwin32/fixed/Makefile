include ../MkRules

R_HOME = ../../..

all: profiles fixhtml fixbin fixetc fixshare ../XINSTALL svnonly

profiles: $(R_HOME)/library/base/R/Rprofile

$(R_HOME)/library/base/R/Rprofile:  ../../library/profile/Common.R \
../../library/profile/Rprofile.windows
	@$(ECHO) -------- Building $@ from $^--------
	mkdir -p $(R_HOME)/library/base/R
	@$(CAT)  $^ > $@

fixbin:  cp2bin
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

svnonly:
	@$(MAKE) -C ../../../doc/manual -f Makefile.win svnonly

clean:
	$(RM) *~ */*~ fixhtml fixbin fixetc fixshare

distclean:

$(R_HOME)/doc/html/search/SearchEngine.html: $(R_HOME)/doc/html/search/SearchEngine-head.html $(R_HOME)/doc/html/search/SearchEngine-foot.html $(R_HOME)/doc/KEYWORDS.db
	(cd $(R_HOME)/doc; \
	cat html/search/SearchEngine-head.html > html/search/SearchEngine.html; \
	perl ../tools/keywords2html.pl KEYWORDS.db >> html/search/SearchEngine.html; \
	cat html/search/SearchEngine-foot.html >> html/search/SearchEngine.html)
../XINSTALL: bin/INSTALL
ifeq ($(strip $(BUILD)),CROSS)
	@$(SED) -e 's/"MINGW"/"CROSS"/' $< > $@
endif
