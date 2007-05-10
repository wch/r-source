
include ../MkRules

R_HOME = ../../..
ifneq ($(strip $(BUILD)),CROSS)
RHOME?=$(shell ../Rpwd.exe $(R_HOME))
else
RHOME=$(shell $(PERL) ../pwd.pl $(R_HOME))
endif

all: profiles fixhtml fixbin fixetc fixshare ../XINSTALL svnonly

profiles: $(R_HOME)/library/base/R/Rprofile

$(R_HOME)/library/base/R/Rprofile:  ../../library/profile/Common.R \
../../library/profile/Rprofile.windows
	@$(ECHO) -------- Building $@ from $^--------
	mkdir -p $(R_HOME)/library/base/R
	@$(CAT)  $^ > $@

fixbin:  cp2bin
	@$(ECHO) done > fixbin

cp2bin: $(wildcard ./bin/*)
ifeq ($(strip $(BUILD)),CROSS)
	@zip -ql bins $^
	@unzip -oq bins -d  $(R_HOME)
else
	@zip -q bins $^
	@unzip -oaq bins -d  $(R_HOME)
endif
	@$(RM) bins.zip

fixhtml:  html/rwin.html $(R_HOME)/doc/html/search/SearchEngine.html
	$(CP) -p html/rwin.html $(R_HOME)/doc/html/index.html
	@$(ECHO) done > fixhtml

fixetc: $(wildcard ./etc/*)
	$(CP) -p $^ $(R_HOME)/etc
	@$(ECHO) done > fixetc

fixshare: share/tests.mk
	$(MKDIR) -p $(R_HOME)/share/make
	$(CP) -p $^ $(R_HOME)/share/make/wintests.mk
	@$(ECHO) done > fixshare	
	
fixparl: ../../../share/perl/scripts.par
	@$(ECHO) done > fixparl

svnonly:
	@$(MAKE) -C ../../../doc/manual -f Makefile.win svnonly

clean:
	$(RM) *~ */*~ fixhtml fixbin fixetc fixshare

distclean:

ifeq ($(strip $(USE_PARL)),YES)
PERL_TOOLS=
else
PERL_TOOLS=../tools/
endif

$(R_HOME)/doc/html/search/SearchEngine.html: fixparl $(R_HOME)/doc/html/search/SearchEngine-head.html $(R_HOME)/doc/html/search/SearchEngine-foot.html $(R_HOME)/doc/KEYWORDS.db
	(cd $(R_HOME)/doc; \
	cat html/search/SearchEngine-head.html > html/search/SearchEngine.html; \
	$(PERL) $(PERL_TOOLS)keywords2html.pl KEYWORDS.db >> html/search/SearchEngine.html; \
	cat html/search/SearchEngine-foot.html >> html/search/SearchEngine.html)
../XINSTALL: bin/INSTALL
ifeq ($(strip $(BUILD)),CROSS)
	@$(SED) -e 's/"MINGW"/"CROSS"/' $< > $@
endif

FIXED_SCRIPTS=bin/INSTALL bin/REMOVE bin/Rd2txt bin/SHLIB

OTHER_SCRIPTS=rwver.pl ../../../share/perl/build-help.pl \
	../../../share/perl/build-help-windows.pl \
	../../../share/perl/massage-Examples.pl \
	../../../tools/pkg2tex.pl ../../../tools/Rdnewer.pl \
	../../../tools/keywords2html.pl \
	../installer/JRins.pl ../installer/WIXins.pl \
	../makeDllRes.pl ../pwd.pl \
	../../../bin/Rdconv ../../../bin/Rprof ../../../bin/Sd2Rd \
	../../../bin/build ../../../bin/check ../../../bin/massage-Examples 

# This target updates an existing scripts.par.  To create a new one, install
# the PAR::packer package in Perl, and then run the last line instead of the others

../../../share/perl/scripts.par: fixbin  $(OTHER_SCRIPTS)
	@$(MKDIR) -p script
	cp $(FIXED_SCRIPTS) $(OTHER_SCRIPTS) script
	zip -q -r $@ script
	@$(RM) -rf script
#	PERL5LIB="../../../share/perl" pp.bat -p -o $@ $(FIXED_SCRIPTS) $(OTHER_SCRIPTS)
