include ../MkRules

R_HOME = ../../..

all: profiles fixhtml fixbin fixetc svnonly

profiles: $(R_HOME)/library/base/R/Rprofile

$(R_HOME)/library/base/R/Rprofile:  ../../library/profile/Common.R \
../../library/profile/Rprofile.windows
	@$(ECHO) -------- Building $@ from $^--------
	mkdir -p $(R_HOME)/library/base/R
	@$(CAT)  $^ > $@

fixbin:  cp2bin # iconv
	@$(ECHO) done > fixbin

iconv:
	@$(CP) ../unicode/iconv.dll ../../../bin

cp2bin: $(wildcard ./bin/*)
	@zip -q bins $^
	@unzip -oaq bins -d  $(R_HOME)
	@$(RM) bins.zip

fixhtml:  html/rwin.html
	$(CP) -p html/rwin.html $(R_HOME)/doc/html/index.html
	@$(ECHO) done > fixhtml

fixetc: $(wildcard ./etc/*)
	$(CP) -p $^ $(R_HOME)/etc
	@$(ECHO) done > fixetc

svnonly:
	@$(MAKE) -C ../../../doc/manual -f Makefile.win svnonly

clean:
	$(RM) *~ */*~ fixhtml fixbin fixetc fixshare

distclean:
