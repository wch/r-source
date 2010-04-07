include ../MkRules

R_HOME = ../../..

all: profiles fixhtml fixbin fixetc fixdoc svnonly

profiles: $(R_HOME)/library/base/R/Rprofile

$(R_HOME)/library/base/R/Rprofile:  ../../library/profile/Common.R \
../../library/profile/Rprofile.windows
	@$(ECHO) -------- Building $@ from $^--------
	mkdir -p $(R_HOME)/library/base/R
	@$(CAT)  $^ > $@

fixbin:  cp2bin # iconv
	@$(ECHO) done > fixbin

## unused
iconv:
	@$(CP) ../unicode/iconv.dll ../../../$(BINDIR)

cp2bin: $(wildcard ./bin/*)
	@zip -q bins $^
	@unzip -oaqj bins -d  $(R_HOME)/bin
	@$(RM) bins.zip

fixhtml:  html/rwin.html
	$(CP) -p html/rwin.html $(R_HOME)/doc/html/index.html
	@$(ECHO) done > fixhtml

fixetc: $(wildcard ./etc/*)
	$(CP) -p $^ $(R_HOME)/etc
ifeq ($(strip $(WIN)),64)
	$(SED) -e 's/WIN = 32/WIN = 64/' \
	  -e "s/-O3/-O2/" \
	  -e "s/BINPREF =/BINPREF = $(BINPREF)/" \
	  -e "s|IMPDIR = bin|IMPDIR = $(IMPDIR)|" \
	  -e "s|LOCAL_SOFT =|LOCAL_SOFT = $(LOCAL_SOFT)|" \
	  -e "s|R_ARCH =|R_ARCH = $(R_ARCH)|" \
	  etc/Makeconf > $(R_HOME)/etc/Makeconf
	$(MKDIR) -p $(R_HOME)/etc/x64
	$(CP) -p $(R_HOME)/etc/Makeconf $(R_HOME)/etc/x64/Makeconf
	$(RM) $(R_HOME)/etc/Makeconf
else
	$(SED) -e "s|IMPDIR = bin|IMPDIR = $(IMPDIR)|" \
	  -e "s|LOCAL_SOFT =|LOCAL_SOFT = $(LOCAL_SOFT)|" \
	  etc/Makeconf > $(R_HOME)/etc/Makeconf
endif
	@$(ECHO) done > fixetc

fixdoc: $(wildcard ../CHANGES*)
	$(CP) -p $^ $(R_HOME)/doc
	@$(ECHO) done > fixdoc

svnonly:
	@$(MAKE) -C ../../../doc/manual -f Makefile.win svnonly

clean:
	$(RM) *~ */*~ fixhtml fixbin fixetc fixshare fixdoc

distclean:
	$(RM) $(R_HOME)/doc/CHANGES*
