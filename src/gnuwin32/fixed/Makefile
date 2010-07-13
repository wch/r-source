include ../MkRules

R_HOME = ../../..

all: profiles fixhtml fixetc fixdoc svnonly

profiles: $(R_HOME)/library/base/R/Rprofile

$(R_HOME)/library/base/R/Rprofile:  ../../library/profile/Common.R \
../../library/profile/Rprofile.windows
	@$(ECHO) -------- Building $@ from $^--------
	mkdir -p $(R_HOME)/library/base/R
	@$(CAT)  $^ > $@

fixhtml:  html/rwin.html
	$(CP) -p html/rwin.html $(R_HOME)/doc/html/index.html
	@$(ECHO) done > fixhtml

fixetc: $(wildcard ./etc/*)
	$(CP) -p $^ $(R_HOME)/etc
	$(RM) $(R_HOME)/etc/Makeconf
	$(MKDIR) -p $(R_HOME)/etc/$(R_ARCH)
ifeq ($(strip $(WIN)),64)
	$(SED) -e 's/WIN = 32/WIN = 64/' \
	  -e "s/-O3/-O2/" \
	  -e "s/BINPREF =/BINPREF = $(BINPREF)/" \
	  -e "s|IMPDIR = bin|IMPDIR = $(IMPDIR)|" \
	  -e "s|LOCAL_SOFT =|LOCAL_SOFT = $(LOCAL_SOFT)|" \
	  -e "s|R_ARCH =|R_ARCH = $(R_ARCH)|" \
	  -e "s|@SYMPAT@|$(SYMPAT)|" \
	  etc/Makeconf > $(R_HOME)/etc$(R_ARCH)/Makeconf
else
	$(SED) -e "s|IMPDIR = bin|IMPDIR = $(IMPDIR)|" \
	  -e "s/BINPREF =/BINPREF = $(BINPREF)/" \
	  -e "s|LOCAL_SOFT =|LOCAL_SOFT = $(LOCAL_SOFT)|" \
	  -e "s|R_ARCH =|R_ARCH = $(R_ARCH)|" \
	  -e "s|@SYMPAT@|$(SYMPAT)|" \
	  etc/Makeconf > $(R_HOME)/etc$(R_ARCH)/Makeconf
endif
	@$(ECHO) done > fixetc

fixdoc: $(wildcard ../CHANGES?)
	$(CP) -p $^ $(R_HOME)/doc
	@$(ECHO) done > fixdoc

svnonly:
	@$(MAKE) -C ../../../doc/manual -f Makefile.win svnonly

clean:
	$(RM) *~ */*~ fixhtml fixbin fixetc fixshare fixdoc

distclean:
	$(RM) $(R_HOME)/doc/CHANGES*
