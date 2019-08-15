include ../MkRules

R_HOME = ../../..

# FIXME:  Should find a make where $(abspath ...) works and avoid this...
LOCAL_SOFT := $(shell ../Rpwd.exe $(LOCAL_SOFT))

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

ETC = $(wildcard ./etc/*)
fixetc: $(ETC) ../MkRules Makefile
	$(CP) -p $(ETC) $(R_HOME)/etc
	$(RM) $(R_HOME)/etc/Makeconf
	$(MKDIR) -p $(R_HOME)/etc/$(R_ARCH)
ifeq "$(WIN)" "64"
	$(SED) -e 's/WIN = 32/WIN = 64/' \
	  -e "s/-O3/-O2/" \
	  -e "s/@EOPTS@/$(EOPTS)/" \
	  -e "s|BINPREF =|BINPREF ?= $(BINPREF)|" \
	  -e "s|COMPILED_BY =|COMPILED_BY = $(COMPILED_BY)|" \
	  -e "s|IMPDIR = bin|IMPDIR = $(IMPDIR)|" \
	  -e "s|LOCAL_SOFT ?=|LOCAL_SOFT ?= $(LOCAL_SOFT)|" \
	  -e "s|R_ARCH =|R_ARCH = $(R_ARCH)|" \
	  -e "s|DT_ARCH =|DT_ARCH = $(DT_ARCH)|" \
	  -e "s|RC_ARCH =|RC_ARCH = $(RC_ARCH)|" \
	  -e "s|M_ARCH =|M_ARCH = $(M_ARCH)|" \
	  -e "s|@SYMPAT@|$(SYMPAT)|" \
	  -e "s|(TCL_HOME)/bin|(TCL_HOME)/bin64|" \
	  -e "s|@OPENMP@|$(OPENMP)|" \
	  -e "s|@PTHREAD@|$(PTHREAD)|" \
	  -e "s@NM_FILTER =@NM_FILTER = $(NM_FILTER)@" \
	  -e "s|@GF7OPTS@|$(GF7OPTS)|" \
	  etc/Makeconf > $(R_HOME)/etc$(R_ARCH)/Makeconf
else
	$(SED) -e "s|IMPDIR = bin|IMPDIR = $(IMPDIR)|" \
	  -e "s/-O3/-O2/" \
	  -e "s/@EOPTS@/$(EOPTS)/" \
	  -e "s|BINPREF =|BINPREF ?= $(BINPREF)|" \
	  -e "s|COMPILED_BY =|COMPILED_BY = $(COMPILED_BY)|" \
	  -e "s|LOCAL_SOFT ?=|LOCAL_SOFT ?= $(LOCAL_SOFT)|" \
	  -e "s|R_ARCH =|R_ARCH = $(R_ARCH)|" \
	  -e "s|DT_ARCH =|DT_ARCH = $(DT_ARCH)|" \
	  -e "s|RC_ARCH =|RC_ARCH = $(RC_ARCH)|" \
	  -e "s|M_ARCH =|M_ARCH = $(M_ARCH)|" \
	  -e "s|@SYMPAT@|$(SYMPAT)|" \
	  -e "s|@OPENMP@|$(OPENMP)|" \
	  -e "s|@PTHREAD@|$(PTHREAD)|" \
	  -e "s@NM_FILTER =@NM_FILTER = $(NM_FILTER)@" \
	  -e "s|@GF7OPTS@|$(GF7OPTS)|" \
	  etc/Makeconf > $(R_HOME)/etc$(R_ARCH)/Makeconf
endif
	$(SED) -e "s+@BINDIR@+$(BINDIR)+" Makeconf > $(R_HOME)/Makeconf
	@$(ECHO) done > fixetc

fixdoc: $(wildcard ../CHANGES?)
	$(CP) -p $^ $(R_HOME)/doc
	@$(ECHO) done > fixdoc

svnonly:
	@$(MAKE) -C ../../../doc/manual -f Makefile.win svnonly

clean:
	$(RM) *~ */*~ fixhtml fixbin fixetc fixshare fixdoc

distclean:
	$(RM) $(R_HOME)/doc/CHANGES* $(R_HOME)/Makeconf
