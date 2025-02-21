#-*- Makefile -*-
#
# ${R_HOME}/tests/Makefile.install

## Keep in step with Makefile.install

srcdir = .
top_srcdir = ..

top_builddir = ..

all check: test-all-basics
check-devel: check test-all-devel
check-all: check-devel test-Recommended

include $(top_builddir)/etc$(R_ARCH)/Makeconf
include Makefile.common


## GNU gettext disables LANGUAGE if LC_ALL=C, but other systems might not.
R  = $(R_HOME)/$(BINDIR)/Rterm --vanilla LANGUAGE=en LC_ALL=C SRCDIR=. R_DEFAULT_PACKAGES=
R2 = $(R_HOME)/$(BINDIR)/Rterm --vanilla SRCDIR=. R_DEFAULT_PACKAGES=
RCHK  = $(R_HOME)/$(BINDIR)/Rcmd check
RDIFF = $(R_HOME)/$(BINDIR)/R CMD Rdiff
RVAL_IF_DIFF=0
## suppress fancy quotes for diff-ing
RDCONV = LC_ALL=C $(R_HOME)/$(BINDIR)/Rcmd Rdconv
MK = $(MAKE)

all-basic-tests = BasePackages Specific Reg Internet
## Keep in line with Makefile.common.
## not Docs Standalone Packages
all-devel-tests = DateTime IsAs Random Demo Primitive Regexp Translation \
    Internet2 CRANtools Segfault Conditions Misc-dev


## <NOTE>
## These depend on an internet connection, and the sites being up.
## So allow this to fail: it may be slow doing so.
test-Internet:
	@$(ECHO) "running tests of Internet functions"
	-@$(MAKE) $(test-out-internet) RVAL_IF_DIFF=0
## </NOTE>

test-BasePackages:
	@$(MKINSTALLDIRS) Packages
	@$(ECHO) "tools::testInstalledPackages(outDir='Packages', scope='base')" | $(R) --no-echo

test-Recommended:
	@$(MKINSTALLDIRS) Packages
	@$(ECHO) "tools::testInstalledPackages(outDir='Packages', scope='recommended')" | $(R) --no-echo


mostlyclean: clean
clean:
	-@rm -f reg-*.pdf p-qbeta-strict-tst.pdf ex.gz
	-@rm -Rf myTst* myLib myLib_2
	-@rm -f p-r-random-tests_seed
	-@rm -f Rplots* Rprof.out Rprofmem.out data dumpdata.R mirrors.html \
	  .Rd l0.Rd m0.Rd df0.Rd ./*-package.Rd .RData

distclean: clean
	-@rm -f $(test-out) $(test-src-auto) *.Rout.fail
	-@rm -Rf Packages

maintainer-clean: distclean
