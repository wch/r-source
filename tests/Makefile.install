#-*- Makefile -*-
#
# ${R_HOME}/tests/Makefile.install

## Keep in step with Makefile.install.win

srcdir = .
top_srcdir = ..

top_builddir = ..

all check: test-all-basics
check-devel: check test-all-devel
check-all: check-devel test-Recommended

include $(top_builddir)/etc$(R_ARCH)/Makeconf
include Makefile.common


## GNU gettext disables LANGUAGE if LC_ALL=C, but other systems might not.
R = LANGUAGE=en LC_ALL=C SRCDIR=$(srcdir) R_DEFAULT_PACKAGES= $(top_builddir)/bin/R --vanilla
## grDevices should be run in an UTF-8 locale
R0 = LANGUAGE=en SRCDIR=$(srcdir) R_DEFAULT_PACKAGES= $(top_builddir)/bin/R --vanilla
R2 = R_DEFAULT_PACKAGES= $(top_builddir)/bin/R --vanilla
RCHK  = $(top_builddir)/bin/R CMD check
RDIFF = $(top_builddir)/bin/R CMD Rdiff
RDCONV = LC_CTYPE=C $(top_builddir)/bin/R CMD Rdconv
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
	@$(ECHO) "tools::testInstalledPackages(outDir='Packages', scope='base')" | $(R0) --no-echo

test-Recommended:
	@$(MKINSTALLDIRS) Packages
	@$(ECHO) "tools::testInstalledPackages(outDir='Packages', scope='recommended')" | $(R0) --no-echo


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
