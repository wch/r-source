# -*- Makefile -*-
#
# ${R_HOME}/share/make/wintests.mk

makevars =
srcdir = .

test-src = $(test-src-1) $(test-src-auto)
test-out = $(test-src:.R=.Rout)

R = srcdir=$(srcdir) LANGUAGE=C $(R_HOME)/bin/Rterm.exe --vanilla
RDIFF = $(R_HOME)/bin/Rcmd Rdiff.sh
USE_GCT = 0
R_OPTS =

.SUFFIXES:
.SUFFIXES: .R .Rin .Rout

.Rin.R:
	@echo "Creating '$@'"
	@$(R) < $< > /dev/null

.R.Rout:
	@rm -f $@ $@.fail
	@echo "  Running '$<'"
	@(if test "$(USE_GCT)" != 0; then echo "gctorture(TRUE)"; fi; \
	  cat $<) | $(R) R_LIBS="$(R_LIBS)" $(R_OPTS) > $@ 2>&1 || \
	  (mv $@ $@.fail && exit 1)
	@if test -f $(srcdir)/$@.save; then \
	  mv $@ $@.fail; \
	  echo -n "  Comparing '$@' to '$@.save' ..."; \
	  $(RDIFF) $@.fail $(srcdir)/$@.save 0 || exit 1; \
	  mv $@.fail $@; \
	  echo "OK"; \
	fi

all:
	@(out=`echo "$(test-out)" | sed 's/ $$//g'`; \
	  if test -n "$${out}"; then \
	    $(MAKE) -f $(R_HOME)/share/make/wintests.mk $(makevars) $${out}; \
	  fi)

clean:
	@rm -f $(test-out) $(test-src-auto) *.fail
