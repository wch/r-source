# -*- Makefile -*-
#
# ${R_HOME}/share/make/tests.mk

ECHO_C =
ECHO_N = -n
ECHO_T =

makevars =
srcdir = .

test-src = $(test-src-1) $(test-src-auto)
test-out = $(test-src:.R=.Rout)

R = srcdir=$(srcdir) $(R_HOME)/bin/Rterm.exe --vanilla
RDIFF = $(R_HOME)/bin/Rcmd Rdiff.sh
USE_GCT = 0

.SUFFIXES:
.SUFFIXES: .R .Rin .Rout

.Rin.R:
	@echo "Creating \`$@'"
	@$(R) < $< > /dev/null

.R.Rout:
	@rm -f $@ $@.fail
	@echo "  Running \`$<'"
	@if test "$(USE_GCT)" = 0; then \
	  $(R) R_LIBS="$(R_LIBS)" < $< > $@; \
	else \
	  (echo "gctorture(TRUE)"; cat $<) | $(R) R_LIBS="$(R_LIBS)" > $@; \
	fi
	@if test -f $(srcdir)/$@.save; then \
	  mv $@ $@.fail; \
	  echo $(ECHO_N) "  Comparing \`$@' to \`$@.save' ...$(ECHO_C)"; \
	  $(RDIFF) $@.fail $(srcdir)/$@.save 0 || exit 1; \
	  mv $@.fail $@; \
	  echo "$(ECHO_T) OK"; \
	fi

all:
	@(out=`echo "$(test-out)" | sed 's/ $$//g'`; \
	  if test -n "$${out}"; then \
	    $(MAKE) -f $(R_HOME)/share/make/tests.mk $(makevars) $${out}; \
	  fi)

clean:
	@rm -f $(test-out) $(test-src-auto) *.fail
