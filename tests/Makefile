## Makefile  for  <RHOME>/tests/		(C) Martin Maechler
##
RHOME= ..
REXE = $(RHOME)/bin/R.binary
R    = $(RHOME)/bin/R --no-init-file --no-save --no-restore

Ex = ./Examples

TESTsrc = simple-true.R simple-tests.R d-p-q-r-tests.R \
	 print-tests.R is-things.R

TESTout = $(TESTsrc:.R=.Rout)

%.Rout: %.R $(REXE)
	$(R) < $< > $@
	-@wc $@

tests: test-Examples $(TESTout)

test-Examples:
	-(cd $(Ex); $(MAKE) $@)

$(REXE)::
	-(cd $(RHOME); $(MAKE) R)

realclean clean:
	rm -f $(TESTout)
	-(cd $(Ex); $(MAKE) $@)


