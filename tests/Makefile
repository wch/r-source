## Makefile  for  <RHOME>/tests/		-- Martin Maechler
##
RHOME= ..
R    = $(RHOME)/bin/R --no-init-file --no-save --no-restore
Rbinary = $(rRHOME)/bin/R.binary
Rbase	= $(rRHOME)/library/base
REXE = $(Rbinary) $(Rbase)

Ex = ./Examples

TESTsrc = simple-true.R simple-tests.R d-p-q-r-tests.R \
	 print-tests.R is-things.R

TESTout = $(TESTsrc:.R=.Rout)

%.Rout: %.R stamp-Rexe
	$(R) < $< > $@
	-@wc $@

test-All: test-Examples $(TESTout)

test-Examples:
	-(cd $(Ex); $(MAKE) $@)

stamp-Rexe: $(REXE)
	@touch $@

$(REXE)::
	-(cd $(RHOME); $(MAKE) R)

clean:
	rm -f stamp-Rexe
	-(cd $(Ex); $(MAKE) $@)
realclean: clean
	rm -f $(TESTout)
	-(cd $(Ex); $(MAKE) $@)


