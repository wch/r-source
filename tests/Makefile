## Makefile  for  <RHOME>/tests/		(C) Martin Maechler
##
R = ../bin/R --no-init-file --no-save --no-restore

TESTsrc = simple-true.R ../demos/language/is-things.R simple-tests.R\
	 print-tests.R

TESTout = $(TESTsrc:.R=.Rout)

%.Rout: %.R
	$(R) < $< > $@
	-@wc $@

tests: $(TESTout)
