## Makefile  for  <RHOME>/tests/		(C) Martin Maechler
##
R = ../bin/R --no-init-file --no-save --no-restore

TESTsrc = simple-true.R simple-tests.R d-p-q-r-tests.R \
	 print-tests.R\
	 ../demos/language/is-things.R

TESTout = $(TESTsrc:.R=.Rout)

%.Rout: %.R
	$(R) < $< > $@
	-@wc $@

tests: $(TESTout)

clean: ; rm -f $(TESTout)
