top_builddir = ../..

include $(top_builddir)/etc/Makeconf

R=$(top_builddir)/bin/R 

check: check.S Tests$(SHLIB_EXT)
	$(R) --vanilla < check.S

Tests$(SHLIB_EXT): Tests.c
	$(R) CMD SHLIB -o $@ $^

clean:
	rm Tests.o Tests$(SHLIB_EXT)