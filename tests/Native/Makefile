include $(R_HOME)/etc/Makeconf

R=$(R_HOME)/bin/R 

check: check.S Tests$(SHLIB_EXT)
	$(R) --vanilla < check.S

Tests$(SHLIB_EXT): Tests.c
	R CMD SHLIB -o $@ $^

clean:
	rm Tests.o Tests$(SHLIB_EXT)