# The real work is being done in Makefile.2nd. This is just to ensure that
# Makeconf is built first
all R docs help html latex dvi test-All test-Examples install\
 clean realclean acclean distclean :: config.status Makeconf
	$(MAKE) -f Makefile.2nd $@
Makeconf config.status:: configure config.site date-stamp
	-./configure
