include ../Makeconf

# Everything except CVS and Makefile
INSTALLFILES = A* B* Cour* H* [N-Z]*

install:
	$(INSTALL) -d $(rhome)/afm
	for f in $(INSTALLFILES); \
		do $(INSTALL_DATA) $$f $(rhome)/afm; \
	done
