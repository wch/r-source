include ../Makeconf

# Everything except CVS and Makefile
INSTALLFILES = A* B* Cour* H* [N-Z]*

install:
	$(INSTALL_DIR) $(rhome)/afm
	for f in $(INSTALLFILES); \
		do $(INSTALL_DATA) $$f $(rhome)/afm; \
	done
