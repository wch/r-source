include ../Makeconf

# Everything except CVS and Makefile
INSTALLFILES = *.afm *.lt1

install:
	$(INSTALL_DIR) $(rhome)/afm
	for f in $(INSTALLFILES); \
		do $(INSTALL_DATA) $$f $(rhome)/afm; \
	done
