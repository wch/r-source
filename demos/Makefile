include ../Makeconf

install:
	$(INSTALL_DIR) $(rhome)/demos/dynload 
	$(INSTALL_DIR) $(rhome)/demos/graphics 
	$(INSTALL_DIR) $(rhome)/demos/language 
	$(INSTALL_DIR) $(rhome)/demos/models 
	$(INSTALL_DIR) $(rhome)/demos/nlm
	for f in dynload/Makefile dynload/README dynload/zero*; \
		do $(INSTALL_DATA)  $$f $(rhome)/demos/dynload; \
	done
	for f in graphics/*.R; \
		do $(INSTALL_DATA) $$f $(rhome)/demos/graphics; \
	done
	for f in language/*.R; \
		do $(INSTALL_DATA) $$f $(rhome)/demos/language; \
	done
	for f in models/*.R; \
		do $(INSTALL_DATA) $$f $(rhome)/demos/models; \
	done
	for f in nlm/*.R; \
		do $(INSTALL_DATA) $$f $(rhome)/demos/nlm; \
	done

