include ../Makeconf

install:
	$(INSTALL) -d $(rhome)/demos/dynload 
	$(INSTALL) -d $(rhome)/demos/graphics 
	$(INSTALL) -d $(rhome)/demos/language 
	$(INSTALL) -d $(rhome)/demos/models 
	$(INSTALL) -d $(rhome)/demos/nlm
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

