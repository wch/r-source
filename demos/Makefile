include ../Makeconf

install:
	$(INSTALL) -d $(rhome)/demos/dynload 
	$(INSTALL) -d $(rhome)/demos/graphics 
	$(INSTALL) -d $(rhome)/demos/language 
	$(INSTALL) -d $(rhome)/demos/models 
	$(INSTALL) -d $(rhome)/demos/nlm
	$(INSTALL_DATA) dynload/Makefile dynload/README dynload/zero* \
			$(rhome)/demos/dynload
	$(INSTALL_DATA) graphics/*.R $(rhome)/demos/graphics
	$(INSTALL_DATA) language/*.R $(rhome)/demos/language
	$(INSTALL_DATA) models/*.R $(rhome)/demos/models
	$(INSTALL_DATA) nlm/*.R $(rhome)/demos/nlm
