include ../Makeconf

install:
	$(INSTALL) -d $(rhome)/demos/dynload \
		      $(rhome)/demos/graphics \
		      $(rhome)/demos/language \
		      $(rhome)/demos/models \
		      $(rhome)/demos/nlm
	$(INSTALL_DATA) dynload/Makefile dynload/README dynload/zero* \
			$(rhome)/demos/dynload
	$(INSTALL_DATA) graphics/*.R $(rhome)/demos/graphics
	$(INSTALL_DATA) language/*.R $(rhome)/demos/language
	$(INSTALL_DATA) models/*.R $(rhome)/demos/models
	$(INSTALL_DATA) nlm/*.R $(rhome)/demos/nlm
