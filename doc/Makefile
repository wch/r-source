include ../Makeconf

install:
	$(INSTALL) -d $(rhome)/doc/manual $(rhome)/doc/html/search
	$(INSTALL_DATA) KEYWORDS $(rhome)/doc
	$(INSTALL_DATA) html/*.html html/*.jpg $(rhome)/doc/html
	$(INSTALL_DATA) html/search/*.class \
			html/search/*.html \
			html/search/index.txt $(rhome)/doc/html/search
	cd manual; $(MAKE) install

