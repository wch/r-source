include ../Makeconf

install:
	$(INSTALL) -d $(rhome)/doc/manual 
	$(INSTALL) -d $(rhome)/doc/html/search
	$(INSTALL) keyword-test $(rhome)/doc
	$(INSTALL_DATA) KEYWORDS $(rhome)/doc
	$(INSTALL_DATA) html/*.html html/*.jpg $(rhome)/doc/html
	$(INSTALL_DATA) html/search/*.class \
			html/search/*.html \
			html/search/index.txt $(rhome)/doc/html/search
	cd manual; $(MAKE) install

