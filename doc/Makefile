include ../Makeconf

install:
	$(INSTALL_DIR) $(rhome)/doc/manual 
	$(INSTALL_DIR) $(rhome)/doc/html/search
	$(INSTALL) keyword-test $(rhome)/doc
	$(INSTALL_DATA) KEYWORDS $(rhome)/doc
	for f in html/*.html html/*.jpg; \
		do $(INSTALL_DATA) $$f $(rhome)/doc/html; \
	done
	for f in  html/search/*.class html/search/*.html \
		html/search/index.txt ; \
		do $(INSTALL_DATA) $$f $(rhome)/doc/html/search; \
	done
	cd manual; $(MAKE) install

