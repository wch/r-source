R:		build
		@cd main; $(MAKE) install
		@cd library; $(MAKE)
		@cd library; $(MAKE) libs
		@cd front; $(MAKE)

build:
		@echo "Extracting foreign function symbols"
		@sh GETSYMBOLS
		cd graphics; $(MAKE)
		cd nmath; $(MAKE)
		cd appl; $(MAKE)
		cd unix; $(MAKE)
		cd main; $(MAKE)
		cd include; $(MAKE)

install:
		@echo "Sorry, this hasn't been implemented yet"

clean:
		@cd graphics; $(MAKE) $@
		@cd library; $(MAKE) $@
		@cd main; $(MAKE) $@
		@cd nmath; $(MAKE) $@
		@cd appl; $(MAKE) $@
		@cd unix; $(MAKE) $@
		@-rm -f lib/*.a

realclean:
		@cd front; $(MAKE) $@
		@cd graphics; $(MAKE) $@
		@cd library; $(MAKE) clean
		@cd main; $(MAKE) $@
		@cd nmath; $(MAKE) $@
		@cd appl; $(MAKE) $@
		@cd unix; $(MAKE) $@
		@cd include; $(MAKE) $@
		@-rm -f include/FFDecl.h include/FFTab.h include/Platform.h
		@-rm -f lib/*.a
