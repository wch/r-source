build:
		@echo extracting foreign function symbols
		@sh GETSYMBOLS
		cd graphics; $(MAKE)
		cd math; $(MAKE)
		cd appl; $(MAKE)
		cd unix; $(MAKE)
		cd main; $(MAKE)
		cd include; $(MAKE)

install:	build
		@cd main; $(MAKE) install
		@cd library; $(MAKE)
		@cd library; $(MAKE) libs
		@cd front; $(MAKE)

clean:
		@cd graphics; $(MAKE) $@
		@cd library; $(MAKE) $@
		@cd main; $(MAKE) $@
		@cd math; $(MAKE) $@
		@cd appl; $(MAKE) $@
		@cd unix; $(MAKE) $@
		@-rm -f lib/*.a

realclean:
		@cd front; $(MAKE) $@
		@cd graphics; $(MAKE) $@
		@cd library; $(MAKE) clean
		@cd main; $(MAKE) $@
		@cd math; $(MAKE) $@
		@cd appl; $(MAKE) $@
		@cd unix; $(MAKE) $@
		@cd include; $(MAKE) $@
		@-rm -f include/FFDecl.h include/FFTab.h include/Platform.h
		@-rm -f lib/*.a
