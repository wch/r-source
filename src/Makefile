build:
		@echo extracting foreign function symbols
		@sh GETSYMBOLS
		cd graphics; make
		cd math; make
		cd appl; make
		cd unix; make
		cd main; make
		cd include; make

install:	build
		@cd main; make install
		@cd library; make
		@cd library; make libs
		@cd front; make

clean:
		@cd graphics; make $@
		@cd library; make $@
		@cd main; make $@
		@cd math; make $@
		@cd appl; make $@
		@cd unix; make $@
		@-rm -f lib/*.a

realclean:
		@cd front; make $@
		@cd graphics; make $@
		@cd library; make clean
		@cd main; make $@
		@cd math; make $@
		@cd appl; make $@
		@cd unix; make $@
		@cd include; make $@
		@-rm -f include/FFDecl.h include/FFTab.h include/Platform.h
		@-rm -f lib/*.a
