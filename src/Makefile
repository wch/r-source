build:
		@echo extracting foreign function symbols
		@sh GETSYMBOLS
		cd graphics; make
		cd math; make
		cd appl; make
		cd regex; make
		cd unix; make
		cd main; make

install:	build
		@cd main; make install
		@cd library; make
		@cd library; make libs
		@cd front; make

clean:
		@cd graphics; make clean
		@cd library; make clean
		@cd main; make clean
		@cd math; make clean
		@cd appl; make clean
		@cd regex; make clean
		@cd unix; make clean
		@-rm -f lib/*.a

realclean:
		@cd front; make realclean
		@cd graphics; make realclean
		@cd library; make clean
		@cd main; make realclean
		@cd math; make realclean
		@cd appl; make realclean
		@cd regex; make realclean
		@cd unix; make realclean
		@-rm -f include/ForeignDecl.h include/ForeignTab.h include/Platform.h
		@-rm -f lib/*.a
