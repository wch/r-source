all:
	@cd base; $(MAKE)
	@cd profile; $(MAKE)

libs:
	@echo Building application packages..
	@sh LIBSETUP

clean:
	@echo Cleaning in src/library
	@sh LIBCLEAN

realclean: clean
