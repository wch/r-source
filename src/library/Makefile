all:
	@cd base; $(MAKE)
	@cd profile; $(MAKE)

libs:
	@echo Building application libraries..
	@sh LIBSETUP

clean:
	@echo Cleaning in library..
	@sh LIBCLEAN
realclean: clean
	@rm LIBCLEAN
