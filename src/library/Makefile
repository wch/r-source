all:
	@cd base; make
	@cd profile; make

libs:
	@echo Building application libraries..
	@sh LIBSETUP

clean:
	@echo Cleaning in library..
	@sh LIBCLEAN
	@rm LIBCLEAN
