all:
	@cd base; make
	@cd profile; make

libs:
	@echo Building application packages..
	@sh LIBSETUP

clean:
	@echo Cleaning in library..
	@sh LIBCLEAN
realclean: clean
	@rm LIBCLEAN
