all:
	@cd base; make
	@cd profile; make

libs:
	@echo Building application packages..
	@sh LIBSETUP

clean:
	@echo Cleaning in src/library
	@sh LIBCLEAN

realclean: clean
