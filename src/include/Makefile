toLink = S.h S_compat.h f2c.h Fortran.h Blas.h Linpack.h Platform.h

install: rmlinks
	@echo Making include links
	@ln $(toLink) ../../include

rmlinks:
	@echo Removing links in src/include
	@for f in $(toLink); do rm -f ../../include/$${f}; done

clean:
	@echo Cleaning in src/include

realclean: clean rmlinks
