toLink = S.h S_compat.h f2c.h Fortran.h Blas.h Linpack.h

install: rmlinks
	@echo making include links
	@ln $(toLink) ../../include

rmlinks:
	@echo cleaning links in src/include
	@for f in $(toLink); do rm -f ../../include/$${f}; done

clean:
	@echo cleaning in src/include

realclean: rmlinks
	@echo realcleaning in src/include
