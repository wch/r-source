LIBNAME=mva
LD=ld

OBJS=	dblcen.o \
	dist.o \
	hclust.o

eda.so:	$(OBJS)
	@$(LD) $(SHLIBLDFLAGS) -o $(LIBNAME).so $(OBJS)

clean:
	@rm -f *.o *.so

realclean:
	@rm -f Makefile *.o *.so
