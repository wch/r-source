LIBNAME=mva
LD=ld

OBJS=	dblcen.o \
	dist.o \
	hclust.o

$(LIBNAME).so:	$(OBJS)
	$(LD) $(SHLIBLDFLAGS) -o $@ $(OBJS)

clean:
	@rm -f *.o *.so

realclean: clean

