LIBNAME=eda
LD=ld

OBJS=	line.o \
	smooth.o

$(LIBNAME).so:	$(OBJS)
	$(LD) $(SHLIBLDFLAGS) -o $@ $(OBJS)

clean:
	@rm -f *.o *.so

realclean: clean
