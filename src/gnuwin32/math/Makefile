# library of inverse hyperbolic functions

include ../MkRules
CFLAGS=-O2

OBJS=acosh.o asinh.o atanh.o const.o polevl.o

../em.a: $(OBJS)

clean:
	$(RM) *.o *.a *~ \#*\#

acosh.o: mconf.h
asinh.o: mconf.h
atanh.o: mconf.h
const.o: mconf.h
polevl.o: mconf.h

