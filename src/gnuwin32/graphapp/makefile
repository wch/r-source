include ../MkRules

CFLAGS=-I. -Wall -O2 -DINDLL
SOURCES=$(filter-out graphappmain.c, $(wildcard *.c))
OBJS=$(foreach i, $(SOURCES), $(basename $i).o)

../ga.a: $(OBJS)
# workaround line-limit problems on djtools make
	$(AR) cr $@ *.o
	$(RANLIB) $@ 

#../graphapp.o: graphapp.o
#	$(CP) graphapp.o ../graphapp.o


#graphapp.o: $(filter-out graphappmain.c, $(wildcard *.c)) graphapp.h internal.h
#	$(CC) $(CFLAGS) -c graphapp.c


clean:
	$(RM) *.o *.a *~ \#*\#
	$(MAKE) -C gifs clean
