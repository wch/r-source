include ../MkRules

ifdef DEBUG
  DEBUGFLAG=-g
else
  DEBUGFLAG=
endif

CFLAGS=-I. -I.. -Wall $(DEBUGFLAG)  -O2 -DINDLL
SOURCES=$(filter-out graphappmain.c, $(wildcard *.c))
OBJS=$(foreach i, $(SOURCES), $(basename $i).o)

# Uncomment next line to get substitute of malloc and friends
winalloc-CFLAGS=-DDANGER=1

../ga.a: $(OBJS)
# uncomment to work around line-limit problems on djtools make
#	$(AR) cr $@ *.o
#	$(RANLIB) $@ 

# to allow changing USE_NATIVE*
buttons.o : internal.h

clean:
	$(RM) *.o *.a *~ \#*\#
	$(MAKE) -C gifs clean
