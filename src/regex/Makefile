include ../../Makeconf

CFLAGS := $(CFLAGS) -DPOSIX_MISTAKE

SRCS=	regcomp.c regerror.c regexec.c regfree.c

OBJ=	\
	regcomp.o \
	regerror.o \
	regexec.o \
	regfree.o

LIB=	../lib/libregex.a

$(LIB):	$(OBJ)
	$(AR) cr $(LIB) $(OBJ)
	$(RANLIB) $(LIB)

clean:
	@echo Cleaning in src/regex
	@-rm -f *.o *.a

realclean: clean
