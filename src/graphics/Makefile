include ../../Makeconf

INCLUDES= -I ../include

OBJS=	graphics.o

LIB=	../lib/libgraphics.a

$(LIB):	$(OBJS)
	$(AR) cr $(LIB) $(OBJS)
	$(RANLIB) $(LIB)

$(OBJS):	../include/Graphics.h
gdevice.o:	../include/Platform.h

clean:
		@echo Cleaning in src/graphics
		@-rm -f *.o *.a

realclean:	clean
