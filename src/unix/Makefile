include ../../Makeconf

INCLUDES= -I../include

OBJ= \
	dataentry.o \
	devX11.o \
	devPS.o \
	dynload.o \
	edit.o \
	fileio.o \
	PostScript.o \
	rotated.o \
	system.o \
	devPicTeX.o

LIB=		../lib/libunix.a

$(LIB):		$(OBJ)
		$(AR) cr $(LIB) $(OBJ)
		$(RANLIB) $(LIB)

dataentry.o:	dataentry.h dataentry.c ../include/Defn.h
		$(CC) $(CFLAGS) $(X_CFLAGS) $(INCLUDES) -c dataentry.c

fileio.o:	fileio.c ../include/Fileio.h
		$(CC) $(CFLAGS) $(INCLUDES) -c fileio.c

rotated.o:	rotated.c rotated.h
		$(CC) $(CFLAGS) $(X_CFLAGS) $(INCLUDES) -c rotated.c

devX11.o:	devX11.c psx11.h ../include/Graphics.h ../include/Defn.h
		$(CC) $(CFLAGS) $(X_CFLAGS) $(INCLUDES) -c devX11.c

devPS.o:	devPS.c ../include/Graphics.h ../include/Defn.h
		$(CC) $(CFLAGS) $(INCLUDES) -c devPS.c

devPicTeX.o:	devPicTeX.c ../include/Graphics.h ../include/Defn.h
		$(CC) $(CFLAGS) $(INCLUDES) -c devPicTeX.c

system.o:	system.c ../include/Defn.h
		$(CC) $(CFLAGS) $(INCLUDES) -c system.c

clean:
		@echo Cleaning in src/unix
		@-rm -f *.o *.a

realclean:	clean
