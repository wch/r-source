include ../MkRules

JPEGDIR=jpeg-6b

all: buildzlib buildpng buildjpeg Rbitmap.dll installdll

buildzlib:
#	$(MAKE) CC='$(CC)' AR='$(AR) rcs' -C zlib -f nt/Makefile.gcc libz.a
	$(MAKE) -C ../../extra/zlib -f Makefile.win

buildpng:
	$(MAKE) CC='$(CC)' RANLIB=$(RANLIB) ZLIBLIB=../../../extra/zlib ZLIBINC=../../../extra/zlib -C libpng -f scripts/makefile.std libpng.a

# jconfig.h and Makefile obtained running 'CC="gcc -mno-cygwin" ./configure'
# under CYGWIN. They are here since the configure script doesn't run 
# (I suspect) using the reduced set of tools we distribute.
buildjpeg: $(JPEGDIR)/jconfig.h Makefile.jpeg
	$(MAKE) CC='$(CC)' AR='$(AR) rc' RANLIB=$(RANLIB) -C $(JPEGDIR) -f ../Makefile.jpeg libjpeg.a

$(JPEGDIR)/jconfig.h: jconfig.h 
	$(CP) jconfig.h $(JPEGDIR)/jconfig.h

rbitmap-CFLAGS=-Wall -pedantic -DHAVE_PNG -DHAVE_JPEG -I../../extra/zlib -I./libpng -I$(JPEGDIR) -I../../include
Rbitmap-DLLLIBS=-L./libpng -lpng -L$(JPEGDIR) -ljpeg -L../ -lR
Rbitmap-DLLFLAGS=-s

Rbitmap.dll: rbitmap.o

installdll: 
	$(CP) Rbitmap.dll ../../../bin

clean:
	$(RM) *.o *.a *.dll *.def *~ \#*\#
	-$(MAKE) CC='$(CC)' -C libpng -f scripts/makefile.std clean
	-$(MAKE) CC='$(CC)' -C $(JPEGDIR) -f ../Makefile.jpeg clean




