include ../MkRules

JPEGDIR=jpeg-6b

all: buildzlib buildpng buildjpeg Rbitmap.dll installdll

buildzlib: 
	$(MAKE) CC="$(CC)" -C zlib -f nt/Makefile.gcc libz.a

buildpng:
	$(MAKE) CC="$(CC)" -C libpng -f scripts/makefile.std libpng.a

# jconfig.h and Makefile obtained running 'CC="gcc -mno-cygwin" ./configure'
# under CYGWIN. They are here since the configure script doesn't run 
# (I suspect) using the reduced set of tools we distribuite.
buildjpeg: $(JPEGDIR)/jconfig.h Makefile.jpeg
	$(MAKE) CC="$(CC)" -C $(JPEGDIR) -f ../Makefile.jpeg libjpeg.a

$(JPEGDIR)/jconfig.h: jconfig.h 
	$(CP) jconfig.h $(JPEGDIR)/jconfig.h

rbitmap-CFLAGS=-Wall -pedantic -DHAVE_PNG -DHAVE_JPEG -I./zlib -I./libpng -I$(JPEGDIR) 
Rbitmap-DLLLIBS=-L../ -lR -L./libpng -lpng -L./zlib -lz -L$(JPEGDIR) -ljpeg
Rbitmap-DLLFLAGS=-s

Rbitmap.dll: rbitmap.o

installdll: 
	$(CP) Rbitmap.dll ../../../bin

clean:
	$(RM) *.o *.a *.dll *.def *~ \#*\#
	$(MAKE) CC="$(CC)" RM="$(RM)" -C zlib -f nt/Makefile.gcc clean
	$(MAKE) CC="$(CC)" -C libpng -f scripts/makefile.std clean
	$(MAKE) CC="$(CC)" -C $(JPEGDIR) -f ../Makefile.jpeg clean




