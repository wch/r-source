include ../MkRules

JPEGDIR=jpeg-6b
PNGDIR=libpng

all:
	@$(MAKE) --no-print-directory buildzlib buildpng buildjpeg rbitmap.o 
	@$(MAKE) --no-print-directory installdll

buildzlib:
	@$(MAKE) -C ../../extra/zlib -f Makefile.win

buildpng:
	@$(MAKE) CC='$(CC)' RANLIB=$(RANLIB) ZLIBLIB=../../../extra/zlib ZLIBINC=../../../extra/zlib -C $(PNGDIR) -f scripts/makefile.std libpng.a

# jconfig.h and Makefile obtained running 'CC="gcc -mno-cygwin" ./configure'
# under CYGWIN. They are here since the configure script doesn't run 
# (I suspect) using the reduced set of tools we distribute.
buildjpeg: $(JPEGDIR)/jconfig.h Makefile.jpeg
	$(MAKE) CC='$(CC)' AR='$(AR) rc' RANLIB=$(RANLIB) -C $(JPEGDIR) -f ../Makefile.jpeg libjpeg.a

$(JPEGDIR)/jconfig.h: jconfig.h 
	$(CP) jconfig.h $(JPEGDIR)/jconfig.h

rbitmap-CFLAGS=-Wall -pedantic -DHAVE_PNG -DHAVE_JPEG -I../../extra/zlib -I./libpng -I$(JPEGDIR) -I../../include
Rbitmap-DLLLIBS=-L../ -lR
Rbitmap-DLLFLAGS=-s

Rbitmap.dll: rbitmap.o $(PNGDIR)/libpng.a $(JPEGDIR)/libjpeg.a

installdll: Rbitmap.dll
	$(CP) Rbitmap.dll ../../../bin

clean:
	$(RM) *.o *.a *.dll *.def *~ \#*\#
	-$(MAKE) CC='$(CC)' -C $(PNGDIR) -f scripts/makefile.std clean
	-$(MAKE) CC='$(CC)' -C $(JPEGDIR) -f ../Makefile.jpeg clean




