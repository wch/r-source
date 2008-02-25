include ../MkRules

JPEGDIR=jpeg-6b
PNGDIR=libpng
TIFFDIR=libtiff

.PHONY: all DLL buildzlib buildpng buildjpeg buildtiff clean

all:
	@$(MAKE) --no-print-directory buildzlib buildpng buildjpeg buildtiff rbitmap.o
	@$(MKDIR) -p ../../../library/grDevices/libs
	@$(MAKE) --no-print-directory DLL

DLL: ../../../library/grDevices/libs/Rbitmap.dll

buildzlib:
	@$(MAKE) -C ../../extra/zlib -f Makefile.win

## There is a makefile.mingw these days, but .std has always worked
## Neither the makefile.{mingw,std} build pnggccrd.c but it is needed if
## the compiler defines __MMX__ , which the win64 gcc does.
## So we need to explicitly disable it.
buildpng:
	@$(MAKE) CC='$(CC)' \
	  CFLAGS="-O3 -I../../../extra/zlib -DPNG_NO_MMX_CODE" \
	  RANLIB=$(RANLIB) ZLIBLIB=../../../extra/zlib -C $(PNGDIR) \
	  -f scripts/makefile.std libpng.a

# jconfig.h and Makefile obtained running 'CC="gcc -mno-cygwin" ./configure'
# under CYGWIN. They are here since the configure script doesn't run 
# (I suspect) using the reduced set of tools we distribute.
## 2006-01-15 checked under current tools
buildjpeg: $(JPEGDIR)/jconfig.h Makefile.jpeg
	$(MAKE) CC='$(CC)' AR='$(AR) rc' RANLIB=$(RANLIB) -C $(JPEGDIR) \
	  -f ../Makefile.jpeg libjpeg.a

$(JPEGDIR)/jconfig.h: jconfig.h 
	$(CP) jconfig.h $(JPEGDIR)/jconfig.h

buildtiff: $(JPEGDIR)/jconfig.h Makefile.tiff
	$(MAKE) -C $(TIFFDIR) -f ../Makefile.tiff

rbitmap-CPPFLAGS=-DHAVE_PNG -DHAVE_JPEG -DHAVE_TIFF -I../../extra/zlib -I./$(PNGDIR) -I$(JPEGDIR) -I./$(TIFFDIR) -I../../include
CFLAGS=-O3 -Wall -pedantic 

../../../library/grDevices/libs/Rbitmap.dll: rbitmap.o $(TIFFDIR)/libtiff.a $(PNGDIR)/libpng.a $(JPEGDIR)/libjpeg.a
	$(DLL) -shared -s -o $@ $^ -L../../../bin -lR -lRzlib

clean:
	$(RM) *.o *.a *.dll *.def *~ \#*\#
	-$(MAKE) CC='$(CC)' -C $(PNGDIR) -f scripts/makefile.std clean
	-$(MAKE) CC='$(CC)' -C $(JPEGDIR) -f ../Makefile.jpeg clean




