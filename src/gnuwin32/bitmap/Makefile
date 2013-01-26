## These can be overridden in MkRules.dist or MkRules.local

## must be jpeg-7 jpeg-8 jpeg-8a jpeg-8b jpeg-8c jpeg-8d jpeg-9 libjpeg-turbo
JPEGDIR = jpeg-9
PNGDIR = libpng
TIFFDIR = libtiff

include ../MkRules

.PHONY: all DLL buildzlib buildpng buildjpeg buildtiff clean

all:
	@$(MAKE) --no-print-directory buildzlib buildpng buildjpeg buildtiff rbitmap.o
	@$(MKDIR) -p ../../../library/grDevices/libs
	@$(MAKE) --no-print-directory DLL

DLL: ../../../library/grDevices/libs$(R_ARCH)/Rbitmap.dll

buildzlib:
	@$(MAKE) -C ../../extra/zlib -f Makefile.win

## There is a makefile.mingw in 1.2.x and 1.4.x but not 1.5.x
## Neither the makefile.{mingw,std} build pnggccrd.c but it is needed if
## the compiler defines __MMX__ , which the win64 gcc does.
## So we need to explicitly disable it.
## prefix=foo shuts up a warning
buildpng:
	$(MAKE) CC='$(CC)' AR='$(AR)' AR_RC='$(AR) rcs' \
	  CFLAGS="-O3 -I../../../extra/zlib -DPNG_NO_MMX_CODE" \
	  RANLIB=$(RANLIB) ZLIBLIB=../../../extra/zlib -C $(PNGDIR) \
	  -f scripts/makefile.gcc prefix=foo libpng.a

buildjpeg: $(JPEGDIR)/jconfig.h Makefile.jpeg
	$(MAKE) CC='$(CC)' AR='$(AR) rc' RANLIB=$(RANLIB) JPEGDIR=$(JPEGDIR) -C $(JPEGDIR) \
	  -f ../Makefile.jpeg libjpeg.a

$(JPEGDIR)/jconfig.h: jconfig.h 
	$(CP) jconfig.h $(JPEGDIR)/jconfig.h

## This gives a lot of warnings on Win64, but seems to work
buildtiff: $(JPEGDIR)/jconfig.h Makefile.tiff
	$(MAKE) CC='$(CC)' AR='$(AR)' RANLIB=$(RANLIB) -C $(TIFFDIR) -f ../Makefile.tiff JPEGDIR=$(JPEGDIR)

rbitmap-CPPFLAGS=-DHAVE_PNG -DHAVE_JPEG -DHAVE_TIFF -I. -I../../extra/zlib -I./$(PNGDIR) -I$(JPEGDIR) -I./$(TIFFDIR) -I../../include
CFLAGS=-O3 -Wall -pedantic 

../../../library/grDevices/libs$(R_ARCH)/Rbitmap.dll: rbitmap.o $(TIFFDIR)/libtiff.a $(PNGDIR)/libpng.a $(JPEGDIR)/libjpeg.a
	$(DLL) -shared -s -o $@ $^ -L../../../$(IMPDIR) -lR -lRzlib

clean:
	$(RM) *.o *.a *.dll *.def *~ \#*\#
	-$(MAKE) CC='$(CC)' -C $(PNGDIR) -f scripts/makefile.std clean
	-$(MAKE) CC='$(CC)' -C $(JPEGDIR) -f ../Makefile.jpeg clean
	-$(MAKE) CC='$(CC)' -C $(TIFFDIR) -f ../Makefile.tiff clean



