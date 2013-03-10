include ../MkRules
R_HOME = ../../..

CFLAGS=-O3 -Wall -pedantic
ifdef DEBUG
 CFLAGS+=$(G_FLAG)
 LINKFLAGS=
 DLLFLAGS=
else
 LINKFLAGS=-s
 DLLFLAGS=-s
endif
CPPFLAGS=-I../../include $(arch_DEFS)

ifeq "$(WIN)" "32"
## Increase stack size to 16Mb
LARGE_STACK=-Wl,--stack=0x1000000
## allow 32-bit builds to address over 2Gb
LARGE_ADDR=-Wl,--large-address-aware
else
## Increase stack size to 64Mb
LARGE_STACK=-Wl,--stack=0x4000000
endif

rcmdfn-CFLAGS=-DBINDIR='"$(BINDIR)"'

.PHONY : all makeMakedeps

all: makeMakedeps \
     $(R_HOME)/$(BINDIR)/Rgui.exe $(R_HOME)/$(BINDIR)/Rterm.exe \
     $(R_HOME)/$(BINDIR)/Rcmd.exe  $(R_HOME)/$(BINDIR)/RSetReg.exe  \
     $(R_HOME)/$(BINDIR)/R.exe\
     $(R_HOME)/$(BINDIR)/Rscript.exe $(R_HOME)/$(BINDIR)/open.exe \
     $(R_HOME)/bin/R.exe $(R_HOME)/bin/Rscript.exe 

$(R_HOME)/$(BINDIR)/Rgui.exe: Rgui.exe
	$(MKDIR) -p $(R_HOME)/$(BINDIR)
	$(CP) $^ $@

$(R_HOME)/$(BINDIR)/Rterm.exe: Rterm.exe
	$(MKDIR) -p $(R_HOME)/$(BINDIR)
	$(CP) $^ $@	

$(R_HOME)/$(BINDIR)/Rcmd.exe: Rcmd.exe
	$(MKDIR) -p $(R_HOME)/$(BINDIR)
	$(CP) $^ $@

$(R_HOME)/$(BINDIR)/RSetReg.exe: RSetReg.exe
	$(MKDIR) -p $(R_HOME)/$(BINDIR)
	$(CP) $^ $@

$(R_HOME)/$(BINDIR)/R.exe: R.exe
	$(MKDIR) -p $(R_HOME)/$(BINDIR)
	$(CP) $^ $@	

$(R_HOME)/bin/R.exe: Rfe.exe
	$(MKDIR) -p $(R_HOME)/bin
	$(CP) $^ $@	

$(R_HOME)/bin/Rscript.exe: Rfe.exe
	$(MKDIR) -p $(R_HOME)/bin
	$(CP) $^ $@	

$(R_HOME)/$(BINDIR)/Rscript.exe: Rscript.exe
	$(MKDIR) -p $(R_HOME)/$(BINDIR)
	$(CP) $^ $@	

$(R_HOME)/$(BINDIR)/open.exe: open.exe
	$(MKDIR) -p $(R_HOME)/$(BINDIR)
	$(CP) $^ $@	

rgui-CPPFLAGS=-DWin32
rterm-CPPFLAGS=-DWin32

## -luser32 -lkernel32 -ladvapi32 -lshell32 are now standard
Rgui-LIBS=-L../../../$(IMPDIR) -lR -lRgraphapp
Rscript-LIBS=-L../../../$(IMPDIR) -lR -lRgraphapp
Rterm-LIBS=-L../../../$(IMPDIR) -lR -lRgraphapp

Rgui-LINKFLAGS=-mwindows $(LARGE_ADDR) $(LARGE_STACK)
Rterm-LINKFLAGS=$(LARGE_ADDR) $(LARGE_STACK)
Rscript-LINKFLAGS=$(LARGE_ADDR) $(LARGE_STACK)

Rgui.exe: graphappmain.o rgui.o rgico.o
Rterm.exe: graphappmain.o rterm.o rtico.o

ifneq "$(WIN)" "64"
rcico.o: Rcmd.exe.manifest
rgico.o: Rgui32.manifest
rtico.o: Rterm32.manifest
else
RESFLAGS = -DWN64
rtico.o: Rterm64.manifest
rgico.o: Rgui64.manifest
endif

Rcmd.exe: rcmd.o ../rhome.o ../shext.o rcico.o rcmdfn.o Renviron.o

R.exe: R.o ../rhome.o ../shext.o rcico.o rcmdfn.o Renviron.o

Rfe.exe: Rfe.o ../rhome.o rcico.o

RSetReg.exe: RSetReg.o ../rhome.o rcico.o

Rscript.o: ../../unix/Rscript.c ../../include/Rversion.h
	$(CC) $(CFLAGS) -DBINDIR='"$(BINDIR)"' -I. -I../../include -DHAVE_CONFIG_H -c $< -o $@

Rscript.exe: Rscript.o rcico.o 

## Not currently used (2.13.0)
Rpwd:
	$(MAKE) -C ../../include -f Makefile.win version
	$(MAKE) Rpwd.exe

Rpwd.exe: rpwd.o rcico.o

rcmd.o R.o rgui.o rterm.o RSetReg.o rgico.o rtico.o rcico.o: ../../include/Rversion.h

open.o: open.c
open.exe: open.o

clean:
	$(RM) *.o *.obj *.exe *~ *.exp *.dll *.lib *.a *.d Makedeps

CSOURCES=$(filter-out R.c rcmd.c rpwd.c rtest.c, $(wildcard *.c))
DEPS=$(CSOURCES:.c=.d)

makeMakedeps: $(DEPS)
	@$(RM) Makedeps
ifneq ($(strip $(DEPS)),)
	@cat $(DEPS) >> Makedeps
endif

-include Makedeps
